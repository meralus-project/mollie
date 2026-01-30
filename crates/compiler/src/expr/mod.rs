use std::hash::{DefaultHasher, Hash, Hasher};

pub use cranelift;
use cranelift::{
    codegen::ir,
    module::Module,
    prelude::{FloatCC, FunctionBuilderContext, InstBuilder, IntCC},
};
use indexmap::map::Entry;
pub use itertools;
use itertools::Itertools;
use mollie_index::Idx;
use mollie_ir::{Array, Field, MollieType, VTablePtr, compile_constant};
use mollie_shared::Operator;
use mollie_typed_ast::{BlockRef, Expr, ExprRef, IsPattern, LiteralExpr, TypePath, TypedAST, VFunc};
use mollie_typing::{AdtKind, AdtVariantRef, FieldRef, PrimitiveType, TypeInfo, TypeInfoRef};

use crate::{
    AsIrType, CompileTypedAST, MolValue, Var,
    error::CompileResult,
    func::{FuncKey, FunctionCompiler, Variable},
};

impl<M: Module> FunctionCompiler<'_, M> {
    pub fn compile_lit_expr(&mut self, ast: &TypedAST, expr: ExprRef, lit_expr: &LiteralExpr) -> CompileResult<MolValue> {
        Ok(MolValue::Value(match lit_expr {
            &LiteralExpr::Integer(value) => match ast[expr].ty.as_ir_type(&self.checker.solver, self.compiler.isa()) {
                MollieType::Regular(ty) => self.fn_builder.ins().iconst(ty, value),
                MollieType::Fat(..) => return Ok(MolValue::Nothing),
            },
            &LiteralExpr::Float(value) => self.fn_builder.ins().f32const(value),
            LiteralExpr::String(value) => {
                let data_id = self.compiler.codegen.static_data(value.as_bytes())?;
                let data_id = self.compiler.codegen.module.declare_data_in_func(data_id, self.fn_builder.func);
                let ptr_type = self.compiler.ptr_type();
                let ptr = self.fn_builder.ins().global_value(ptr_type, data_id);
                let size = self.fn_builder.ins().iconst(ptr_type, value.len().cast_signed() as i64);

                return Ok(MolValue::FatPtr(ptr, size));
            }
            &LiteralExpr::Boolean(value) => self.fn_builder.ins().iconst(ir::types::I8, i64::from(value)),
        }))
    }

    pub fn compile_if_expr(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef, else_block: Option<ExprRef>) -> CompileResult<MolValue> {
        self.push_frame();

        let cond_result = condition.compile(ast, self)?.expect_value();
        let then_block = self.fn_builder.create_block();
        let after_block = self.fn_builder.create_block();

        let returning_param = if let Some(final_stmt) = &ast[block].value.expr {
            Some(match ast[*final_stmt].ty.as_ir_type(&self.checker.solver, self.compiler.isa()) {
                MollieType::Fat(ty, metadata_ty) => MolValue::FatPtr(
                    self.fn_builder.append_block_param(after_block, ty),
                    self.fn_builder.append_block_param(after_block, metadata_ty),
                ),
                MollieType::Regular(ty) => MolValue::Value(self.fn_builder.append_block_param(after_block, ty)),
            })
        } else {
            None
        };

        let returned = if let Some(otherwise) = else_block {
            let else_block = self.fn_builder.create_block();

            self.fn_builder.ins().brif(cond_result, then_block, &[], else_block, &[]);

            self.fn_builder.switch_to_block(then_block);
            self.fn_builder.seal_block(then_block);

            let returned = block.compile(ast, self)?;

            match returned {
                MolValue::Value(value) => self.fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value)]),
                MolValue::Values(values) => self
                    .fn_builder
                    .ins()
                    .jump(after_block, values.into_iter().map(ir::BlockArg::Value).collect::<Box<[_]>>().as_ref()),
                MolValue::FuncRef(_) => todo!(),
                MolValue::FatPtr(value, metadata) => self
                    .fn_builder
                    .ins()
                    .jump(after_block, &[ir::BlockArg::Value(value), ir::BlockArg::Value(metadata)]),
                MolValue::Nothing => self.fn_builder.ins().jump(after_block, &[]),
            };

            self.pop_frame();

            self.fn_builder.switch_to_block(else_block);
            self.fn_builder.seal_block(else_block);

            otherwise.compile(ast, self)?
        } else {
            self.fn_builder.ins().brif(cond_result, then_block, &[], after_block, &[]);

            self.fn_builder.switch_to_block(then_block);
            self.fn_builder.seal_block(then_block);

            let returned = block.compile(ast, self)?;

            self.pop_frame();

            returned
        };

        match returned {
            MolValue::Value(value) => self.fn_builder.ins().jump(after_block, &[ir::BlockArg::Value(value)]),
            MolValue::Values(values) => self
                .fn_builder
                .ins()
                .jump(after_block, values.into_iter().map(ir::BlockArg::Value).collect::<Box<[_]>>().as_ref()),
            MolValue::FuncRef(_) => todo!(),
            MolValue::FatPtr(value, metadata) => self
                .fn_builder
                .ins()
                .jump(after_block, &[ir::BlockArg::Value(value), ir::BlockArg::Value(metadata)]),
            MolValue::Nothing => self.fn_builder.ins().jump(after_block, &[]),
        };

        self.fn_builder.switch_to_block(after_block);
        self.fn_builder.seal_block(after_block);

        returning_param.map_or(Ok(MolValue::Nothing), Ok)
    }

    pub fn compile_var_expr(&mut self, ast: &TypedAST, expr: ExprRef, name: &str) -> CompileResult<MolValue> {
        if let Some((_, value_ref)) = self.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == expr) {
            let value = value_ref.compile(ast, self)?;

            match (self.get_var(name), value) {
                (Some(Var::Regular(v)), MolValue::Value(value)) => {
                    self.fn_builder.def_var(v, value);

                    Ok(MolValue::Nothing)
                }
                (Some(Var::Fat(v, m)), MolValue::FatPtr(value, metadata)) => {
                    self.fn_builder.def_var(v, value);
                    self.fn_builder.def_var(m, metadata);

                    Ok(MolValue::Nothing)
                }
                _ => unimplemented!(),
            }
        } else if let Some(v) = self.get_var(name) {
            match v {
                Var::Regular(v) => Ok(MolValue::Value(self.fn_builder.use_var(v))),
                Var::Fat(v, m) => Ok(MolValue::FatPtr(self.fn_builder.use_var(v), self.fn_builder.use_var(m))),
            }
        } else if let Some(&func) = self.compiler.name_to_func_id.get(name) {
            Ok(MolValue::FuncRef(match self.funcs.entry(FuncKey::Id(func)) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let func = self.compiler.codegen.module.declare_func_in_func(func, self.fn_builder.func);

                    *entry.insert(func)
                }
            }))
        } else {
            unimplemented!("there's no anything for {name}")
        }
    }

    pub fn compile_field_access_expr(&mut self, ast: &TypedAST, expr: ExprRef, target: ExprRef, field: FieldRef) -> CompileResult<MolValue> {
        let v = target.compile(ast, self)?;

        if let MolValue::Value(v) = v {
            if let TypeInfo::Adt(..) = self.checker.solver.get_info2(ast[target].ty) {
                let hash = self.checker.solver.hash_of(ast[target].ty);
                let assign_value = match self.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == expr) {
                    Some((_, assign)) => Some(assign.compile(ast, self)?),
                    None => None,
                };

                let (field, _) = &self.compiler.get_adt_variant(hash, AdtVariantRef::ZERO).fields[field];

                Ok(match (field.ty, assign_value) {
                    (MollieType::Regular(ty), None) => MolValue::Value(self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), v, field.offset)),
                    (MollieType::Fat(ty, metadata_ty), None) => MolValue::FatPtr(
                        self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), v, field.offset),
                        self.fn_builder
                            .ins()
                            .load(metadata_ty, ir::MemFlags::trusted(), v, field.offset + ty.bytes().cast_signed()),
                    ),
                    (MollieType::Regular(_), Some(MolValue::Value(value))) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, v, field.offset);

                        MolValue::Nothing
                    }
                    (MollieType::Fat(ty, _), Some(MolValue::FatPtr(value, metadata))) => {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, v, field.offset);
                        self.fn_builder
                            .ins()
                            .store(ir::MemFlags::trusted(), metadata, v, field.offset + ty.bytes().cast_signed());

                        MolValue::Nothing
                    }
                    _ => unimplemented!(),
                })
            } else {
                unimplemented!()
            }
        } else {
            unimplemented!()
        }
    }

    pub fn compile_vtable_access(&mut self, ast: &TypedAST, target: ExprRef, func: VFunc) -> CompileResult<MolValue> {
        let target_val = target.compile(ast, self)?;

        match func {
            VFunc::Known(vtable_ref, func) => {
                self.this.replace(target_val);

                let target_hash = self.hash_of(ast[target].ty);

                Ok(MolValue::FuncRef(self.get_vfunc(target_hash, vtable_ref, func)))
            }
            VFunc::Unknown(_, trait_func_ref) => {
                if let MolValue::FatPtr(value, vtable_ptr) = target_val {
                    let vtable_func = VTablePtr::get_func_ptr(
                        self.compiler.isa(),
                        &mut self.fn_builder,
                        vtable_ptr,
                        trait_func_ref.index().try_into().unwrap(),
                    );

                    self.this.replace(MolValue::Value(value));

                    Ok(MolValue::Value(vtable_func))
                } else {
                    panic!("expected fat ptr for accessing dynamic vtable value")
                }
            }
        }
    }

    pub fn compile_array_index(&mut self, ast: &TypedAST, expr: ExprRef, target: ExprRef, index: ExprRef) -> CompileResult<MolValue> {
        let target = target.compile(ast, self)?;
        let index = index.compile(ast, self)?;

        if let (&MolValue::FatPtr(ptr, _), &MolValue::Value(index)) = (&target, &index) {
            let element_type = ast[expr].ty.as_ir_type(&self.checker.solver, self.compiler.isa());
            let size = match element_type {
                MollieType::Fat(ty, metadata_ty) => ty.bytes() + metadata_ty.bytes(),
                MollieType::Regular(ty) => ty.bytes(),
            };

            let size = self.fn_builder.ins().iconst(self.compiler.ptr_type(), i64::from(size));
            let offset = self.fn_builder.ins().imul(size, index);
            let ptr = self.fn_builder.ins().iadd(ptr, offset);

            if let Some((_, assign)) = self.assign_ref.take_if(|(lhs_ref, _)| *lhs_ref == expr) {
                if let MolValue::Value(value) = assign.compile(ast, self)? {
                    self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, 0);

                    return Ok(MolValue::Nothing);
                }
            } else {
                return match element_type {
                    MollieType::Fat(ty, metadata_ty) => Ok(MolValue::FatPtr(
                        self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0),
                        self.fn_builder.ins().load(metadata_ty, ir::MemFlags::trusted(), ptr, ty.bytes().cast_signed()),
                    )),
                    MollieType::Regular(ty) => Ok(MolValue::Value(self.fn_builder.ins().load(ty, ir::MemFlags::trusted(), ptr, 0))),
                };
            }
        }

        todo!("no target[index] for {target:?}[{index:?}]")
    }

    pub fn compile_while_expr(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef) -> CompileResult<MolValue> {
        self.push_frame();

        let condition_block = self.fn_builder.create_block();
        let inner_block = self.fn_builder.create_block();
        let after_block = self.fn_builder.create_block();

        self.fn_builder.ins().jump(condition_block, &[]);
        self.fn_builder.switch_to_block(condition_block);

        if let MolValue::Value(value) = condition.compile(ast, self)? {
            self.fn_builder.ins().brif(value, inner_block, &[], after_block, &[]);
        }

        self.fn_builder.switch_to_block(inner_block);
        self.fn_builder.seal_block(inner_block);

        let value = block.compile(ast, self)?;

        self.unmark_variables();
        self.pop_frame();

        self.fn_builder.ins().jump(condition_block, &[]);
        self.fn_builder.switch_to_block(after_block);
        self.fn_builder.seal_block(condition_block);
        self.fn_builder.seal_block(after_block);

        Ok(value)
    }

    pub fn compile_array_expr(&mut self, ast: &TypedAST, expr: ExprRef, elements: &[ExprRef]) -> CompileResult<MolValue> {
        if let &TypeInfo::Array(element, _) = self.checker.solver.get_info(ast[expr].ty) {
            let mut values = Vec::with_capacity(elements.len());

            for expr_ref in elements {
                if let MolValue::Value(value) = expr_ref.compile(ast, self)? {
                    if self.checker.solver.get_info(element).is_any_component() {
                        let hash = self.checker.solver.hash_of(ast[*expr_ref].ty);
                        let metadata = self.fn_builder.ins().iconst(self.compiler.ptr_type(), hash.cast_signed());

                        values.push(value);
                        values.push(metadata);
                    } else if let &TypeInfo::Trait(t, _) = self.checker.solver.get_info(element) {
                        let hash = self.checker.solver.hash_of(ast[*expr_ref].ty);
                        let data_id = self
                            .compiler
                            .codegen
                            .module
                            .declare_data_in_func(self.compiler.trait_to_vtable[&(hash, Some(t))], self.fn_builder.func);

                        let metadata = self.fn_builder.ins().global_value(self.compiler.ptr_type(), data_id);

                        values.push(value);
                        values.push(metadata);
                    } else {
                        values.push(value);
                    }
                }
            }

            let size = self.fn_builder.ins().iconst(self.compiler.ptr_type(), elements.len() as i64);

            let ir_element = ast[expr].ty.as_ir_type(&self.checker.solver, self.compiler.isa());

            let arr = Array { element: ir_element };

            self.compiler.codegen.data_desc.define_zeroinit(arr.get_size(values.len()) as usize);

            let id = self.compiler.codegen.module.declare_anonymous_data(true, false).unwrap();

            self.compiler.codegen.module.define_data(id, &self.compiler.codegen.data_desc).unwrap();
            self.compiler.codegen.data_desc.clear();

            let data_id = self.compiler.codegen.module.declare_data_in_func(id, self.fn_builder.func);
            let ptr = self.fn_builder.ins().global_value(self.compiler.ptr_type(), data_id);

            match ir_element {
                MollieType::Regular(_) => {
                    for (index, value) in values.into_iter().enumerate() {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, arr.get_offset_of(index));
                    }
                }
                MollieType::Fat(ty, _) => {
                    for (index, (value, metadata)) in values.into_iter().tuples::<(ir::Value, ir::Value)>().enumerate() {
                        self.fn_builder.ins().store(ir::MemFlags::trusted(), value, ptr, arr.get_offset_of(index));
                        self.fn_builder
                            .ins()
                            .store(ir::MemFlags::trusted(), metadata, ptr, arr.get_offset_of(index) + ty.bytes().cast_signed());
                    }
                }
            }

            Ok(MolValue::FatPtr(ptr, size))
        } else {
            panic!("expected array")
        }
    }

    pub fn compile_bin_expr(&mut self, ast: &TypedAST, lhs_ref: ExprRef, operator: Operator, rhs_ref: ExprRef) -> CompileResult<MolValue> {
        if matches!(operator, Operator::Assign) {
            let old = self.assign_ref.replace((lhs_ref, rhs_ref));
            let lhs = lhs_ref.compile(ast, self)?;

            if old.is_some() {
                self.assign_ref = old;
            } else if self.assign_ref.is_some() {
                self.assign_ref.take();
            }

            Ok(lhs)
        } else {
            let lhs = lhs_ref.compile(ast, self)?;
            let rhs = rhs_ref.compile(ast, self)?;

            let lhs_ty = self.checker.solver.get_info2(ast[lhs_ref].ty);
            let rhs_ty = self.checker.solver.get_info2(ast[rhs_ref].ty);

            if let (&MolValue::Value(lhs), &MolValue::Value(rhs)) = (&lhs, &rhs) {
                if lhs_ty.is_unsigned_integer() && rhs_ty.is_unsigned_integer() {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().uadd_overflow(lhs, rhs).0,
                        Operator::Sub => self.fn_builder.ins().usub_overflow(lhs, rhs).0,
                        Operator::Mul => self.fn_builder.ins().umul_overflow(lhs, rhs).0,
                        Operator::Div => self.fn_builder.ins().udiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else if lhs_ty.is_signed_integer() && rhs_ty.is_signed_integer() {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().iadd(lhs, rhs),
                        Operator::Sub => self.fn_builder.ins().isub(lhs, rhs),
                        Operator::Mul => self.fn_builder.ins().imul(lhs, rhs),
                        Operator::Div => self.fn_builder.ins().sdiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else if matches!(lhs_ty, TypeInfo::Primitive(PrimitiveType::Float)) && matches!(rhs_ty, TypeInfo::Primitive(PrimitiveType::Float)) {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().fadd(lhs, rhs),
                        Operator::Sub => self.fn_builder.ins().fsub(lhs, rhs),
                        Operator::Mul => self.fn_builder.ins().fmul(lhs, rhs),
                        Operator::Div => self.fn_builder.ins().fdiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                } else {
                    Ok(MolValue::Value(match operator {
                        Operator::Add => self.fn_builder.ins().iadd(lhs, rhs),
                        Operator::Sub => self.fn_builder.ins().isub(lhs, rhs),
                        Operator::Mul => self.fn_builder.ins().imul(lhs, rhs),
                        Operator::Div => self.fn_builder.ins().udiv(lhs, rhs),
                        Operator::Equal => self.fn_builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        Operator::NotEqual => self.fn_builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        Operator::LessThan => self.fn_builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        Operator::GreaterThan => self.fn_builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    }))
                }
            } else {
                unimplemented!("{lhs:?} {operator} {rhs:?}")
            }
        }
    }

    pub fn compile_call_expr(&mut self, ast: &TypedAST, func: ExprRef, args: &[ExprRef]) -> CompileResult<MolValue> {
        if let Some(TypeInfo::Func(arg_types, returns)) = self.checker.solver.get_maybe_info(ast[func].ty) {
            let v = func.compile(ast, self)?;
            let mut arg_values = Vec::new();

            if let Some(this) = self.this.take() {
                match this {
                    MolValue::Value(value) => arg_values.push(value),
                    MolValue::FatPtr(value, metadata) => {
                        arg_values.push(value);
                        arg_values.push(metadata);
                    }
                    MolValue::Nothing => (),
                    _ => panic!("received incorrect value for <self> argument: {this:?}"),
                }
            }

            for arg in args {
                let value = arg.compile(ast, self)?;

                match value {
                    MolValue::Value(value) => arg_values.push(value),
                    MolValue::FatPtr(value, metadata) => {
                        arg_values.push(value);
                        arg_values.push(metadata);
                    }
                    MolValue::FuncRef(func) => {
                        let func = self.fn_builder.ins().func_addr(self.compiler.ptr_type(), func);

                        arg_values.push(func);
                    }
                    MolValue::Nothing => (),
                    _ => panic!("received incorrect value for argument: {arg:?} {value:?}"),
                }
            }

            let value = if let MolValue::FuncRef(func) = v {
                let result = self.fn_builder.ins().call(func, &arg_values);

                match self.fn_builder.inst_results(result) {
                    [] => MolValue::Nothing,
                    &[value] => MolValue::Value(value),
                    &[value, metadata]
                        if returns != &self.checker.core_types.void && returns.as_ir_type(&self.checker.solver, self.compiler.isa()).is_fat() =>
                    {
                        MolValue::FatPtr(value, metadata)
                    }
                    values => MolValue::Values(values.to_vec()),
                }
            } else if let MolValue::Value(func_addr) = v {
                let mut signature = self.compiler.codegen.module.make_signature();

                for arg in arg_types {
                    if arg.as_inner() != &self.checker.core_types.void {
                        if arg.is_this() && self.checker.solver.get_info(arg.inner()).is_trait() {
                            signature.params.push(ir::AbiParam::new(self.compiler.ptr_type()));
                        } else {
                            arg.as_inner()
                                .as_ir_type(&self.checker.solver, self.compiler.isa())
                                .add_to_params(&mut signature.params);
                        }
                    }
                }

                if returns != &self.checker.core_types.void {
                    returns
                        .as_ir_type(&self.checker.solver, self.compiler.isa())
                        .add_to_params(&mut signature.returns);
                }

                let sig = self.fn_builder.import_signature(signature);
                let result = self.fn_builder.ins().call_indirect(sig, func_addr, &arg_values);

                match self.fn_builder.inst_results(result) {
                    [] => MolValue::Nothing,
                    &[value] => MolValue::Value(value),
                    &[value, metadata]
                        if returns != &self.checker.core_types.void && returns.as_ir_type(&self.checker.solver, self.compiler.isa()).is_fat() =>
                    {
                        MolValue::FatPtr(value, metadata)
                    }
                    values => MolValue::Values(values.to_vec()),
                }
            } else {
                MolValue::Nothing
            };

            Ok(value)
        } else {
            panic!("dada");
        }
    }

    pub fn compile_closure_expr(&mut self, ast: &TypedAST, expr: ExprRef, args: &[String], body: BlockRef) -> CompileResult<MolValue> {
        let mut signature = self.compiler.codegen.module.make_signature();

        if let TypeInfo::Func(args, returns) = self.checker.solver.get_info(ast[expr].ty) {
            for arg in args {
                if arg.as_inner() != &self.checker.core_types.void {
                    arg.as_inner()
                        .as_ir_type(&self.checker.solver, self.compiler.isa())
                        .add_to_params(&mut signature.params);
                }
            }

            if returns != &self.checker.core_types.void {
                returns
                    .as_ir_type(&self.checker.solver, self.compiler.isa())
                    .add_to_params(&mut signature.returns);
            }
        }

        let mut ctx = self.compiler.codegen.module.make_context();
        let mut fn_builder_ctx = FunctionBuilderContext::new();

        let mut compiler = FunctionCompiler::new_anonymous(signature, self.compiler, self.checker, &mut ctx, &mut fn_builder_ctx).unwrap();

        let mut index = 0;

        for (arg, name) in args.iter().enumerate() {
            let value = compiler.fn_builder.block_params(compiler.entry_block)[index];
            let ty = compiler.fn_builder.func.signature.params[index].value_type;

            let var = compiler.fn_builder.declare_var(ty);

            compiler.fn_builder.def_var(var, value);

            if let TypeInfo::Func(args, _) = compiler.checker.solver.get_info(ast[expr].ty) {
                if args[arg].as_inner().as_ir_type(&compiler.checker.solver, compiler.compiler.isa()).is_fat() {
                    let value = compiler.fn_builder.block_params(compiler.entry_block)[index + 1];
                    let ty = compiler.fn_builder.func.signature.params[index + 1].value_type;

                    let metadata_var = compiler.fn_builder.declare_var(ty);

                    compiler.fn_builder.def_var(metadata_var, value);
                    compiler
                        .current_frame_mut()
                        .insert(name.clone(), Variable::new(Var::Fat(var, metadata_var), args[arg].inner()));

                    index += 2;
                } else {
                    compiler
                        .current_frame_mut()
                        .insert(name.clone(), Variable::new(Var::Regular(var), args[arg].inner()));

                    index += 1;
                }
            }
        }

        let returned = body.compile(ast, &mut compiler)?;

        compiler.return_(returned);

        let func_id = compiler.id;

        self.compiler.codegen.module.define_function(func_id, &mut ctx).unwrap();
        self.compiler.codegen.module.clear_context(&mut ctx);

        let func = self.compiler.codegen.module.declare_func_in_func(func_id, self.fn_builder.func);

        Ok(MolValue::FuncRef(func))
    }

    pub fn compile_construct(
        &mut self,
        ast: &TypedAST,
        ty: TypeInfoRef,
        variant: AdtVariantRef,
        fields: &[(FieldRef, String, Option<ExprRef>)],
    ) -> CompileResult<MolValue> {
        let mut values = Vec::new();
        let hash = self.hash_of(ty);
        let &TypeInfo::Adt(adt_ref, adt_kind, ..) = self.checker.solver.get_info(ty) else {
            unreachable!()
        };

        let is_enum = matches!(adt_kind, AdtKind::Enum);

        if is_enum {
            values.push(self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index() as i64));
        }

        for (field_ref, (field, _)) in self.compiler.get_adt_variant(hash, variant).fields.clone().into_iter() {
            if let Some(property) = fields.iter().find(|property| property.0 == field_ref) {
                if is_enum && field_ref.index() == 0 {
                    continue;
                }

                let (got_ty, got) = match property.2 {
                    Some(expr) => (Some(ast[expr].ty), expr.compile(ast, self)?),
                    None => match &field.default_value {
                        Some(value) => (
                            None,
                            compile_constant(
                                value,
                                &mut self.compiler.codegen.module,
                                &mut self.compiler.codegen.data_desc,
                                &mut self.fn_builder,
                            )
                            .map_or(MolValue::Nothing, MolValue::Value),
                        ),
                        None => panic!(
                            "can't compile {}: {field:?} => no value",
                            self.checker.adt_types[adt_ref].variants[variant].fields[field_ref].0
                        ),
                    },
                };

                match (field.ty, &got) {
                    (MollieType::Regular(ty), &MolValue::Value(v)) => {
                        assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                        values.push(v);
                    }
                    (MollieType::Fat(ty, meta), got) => match *got {
                        MolValue::Value(v) => {
                            if let Some(got_ty) = got_ty {
                                assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");

                                let hash = self.checker.solver.hash_of(got_ty);
                                let metadata = self.fn_builder.ins().iconst(meta, hash.cast_signed());

                                values.push(v);
                                values.push(metadata);
                            }
                        }
                        MolValue::FatPtr(v, m) => {
                            assert_eq!(ty, self.fn_builder.func.dfg.value_type(v), "got incorrect type for ...");
                            assert_eq!(meta, self.fn_builder.func.dfg.value_type(m), "got incorrect type for ...");

                            values.push(v);
                            values.push(m);
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }
        }

        Ok(MolValue::Value(self.construct(ty, variant, &values)?))
    }

    pub fn compile_is_pattern_expr(&mut self, ast: &TypedAST, target: ExprRef, pattern: &IsPattern) -> CompileResult<MolValue> {
        fn compile_pattern<M: Module>(
            target: &MolValue,
            target_ty: TypeInfoRef,
            pattern: &IsPattern,
            ast: &TypedAST,
            compiler: &mut FunctionCompiler<'_, M>,
        ) -> CompileResult<MolValue> {
            match pattern {
                &IsPattern::Literal(expr_ref) => {
                    if let &MolValue::Value(target) = target {
                        let target_ty = compiler.checker.solver.get_info(target_ty);
                        let expr_ty = compiler.checker.solver.get_info(ast[expr_ref].ty);
                        let MolValue::Value(value) = expr_ref.compile(ast, compiler)? else {
                            unreachable!()
                        };

                        Ok(MolValue::Value(if target_ty.is_float() && expr_ty.is_float() {
                            compiler.fn_builder.ins().fcmp(FloatCC::Equal, target, value)
                        } else {
                            compiler.fn_builder.ins().icmp(IntCC::Equal, target, value)
                        }))
                    } else {
                        Ok(MolValue::Nothing)
                    }
                }
                IsPattern::EnumVariant {
                    target: target_ty,
                    target_args,
                    variant,
                    values,
                } => {
                    if let &MolValue::Value(target) = target {
                        let expected = compiler.fn_builder.ins().iconst(compiler.compiler.ptr_type(), variant.index() as i64);
                        let metadata = compiler.fn_builder.ins().load(compiler.compiler.ptr_type(), ir::MemFlags::trusted(), target, 0);
                        let result = compiler.fn_builder.ins().icmp(IntCC::Equal, metadata, expected);

                        let hash = {
                            let mut state = DefaultHasher::new();

                            "adt".hash(&mut state);

                            target_ty.hash(&mut state);
                            compiler.checker.adt_types[*target_ty].kind.hash(&mut state);

                            for &arg in target_args {
                                compiler.checker.solver.hash_into(&mut state, arg);
                            }

                            state.finish()
                        };

                        let declaration_block = compiler.fn_builder.create_block();
                        let otherwise_block = compiler.fn_builder.create_block();
                        let after_block = compiler.fn_builder.create_block();

                        compiler.fn_builder.ins().brif(result, declaration_block, &[], otherwise_block, &[]);

                        compiler.fn_builder.switch_to_block(declaration_block);
                        compiler.fn_builder.seal_block(declaration_block);

                        for (field_ref, name, pattern) in values {
                            let &(Field { ty, offset, .. }, field_ty) = &compiler.compiler.get_adt_variant(hash, *variant).fields[*field_ref];

                            match ty {
                                MollieType::Regular(ty) => {
                                    let value = compiler.fn_builder.ins().load(ty, ir::MemFlags::trusted(), target, offset);

                                    if let Some(pattern) = pattern {
                                        compile_pattern(&MolValue::Value(value), field_ty, pattern, ast, compiler)?;
                                    } else {
                                        let var = compiler.fn_builder.declare_var(ty);

                                        compiler.fn_builder.def_var(var, value);
                                        compiler.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), field_ty));
                                    }
                                }
                                MollieType::Fat(ty, metadata_ty) => {
                                    let value = compiler.fn_builder.ins().load(ty, ir::MemFlags::trusted(), target, offset);
                                    let metadata =
                                        compiler
                                            .fn_builder
                                            .ins()
                                            .load(metadata_ty, ir::MemFlags::trusted(), target, offset + ty.bytes().cast_signed());

                                    if let Some(pattern) = pattern {
                                        compile_pattern(&MolValue::FatPtr(value, metadata), field_ty, pattern, ast, compiler)?;
                                    } else {
                                        let var = compiler.fn_builder.declare_var(ty);
                                        let metadata_var = compiler.fn_builder.declare_var(metadata_ty);

                                        compiler.fn_builder.def_var(var, value);
                                        compiler.fn_builder.def_var(metadata_var, metadata);
                                        compiler
                                            .current_frame_mut()
                                            .insert(name.into(), Variable::new(Var::Fat(var, metadata_var), field_ty));
                                    }
                                }
                            }
                        }

                        compiler.fn_builder.ins().jump(after_block, &[]);
                        compiler.fn_builder.switch_to_block(otherwise_block);
                        compiler.fn_builder.seal_block(otherwise_block);
                        compiler.fn_builder.ins().jump(after_block, &[]);
                        compiler.fn_builder.switch_to_block(after_block);
                        compiler.fn_builder.seal_block(after_block);

                        Ok(MolValue::Value(result))
                    } else {
                        Ok(MolValue::Nothing)
                    }
                }
                IsPattern::TypeName { ty, name } => {
                    if let &MolValue::FatPtr(ptr, metadata) = target {
                        let ptr_type = compiler.compiler.ptr_type();
                        let expected_hash = compiler.hash_of(*ty);

                        let value = compiler.fn_builder.ins().iconst(ptr_type, expected_hash.cast_signed());

                        if compiler.checker.solver.get_info(target_ty).is_trait() {
                            let metadata = compiler.fn_builder.ins().load(ptr_type, ir::MemFlags::trusted(), metadata, 0);
                            let result = compiler.fn_builder.ins().icmp(IntCC::Equal, metadata, value);

                            let declaration_block = compiler.fn_builder.create_block();
                            let after_block = compiler.fn_builder.create_block();

                            compiler.fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);

                            compiler.fn_builder.switch_to_block(declaration_block);
                            compiler.fn_builder.seal_block(declaration_block);

                            let var = compiler.fn_builder.declare_var(ptr_type);

                            compiler.fn_builder.def_var(var, ptr);
                            compiler.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), *ty));
                            compiler.fn_builder.ins().jump(after_block, &[]);
                            compiler.fn_builder.switch_to_block(after_block);
                            compiler.fn_builder.seal_block(after_block);

                            Ok(MolValue::Value(result))
                        } else {
                            let result = compiler.fn_builder.ins().icmp(IntCC::Equal, metadata, value);

                            let declaration_block = compiler.fn_builder.create_block();
                            let after_block = compiler.fn_builder.create_block();

                            compiler.fn_builder.ins().brif(result, declaration_block, &[], after_block, &[]);
                            compiler.fn_builder.switch_to_block(declaration_block);
                            compiler.fn_builder.seal_block(declaration_block);

                            let var = compiler.fn_builder.declare_var(ptr_type);

                            compiler.fn_builder.def_var(var, ptr);
                            compiler.current_frame_mut().insert(name.into(), Variable::new(Var::Regular(var), *ty));
                            compiler.fn_builder.ins().jump(after_block, &[]);
                            compiler.fn_builder.switch_to_block(after_block);
                            compiler.fn_builder.seal_block(after_block);

                            Ok(MolValue::Value(result))
                        }
                    } else {
                        Ok(MolValue::Nothing)
                    }
                }
            }
        }

        let target_ty = ast[target].ty;
        let target = target.compile(ast, self)?;

        compile_pattern(&target, target_ty, pattern, ast, self)
    }

    pub fn compile_type_index_expr(&mut self, _: &TypedAST, ty: TypeInfoRef, path: TypePath) -> CompileResult<MolValue> {
        match path {
            TypePath::Adt(.., Some((vtable_ref, vfunc_ref))) => Ok(MolValue::FuncRef(self.get_vfunc(self.checker.solver.hash_of(ty), vtable_ref, vfunc_ref))),
            TypePath::Adt(.., Some(variant), None) => {
                if self.checker.solver.get_info2(ty).is_enum() {
                    let discriminant = self.fn_builder.ins().iconst(self.compiler.ptr_type(), variant.index() as i64);

                    Ok(MolValue::Value(self.construct(ty, variant, &[discriminant])?))
                } else {
                    Ok(MolValue::Nothing)
                }
            }
            TypePath::Func(func_ref) => Ok(MolValue::FuncRef(match self.funcs.entry(FuncKey::Ref(func_ref)) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let func = self
                        .compiler
                        .codegen
                        .module
                        .declare_func_in_func(self.compiler.func_ref_to_func_id[&func_ref], self.fn_builder.func);

                    *entry.insert(func)
                }
            })),
            path => unimplemented!("{path:?}"),
        }
    }
}

impl<M: Module> CompileTypedAST<M, MolValue> for ExprRef {
    fn compile(self, ast: &TypedAST, compiler: &mut FunctionCompiler<'_, M>) -> CompileResult<MolValue> {
        match &ast[self].value {
            Expr::Literal(literal_expr) => compiler.compile_lit_expr(ast, self, literal_expr),
            &Expr::If { condition, block, else_block } => compiler.compile_if_expr(ast, condition, block, else_block),
            Expr::Block(block_ref) => block_ref.compile(ast, compiler),
            Expr::Var(name) => compiler.compile_var_expr(ast, self, name.as_str()),
            &Expr::Access { target, field } => compiler.compile_field_access_expr(ast, self, target, field),
            &Expr::VTableAccess { target, func } => compiler.compile_vtable_access(ast, target, func),
            &Expr::Index { target, index } => compiler.compile_array_index(ast, self, target, index),
            &Expr::While { condition, block } => compiler.compile_while_expr(ast, condition, block),
            Expr::Array(elements) => compiler.compile_array_expr(ast, self, elements.as_ref()),
            &Expr::Binary { operator, lhs, rhs } => compiler.compile_bin_expr(ast, lhs, operator, rhs),
            Expr::Call { func, args } => compiler.compile_call_expr(ast, *func, args.as_ref()),
            Expr::Closure { args, body } => compiler.compile_closure_expr(ast, self, args.as_ref(), *body),
            Expr::Construct { ty, variant, fields } => compiler.compile_construct(ast, *ty, *variant, fields.as_ref()),
            Expr::IsPattern { target, pattern } => compiler.compile_is_pattern_expr(ast, *target, pattern),
            &Expr::TypeIndex { ty, path } => compiler.compile_type_index_expr(ast, ty, path),
            Expr::Nothing => Ok(MolValue::Nothing),
        }
    }
}
