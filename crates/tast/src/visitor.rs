use mollie_shared::Operator;
use mollie_typing::{FieldRef, TypeInfoRef};

pub use self::default_visitors::*;
use crate::{
    BlockRef, ExprRef, StmtRef, TypedAST,
    expression::{IsPattern, LiteralExpr, VFunc},
};

pub trait Visitor {
    #[allow(unused_variables)]
    fn visit_literal(&mut self, ast: &TypedAST, literal: &LiteralExpr) {}

    fn visit_if(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef, else_block: Option<ExprRef>) {
        visit_if(self, ast, condition, block, else_block);
    }

    #[allow(unused_variables)]
    fn visit_var(&mut self, ast: &TypedAST, name: &str) {}

    fn visit_vtable_access(&mut self, ast: &TypedAST, target: ExprRef, func: VFunc) {
        visit_vtable_access(self, ast, target, func);
    }

    fn visit_access(&mut self, ast: &TypedAST, target: ExprRef, field: FieldRef) {
        visit_access(self, ast, target, field);
    }

    fn visit_index(&mut self, ast: &TypedAST, target: ExprRef, index: ExprRef) {
        visit_index(self, ast, target, index);
    }

    fn visit_while(&mut self, ast: &TypedAST, condition: ExprRef, block: BlockRef) {
        visit_while(self, ast, condition, block);
    }

    fn visit_array(&mut self, ast: &TypedAST, elements: &[ExprRef]) {
        visit_array(self, ast, elements);
    }

    fn visit_binary(&mut self, ast: &TypedAST, lhs: ExprRef, rhs: ExprRef, operator: Operator) {
        visit_binary(self, ast, lhs, rhs, operator);
    }

    fn visit_call(&mut self, ast: &TypedAST, func: ExprRef, args: &[ExprRef]) {
        visit_call(self, ast, func, args);
    }

    fn visit_closure(&mut self, ast: &TypedAST, args: &[String], body: BlockRef) {
        visit_closure(self, ast, args, body);
    }

    fn visit_construct(&mut self, ast: &TypedAST, ty: TypeInfoRef, fields: &[(FieldRef, String, Option<ExprRef>)]) {
        visit_construct(self, ast, ty, fields);
    }

    fn visit_construct_enum(&mut self, ast: &TypedAST, ty: TypeInfoRef, variant: usize, fields: Option<&[(FieldRef, String, ExprRef)]>) {
        visit_construct_enum(self, ast, ty, variant, fields);
    }

    fn visit_construct_component(&mut self, ast: &TypedAST, ty: TypeInfoRef, fields: &[(String, ExprRef)], children: &[ExprRef]) {
        visit_construct_component(self, ast, ty, fields, children);
    }

    fn visit_is_pattern(&mut self, ast: &TypedAST, target: ExprRef, pattern: &IsPattern) {
        visit_is_pattern(self, ast, target, pattern);
    }

    #[allow(unused_variables)]
    fn visit_type_index(&mut self, ast: &TypedAST, target: TypeInfoRef, func: VFunc) {}

    fn visit_expr(&mut self, ast: &TypedAST, expr: ExprRef) {
        visit_expr(self, ast, expr);
    }

    fn visit_stmt(&mut self, ast: &TypedAST, stmt: StmtRef) {
        visit_stmt(self, ast, stmt);
    }

    fn visit_block(&mut self, ast: &TypedAST, block: BlockRef) {
        visit_block(self, ast, block);
    }
}

#[allow(unused_variables)]
mod default_visitors {
    use mollie_shared::Operator;
    use mollie_typing::{FieldRef, TypeInfoRef};

    use super::Visitor;
    use crate::{
        BlockRef, ExprRef, StmtRef, TypedAST,
        expression::{Expr, IsPattern, VFunc},
        statement::Stmt,
    };

    pub fn visit_if<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, condition: ExprRef, block: BlockRef, else_block: Option<ExprRef>) {
        visitor.visit_expr(ast, condition);
        visitor.visit_block(ast, block);

        if let Some(else_block) = else_block {
            visitor.visit_expr(ast, else_block);
        }
    }

    pub fn visit_vtable_access<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, target: ExprRef, func: VFunc) {
        visitor.visit_expr(ast, target);
    }

    pub fn visit_access<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, target: ExprRef, field: FieldRef) {
        visitor.visit_expr(ast, target);
    }

    pub fn visit_index<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, target: ExprRef, index: ExprRef) {
        visitor.visit_expr(ast, target);
        visitor.visit_expr(ast, index);
    }

    pub fn visit_while<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, condition: ExprRef, block: BlockRef) {
        visitor.visit_expr(ast, condition);
        visitor.visit_block(ast, block);
    }

    pub fn visit_array<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, elements: &[ExprRef]) {
        for &element in elements {
            visitor.visit_expr(ast, element);
        }
    }

    pub fn visit_binary<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, lhs: ExprRef, rhs: ExprRef, operator: Operator) {
        visitor.visit_expr(ast, lhs);
        visitor.visit_expr(ast, rhs);
    }

    pub fn visit_call<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, func: ExprRef, args: &[ExprRef]) {
        visitor.visit_expr(ast, func);

        for &arg in args {
            visitor.visit_expr(ast, arg);
        }
    }

    pub fn visit_closure<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, args: &[String], body: BlockRef) {
        visitor.visit_block(ast, body);
    }

    pub fn visit_construct<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, ty: TypeInfoRef, fields: &[(FieldRef, String, Option<ExprRef>)]) {
        for field in fields {
            if let Some(expr) = field.2 {
                visitor.visit_expr(ast, expr);
            }
        }
    }

    pub fn visit_construct_enum<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, ty: TypeInfoRef, variant: usize, fields: Option<&[(FieldRef, String, ExprRef)]>) {
        if let Some(fields) = fields {
            for field in fields {
                visitor.visit_expr(ast, field.2);
            }
        }
    }

    pub fn visit_construct_component<T: Visitor + ?Sized>(
        visitor: &mut T,
        ast: &TypedAST,
        ty: TypeInfoRef,
        fields: &[(String, ExprRef)],
        children: &[ExprRef],
    ) {
        for field in fields {
            visitor.visit_expr(ast, field.1);
        }

        for &child in children {
            visitor.visit_expr(ast, child);
        }
    }

    pub fn visit_is_pattern<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, target: ExprRef, pattern: &IsPattern) {
        fn visit_is_pattern_inner<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, pattern: &IsPattern) {
            match pattern {
                &IsPattern::Literal(expr) => visitor.visit_expr(ast, expr),
                IsPattern::EnumVariant { target, target_args, variant, values } => {
                    if let Some(values) = values.as_deref() {
                        for value in values {
                            if let Some(value) = &value.2 {
                                visit_is_pattern_inner(visitor, ast, value);
                            }
                        }
                    }
                }
                IsPattern::TypeName { ty, name } => (),
            }
        }

        visitor.visit_expr(ast, target);

        visit_is_pattern_inner(visitor, ast, pattern);
    }

    pub fn visit_expr<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, expr: ExprRef) {
        match &ast[expr].value {
            Expr::Literal(literal) => visitor.visit_literal(ast, literal),
            &Expr::If { condition, block, else_block } => visitor.visit_if(ast, condition, block, else_block),
            &Expr::Block(block) => visitor.visit_block(ast, block),
            Expr::Var(name) => visitor.visit_var(ast, name.as_str()),
            &Expr::VTableAccess { target, func } => visitor.visit_vtable_access(ast, target, func),
            &Expr::Access { target, field } => visitor.visit_access(ast, target, field),
            &Expr::Index { target, index } => visitor.visit_index(ast, target, index),
            &Expr::While { condition, block } => visitor.visit_while(ast, condition, block),
            Expr::Array(elements) => visitor.visit_array(ast, elements.as_ref()),
            &Expr::Binary { operator, lhs, rhs } => visitor.visit_binary(ast, lhs, rhs, operator),
            Expr::Call { func, args } => visitor.visit_call(ast, *func, args.as_ref()),
            Expr::Closure { args, body } => visitor.visit_closure(ast, args.as_ref(), *body),
            Expr::Construct { ty, fields } => visitor.visit_construct(ast, *ty, fields.as_ref()),
            Expr::ConstructEnum { ty, variant, fields } => visitor.visit_construct_enum(ast, *ty, *variant, fields.as_deref()),
            Expr::ConstructComponent { ty, fields, children } => visitor.visit_construct_component(ast, *ty, fields.as_ref(), children.as_ref()),
            Expr::IsPattern { target, pattern } => visitor.visit_is_pattern(ast, *target, pattern),
            &Expr::TypeIndex { target, func } => visitor.visit_type_index(ast, target, func),
            Expr::Nothing => (),
        }
    }

    pub fn visit_stmt<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, stmt: StmtRef) {
        match &ast[stmt] {
            &Stmt::Expr(expr) => visitor.visit_expr(ast, expr),
            Stmt::VariableDecl { value, .. } => visitor.visit_expr(ast, *value),
            _ => todo!()
        }
    }

    pub fn visit_block<T: Visitor + ?Sized>(visitor: &mut T, ast: &TypedAST, block: BlockRef) {
        let block = &ast[block].value;

        for &stmt in &block.stmts {
            visitor.visit_stmt(ast, stmt);
        }

        if let Some(expr) = block.expr {
            visitor.visit_expr(ast, expr);
        }
    }
}
