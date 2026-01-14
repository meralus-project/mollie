use std::{
    collections::HashMap,
    fmt::{self, Write as _},
};

use cranelift::codegen::{
    entity::SecondaryMap,
    ir::{self, entities::AnyEntity},
    write::{FuncWriter, PlainWriter, write_operands},
};

#[derive(Clone, Debug)]
pub struct CommentWriter {
    enabled: bool,
    global_comments: Vec<String>,
    entity_comments: HashMap<AnyEntity, String>,
    inst_post_comments: HashMap<ir::Inst, String>,
}

impl CommentWriter {
    pub(crate) fn new() -> Self {
        CommentWriter {
            enabled: true,
            global_comments: vec![],
            entity_comments: HashMap::default(),
            inst_post_comments: HashMap::default(),
        }
    }
}

impl CommentWriter {
    pub(crate) fn enabled(&self) -> bool {
        self.enabled
    }

    pub(crate) fn add_global_comment<S: Into<String>>(&mut self, comment: S) {
        debug_assert!(self.enabled);
        self.global_comments.push(comment.into());
    }

    pub(crate) fn add_comment<S: Into<String> + AsRef<str>, E: Into<AnyEntity>>(&mut self, entity: E, comment: S) {
        debug_assert!(self.enabled);

        use std::collections::hash_map::Entry;
        match self.entity_comments.entry(entity.into()) {
            Entry::Occupied(mut occ) => {
                occ.get_mut().push('\n');
                occ.get_mut().push_str(comment.as_ref());
            }
            Entry::Vacant(vac) => {
                vac.insert(comment.into());
            }
        }
    }

    pub(crate) fn add_post_comment<S: Into<String> + AsRef<str>>(&mut self, entity: ir::Inst, comment: S) {
        debug_assert!(self.enabled);

        use std::collections::hash_map::Entry;
        match self.inst_post_comments.entry(entity) {
            Entry::Occupied(mut occ) => {
                occ.get_mut().push('\n');
                occ.get_mut().push_str(comment.as_ref());
            }
            Entry::Vacant(vac) => {
                vac.insert(comment.into());
            }
        }
    }
}

impl FuncWriter for &'_ CommentWriter {
    fn write_preamble(&mut self, w: &mut dyn fmt::Write, func: &ir::Function) -> Result<bool, fmt::Error> {
        for comment in &self.global_comments {
            if !comment.is_empty() {
                writeln!(w, "; {}", comment)?;
            } else {
                writeln!(w)?;
            }
        }
        if !self.global_comments.is_empty() {
            writeln!(w)?;
        }

        self.super_preamble(w, func)
    }

    fn write_entity_definition(
        &mut self,
        w: &mut dyn fmt::Write,
        _func: &ir::Function,
        entity: AnyEntity,
        value: &dyn fmt::Display,
        maybe_fact: Option<&ir::Fact>,
    ) -> fmt::Result {
        if let Some(fact) = maybe_fact {
            write!(w, "    {} ! {} = {}", entity, fact, value)?;
        } else {
            write!(w, "    {} = {}", entity, value)?;
        }

        if let Some(comment) = self.entity_comments.get(&entity) {
            writeln!(w, " ; {}", comment.replace('\n', "\n; "))
        } else {
            writeln!(w)
        }
    }

    fn write_block_header(&mut self, w: &mut dyn fmt::Write, func: &ir::Function, block: ir::Block, indent: usize) -> fmt::Result {
        PlainWriter.write_block_header(w, func, block, indent)
    }

    fn write_instruction(
        &mut self,
        w: &mut dyn fmt::Write,
        func: &ir::Function,
        aliases: &SecondaryMap<ir::Value, Vec<ir::Value>>,
        inst: ir::Inst,
        indent: usize,
    ) -> fmt::Result {
        if let Some(comment) = self.entity_comments.get(&inst.into()) {
            writeln!(w, "; {}", comment.replace('\n', "\n; "))?;
        }

        {
            // Prefix containing source location, encoding, and value locations.
            let mut s = String::with_capacity(16);

            // Source location goes first.
            let srcloc = func.srcloc(inst);
            if !srcloc.is_default() {
                write!(s, "{srcloc} ")?;
            }

            // Write out prefix and indent the instruction.
            write!(w, "{s:indent$}")?;

            // Write out the result values, if any.
            let mut has_results = false;
            for r in func.dfg.inst_results(inst) {
                if has_results {
                    write!(w, ", {r}")?;
                } else {
                    has_results = true;

                    write!(w, "{r}")?;
                }

                if let Some(f) = &func.dfg.facts[*r] {
                    write!(w, " ! {f}")?;
                }
            }
            if has_results {
                write!(w, " = ")?;
            }

            // Then the opcode, possibly with a '.type' suffix.
            let opcode = func.dfg.insts[inst].opcode();

            match type_suffix(func, inst) {
                Some(suf) => write!(w, "{opcode}.{suf}")?,
                None => write!(w, "{opcode}")?,
            }

            write_operands(w, &func.dfg, inst)?;
            if let Some(comment) = self.inst_post_comments.get(&inst) {
                writeln!(w, "; {}", comment.replace('\n', "\n; "))?;
            } else {
                writeln!(w)?;
            }

            // Value aliases come out on lines after the instruction defining the referent.
            for r in func.dfg.inst_results(inst) {
                write_value_aliases(w, aliases, *r, indent)?;
            }
        }

        Ok(())
    }
}

fn type_suffix(func: &ir::Function, inst: ir::Inst) -> Option<ir::Type> {
    let inst_data = &func.dfg.insts[inst];
    let constraints = inst_data.opcode().constraints();

    if !constraints.is_polymorphic() {
        return None;
    }

    // If the controlling type variable can be inferred from the type of the
    // designated value input operand, we don't need the type suffix.
    if constraints.use_typevar_operand() {
        let ctrl_var = inst_data.typevar_operand(&func.dfg.value_lists).unwrap();
        let def_block = match func.dfg.value_def(ctrl_var) {
            ir::ValueDef::Result(instr, _) => func.layout.inst_block(instr),
            ir::ValueDef::Param(block, _) => Some(block),
            ir::ValueDef::Union(..) => None,
        };
        if def_block.is_some() && def_block == func.layout.inst_block(inst) {
            return None;
        }
    }

    let rtype = func.dfg.ctrl_typevar(inst);
    assert!(!rtype.is_invalid(), "Polymorphic instruction must produce a result");
    Some(rtype)
}

fn write_value_aliases(w: &mut dyn fmt::Write, aliases: &SecondaryMap<ir::Value, Vec<ir::Value>>, target: ir::Value, indent: usize) -> fmt::Result {
    let mut todo_stack = vec![target];
    while let Some(target) = todo_stack.pop() {
        for &a in &aliases[target] {
            writeln!(w, "{1:0$}{2} -> {3}", indent, "", a, target)?;
            todo_stack.push(a);
        }
    }

    Ok(())
}
