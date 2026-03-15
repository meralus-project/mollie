#![allow(clippy::missing_errors_doc)]

use core::{
    fmt::{self, Display},
    mem,
};

pub struct Formatter<'a, 'b> {
    track_newlines: bool,
    newlines: usize,
    indent_level: usize,
    f: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> Formatter<'a, 'b> {
    pub const fn new(f: &'a mut fmt::Formatter<'b>) -> Self {
        Self {
            track_newlines: false,
            newlines: 1,
            indent_level: 0,
            f,
        }
    }

    pub fn tracked_lines<F: FnOnce(&mut Self) -> fmt::Result>(&mut self, func: F) -> Result<usize, fmt::Error> {
        self.track_newlines = true;

        let newlines = mem::replace(&mut self.newlines, 1);

        func(self)?;

        self.track_newlines = false;

        Ok(mem::replace(&mut self.newlines, newlines))
    }

    pub fn newline(&mut self) -> fmt::Result {
        if self.track_newlines {
            self.newlines += 1;
        }

        self.f.write_str("\n")
    }

    pub fn write_indent(&mut self) -> fmt::Result {
        for _ in 0..self.indent_level {
            self.f.write_str("  ")?;
        }

        Ok(())
    }

    pub const fn inc_indent(&mut self) {
        self.indent_level += 1;
    }

    pub const fn dec_indent(&mut self) {
        self.indent_level -= 1;
    }

    pub fn in_scope<F: FnOnce(&mut Self) -> fmt::Result>(&mut self, func: F) -> fmt::Result {
        self.inc_indent();

        func(self)?;

        self.dec_indent();

        Ok(())
    }

    pub fn in_block<S: Display, E: Display, F: FnOnce(&mut Self) -> fmt::Result>(&mut self, start: S, end: E, compact: bool, func: F) -> fmt::Result {
        start.fmt(self.f)?;

        if compact {
            self.f.write_str(" ")?;
        } else {
            self.inc_indent();
            self.newline()?;
        }

        func(self)?;

        if compact {
            self.f.write_str(" ")?;
        } else {
            self.newline()?;
            self.dec_indent();
            self.write_indent()?;
        }

        end.fmt(self.f)?;

        Ok(())
    }

    pub fn fmt_separated_by<T: Display, V, F: Fn(&mut Self, &V) -> fmt::Result>(&mut self, items: &[V], separator: T, pretty: bool, fmt: F) -> fmt::Result {
        let mut first = true;

        for item in items {
            if first {
                first = false;

                if pretty {
                    self.write_indent()?;
                }
            } else {
                separator.fmt(self.f)?;

                if pretty {
                    self.newline()?;
                    self.write_indent()?;
                }
            }

            fmt(self, item)?;
        }

        Ok(())
    }

    pub fn fmt_literal(&mut self, literal_expr: &mollie_parser::LiteralExpr) -> fmt::Result {
        use mollie_parser::{
            LiteralExpr::{Bool, Number, String},
            Number::{F32, I64},
        };

        match literal_expr {
            Number(number, _suffix) => match number.value {
                I64(value) => value.fmt(self.f),
                F32(value) => value.fmt(self.f),
            },
            Bool(value) => value.fmt(self.f),
            String(value) => write!(self.f, "{value:?}"),
        }
    }

    pub fn fmt_type(&mut self, ty: &mollie_parser::Type) -> fmt::Result {
        match ty {
            mollie_parser::Type::Primitive(primitive_type) => primitive_type.fmt(self.f),
            mollie_parser::Type::Array(element, size) => {
                self.fmt_type(&element.value)?;

                if let Some(size) = size {
                    self.f.write_str("[")?;

                    size.value.fmt(self.f)?;

                    self.f.write_str("]")
                } else {
                    self.f.write_str("[]")
                }
            }
            mollie_parser::Type::Func(_args, _returns) => todo!(),
            mollie_parser::Type::Path(type_path_expr) => self.fmt_type_path(type_path_expr),
        }
    }

    pub fn fmt_type_path(&mut self, type_path_expr: &mollie_parser::TypePathExpr) -> fmt::Result {
        self.fmt_separated_by(&type_path_expr.segments, "::", false, |me, segment| {
            me.f.write_str(segment.value.name.value.0.as_str())?;

            if let Some(args) = &segment.value.args {
                me.f.write_str("<")?;
                me.fmt_separated_by(&args.value.0, ", ", false, |me, arg| me.fmt_type(&arg.value))?;
                me.f.write_str(">")
            } else {
                Ok(())
            }
        })
    }

    pub fn fmt_node_expr(&mut self, node_expr: &mollie_parser::NodeExpr) -> fmt::Result {
        self.fmt_type_path(&node_expr.name.value)?;
        self.f.write_str(" ")?;
        self.in_block('{', '}', node_expr.properties.is_empty() && node_expr.children.value.is_empty(), |me| {
            me.fmt_separated_by(&node_expr.properties, ",", true, |me, prop| {
                prop.value.name.value.0.fmt(me.f)?;

                if let Some(value) = &prop.value.value {
                    me.f.write_str(": ")?;
                    me.fmt_expr(&value.value)
                } else {
                    Ok(())
                }
            })?;

            if !node_expr.children.value.is_empty() {
                me.f.write_str(",")?;
                me.newline()?;

                let mut write_sep = false;

                for item in &node_expr.children.value {
                    me.newline()?;

                    if write_sep {
                        me.newline()?;
                    }

                    me.write_indent()?;

                    write_sep = me.tracked_lines(|me| me.fmt_node_expr(&item.value))? > 1;
                }
            }

            Ok(())
        })
    }

    pub fn fmt_expr(&mut self, expr: &mollie_parser::Expr) -> fmt::Result {
        use mollie_parser::Expr::{Array, Binary, Block, Closure, ForIn, FunctionCall, Ident, IfElse, Index, Is, Literal, Node, This, TypeIndex, While};

        match expr {
            Literal(literal_expr) => self.fmt_literal(literal_expr),
            FunctionCall(_) => todo!(),
            Node(node_expr) => self.fmt_node_expr(node_expr),
            Index(_) => todo!(),
            Binary(_) => todo!(),
            TypeIndex(_) => todo!(),
            Array(_) => todo!(),
            IfElse(_) => todo!(),
            While(_) => todo!(),
            Block(_) => todo!(),
            Is(_) => todo!(),
            Closure(_) => todo!(),
            ForIn(_) => todo!(),
            Ident(ident) => ident.0.fmt(self.f),
            This => self.f.write_str("self"),
            _ => todo!(),
        }
    }
}
