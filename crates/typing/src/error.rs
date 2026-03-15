use std::fmt;

use mollie_index::Idx;

use crate::{AdtKind, AdtRef, AdtVariantRef, ModuleId, PrimitiveType, TypeContext, TypeRef};

#[derive(Debug, Clone, Copy)]
pub enum SpecialAdtKind {
    AnyOf,
    Specific(AdtKind),
    WithExpectation(AdtKind),
}

#[derive(Debug, Clone, Copy)]
pub enum TypeErrorValue {
    Type,
    Array(Option<usize>),
    Adt(SpecialAdtKind),
    Trait,
    Value,
    Function,
    Module,
    Generic,
    Nothing,
    PrimitiveType(PrimitiveType),
    ExplicitType(TypeRef),
}

#[derive(Debug, Clone)]
pub enum LookupType {
    Variable,
    Type { inside: ModuleId },
    Module { inside: ModuleId },
}

#[derive(Debug, Clone)]
pub enum TypeError {
    Unexpected { expected: TypeErrorValue, found: TypeErrorValue },
    VariantRequired(AdtRef),
    NoField { adt: AdtRef, variant: AdtVariantRef, name: String },
    NoFunction { name: String, postfix: bool },
    NonIndexable { ty: TypeRef, name: String },
    NotFound { name: String, was_looking_for: LookupType },
    NotPostfix { name: String },
    ArgumentCountMismatch { expected: usize, found: usize },
    NonConstantEvaluable,
    // InvalidPostfixFunction { reasons: Vec<Positioned<PostfixRequirement>> },
    // Parse(mollie_parser::ParseError),
}

impl TypeError {
    #[cfg(feature = "ariadne")]
    pub fn add_to_report<S: ariadne::Span>(self, span: S, report: &mut ariadne::ReportBuilder<'_, S>, type_context: &TypeContext) {
        let label = ariadne::Label::new(span).with_color(ariadne::Color::Cyan);

        match self {
            Self::Unexpected { expected, found } => {
                let expected = match &expected {
                    TypeErrorValue::Nothing => None,
                    value => Some(fmt::from_fn(|f| match value {
                        TypeErrorValue::Type => f.write_str("`type`"),
                        TypeErrorValue::Adt(adt_kind) => f.write_str(match adt_kind {
                            SpecialAdtKind::Specific(AdtKind::Struct) => "`struct`",
                            SpecialAdtKind::Specific(AdtKind::View) => "`view`",
                            SpecialAdtKind::Specific(AdtKind::Enum) => "`enum`",
                            SpecialAdtKind::WithExpectation(AdtKind::Struct) => "`enum` or `view`",
                            SpecialAdtKind::WithExpectation(AdtKind::View) => "`struct` or `enum`",
                            SpecialAdtKind::WithExpectation(AdtKind::Enum) => "`struct` or `view`",
                            SpecialAdtKind::AnyOf => "`struct`, `view` or `enum`",
                        }),
                        TypeErrorValue::Trait => f.write_str("`trait`"),
                        TypeErrorValue::Value => f.write_str("`value`"),
                        TypeErrorValue::Function => f.write_str("`function`"),
                        TypeErrorValue::Array(_) => f.write_str("`array`"),
                        TypeErrorValue::Module => f.write_str("`module`"),
                        TypeErrorValue::Generic => f.write_str("`generic`"),
                        TypeErrorValue::PrimitiveType(ty) => write!(f, "`{ty}`"),
                        TypeErrorValue::Nothing => unreachable!(),
                        &TypeErrorValue::ExplicitType(type_ref) => write!(f, "`{}`", type_context.display_of(type_ref)),
                    })),
                };

                let found = match &found {
                    TypeErrorValue::Nothing => None,
                    value => Some(fmt::from_fn(|f| match value {
                        TypeErrorValue::Type => f.write_str("`type`"),
                        TypeErrorValue::Adt(adt_kind) => f.write_str(match adt_kind {
                            SpecialAdtKind::Specific(AdtKind::Struct) => "`struct`",
                            SpecialAdtKind::Specific(AdtKind::View) => "`view`",
                            SpecialAdtKind::Specific(AdtKind::Enum) => "`enum`",
                            SpecialAdtKind::WithExpectation(AdtKind::Struct) => "`enum` or `view`",
                            SpecialAdtKind::WithExpectation(AdtKind::View) => "`struct` or `enum`",
                            SpecialAdtKind::WithExpectation(AdtKind::Enum) => "`struct` or `view`",
                            SpecialAdtKind::AnyOf => "`struct`, `view` or `enum`",
                        }),
                        TypeErrorValue::Trait => f.write_str("`trait`"),
                        TypeErrorValue::Value => f.write_str("`value`"),
                        TypeErrorValue::Function => f.write_str("`function`"),
                        TypeErrorValue::Array(_) => f.write_str("`array`"),
                        TypeErrorValue::Module => f.write_str("`module`"),
                        TypeErrorValue::Generic => f.write_str("`generic`"),
                        TypeErrorValue::PrimitiveType(ty) => write!(f, "`{ty}`"),
                        TypeErrorValue::Nothing => unreachable!(),
                        &TypeErrorValue::ExplicitType(type_ref) => write!(f, "`{}`", type_context.display_of(type_ref)),
                    })),
                };

                if let Some(expected) = expected {
                    report.set_message(format!("expected {expected}"));
                }

                report.add_label(if let Some(found) = found {
                    label.with_message(format!("found {found}"))
                } else {
                    label
                });
            }
            Self::NoField { adt, variant, name } => {
                report.set_message("no field");
                report.add_label(label.with_message(format!(
                    "`{}{}` doesn't have field called `{name}`",
                    type_context.adt_types[adt].name.as_deref().unwrap_or_default(),
                    if matches!(type_context.adt_types[adt].kind, AdtKind::Enum) {
                        format!("::{}", type_context.adt_types[adt].variants[variant].name.as_deref().unwrap_or_default())
                    } else {
                        String::new()
                    }
                )));
            }
            Self::VariantRequired(adt) => {
                report.set_message("can't construct");
                report.add_label(label.with_message(format!(
                    "`{}` requires to specify variant explicitly",
                    type_context.adt_types[adt].name.as_deref().unwrap_or_default(),
                )));
            }
            Self::NonIndexable { ty, name } => {
                report.set_message("non-indexable value");
                report.add_label(label.with_message(format!("`{}` can't have fields and be indexed by `{name}`", type_context.display_of(ty))));
            }
            // Self::InvalidPostfixFunction { reasons } => {
            //     report.set_message("invalid postfix function definition");

            //     for reason in reasons {
            //         let label = Label::new(("ui.mol", reason.span.start..reason.span.end)).with_color(ariadne::Color::Cyan);

            //         report.add_label(match reason.value {
            //             mollie_typed_ast::PostfixRequirement::NoGenerics => label.with_message("generics are not allowed in postfix context"),
            //             mollie_typed_ast::PostfixRequirement::OneArgument => label.with_message("one argument is expected"),
            //             mollie_typed_ast::PostfixRequirement::OnlyOneArgument => label.with_message("multiple arguments are not allowed"),
            //             mollie_typed_ast::PostfixRequirement::ArgumentType => {
            //                 label.with_message("argument type must be either an (unsigned) integer or a float")
            //             }
            //         });
            //     }
            // }
            Self::NotFound { name, was_looking_for } => {
                let message = match was_looking_for {
                    LookupType::Variable => format!("there's no variable called `{name}`"),
                    LookupType::Type { inside } => {
                        if inside == ModuleId::ZERO {
                            format!("there's no type called `{name}`")
                        } else {
                            format!("there's no type called `{name}` in `{}`", type_context.modules[inside].name)
                        }
                    }
                    LookupType::Module { inside } => {
                        if inside == ModuleId::ZERO {
                            format!("there's no module called `{name}`")
                        } else {
                            format!("there's no module called `{name}` in `{}`", type_context.modules[inside].name)
                        }
                    }
                };

                report.set_message(message);
                report.add_label(label.with_message("tried to access here"));
            }
            Self::NoFunction { name, postfix } => {
                if postfix {
                    report.set_message(format!("there's no postfix function called `{name}`"));
                    report.add_label(label.with_message("tried to use here"));
                } else {
                    report.set_message(format!("there's no function called `{name}`"));
                    report.add_label(label.with_message("tried to call here"));
                }
            }
            Self::NotPostfix { name } => {
                report.set_message(format!("`{name}` can't be used in postfix context"));
                report.add_label(label.with_message("tried to use here"));
            }
            Self::NonConstantEvaluable => {
                report.set_message("expression can't be evaluated at compile-time");
                report.add_label(label.with_message("this expression"));
            }
            Self::ArgumentCountMismatch { expected, found } => {
                report.set_message(if found > expected {
                    "received more arguments than was expected"
                } else {
                    "received less arguments than was expected"
                });

                report.add_label(label.with_message(format!("expected {expected} arguments here, found {found}")));
            }
        }
    }
}
