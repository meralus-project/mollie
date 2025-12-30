mod component_decl;
mod enum_decl;
mod expression;
mod func_decl;
mod implementation;
mod import;
mod struct_decl;
mod trait_decl;
mod variable_decl;

use mollie_shared::Positioned;

pub use self::{
    component_decl::{ComponentDecl, ComponentProperty},
    enum_decl::EnumDecl,
    expression::*,
    func_decl::{Argument, FuncDecl},
    implementation::{Impl, ImplFunction},
    import::{Import, ImportKind},
    struct_decl::{Property, StructDecl},
    trait_decl::{TraitDecl, TraitFuncArgument, TraitFunction},
    variable_decl::VariableDecl,
};
use super::{ParseResult, Parser};
use crate::Parse;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Stmt {
    Expression(Expr),
    VariableDecl(VariableDecl),
    StructDecl(StructDecl),
    ComponentDecl(ComponentDecl),
    TraitDecl(TraitDecl),
    EnumDecl(EnumDecl),
    FuncDecl(FuncDecl),
    Impl(Impl),
    Import(Import),
}

impl Parse for Stmt {
    fn parse(parser: &mut Parser) -> ParseResult<Positioned<Self>> {
        ComponentDecl::parse(parser)
            .map(|v| v.map(Self::ComponentDecl))
            .or_else(|_| StructDecl::parse(parser).map(|v| v.map(Self::StructDecl)))
            .or_else(|_| EnumDecl::parse(parser).map(|v| v.map(Self::EnumDecl)))
            .or_else(|_| VariableDecl::parse(parser).map(|v| v.map(Self::VariableDecl)))
            .or_else(|_| Impl::parse(parser).map(|v| v.map(Self::Impl)))
            .or_else(|_| FuncDecl::parse(parser).map(|v| v.map(Self::FuncDecl)))
            .or_else(|_| TraitDecl::parse(parser).map(|v| v.map(Self::TraitDecl)))
            .or_else(|_| Import::parse(parser).map(|v| v.map(Self::Import)))
            .or_else(|_| Expr::parse(parser).map(|v| v.map(Self::Expression)))
    }
}
