use std::mem::{ManuallyDrop, transmute};

use mollie_typed_ast::{BlockRef, Expr, ExprRef, FuncSource, Stmt, StmtRef, TypeChecker, TypedAST, VFunc};
use mollie_typing::{AdtVariantRef, FieldRef, TypeInfo, TypeInfoRef};
use tokio::{
    io::{stdin, stdout},
    sync::{Mutex, MutexGuard},
};
use tower_lsp_server::{
    Client, LanguageServer, LspService, Server,
    jsonrpc::{Error, Result},
    ls_types as types,
};

struct State {
    entry_block: BlockRef,
    typed_ast: Option<TypedAST>,
    checker: TypeChecker,
}

#[derive(Debug)]
enum SearchResult {
    Expr(ExprRef),
    ExprField(ExprRef, AdtVariantRef, FieldRef),
    ExprVFunc(ExprRef, VFunc),
    ExprType(ExprRef, TypeInfoRef),
    ExprOperator(ExprRef, usize),
    Block(BlockRef),
    Stmt(StmtRef),
}

struct KnownState<'a> {
    entry_block: BlockRef,
    typed_ast: &'a mut TypedAST,
    checker: &'a mut TypeChecker,
    #[doc(hidden)]
    guard: ManuallyDrop<MutexGuard<'a, State>>,
}

impl KnownState<'_> {
    fn find_in_block(&self, position: types::Position, block_ref: BlockRef) -> Option<SearchResult> {
        let block = &self.typed_ast.blocks[block_ref].value;

        for &stmt in &block.stmts {
            match self.typed_ast.statements[stmt] {
                Stmt::Expr(value) | Stmt::VariableDecl { value, .. } => {
                    if self.typed_ast.exprs[value].span.contains(position.line, position.character) {
                        return self.find_in_expr(position, value);
                    }
                }
                Stmt::Import(_) => (),
            }
        }

        Some(SearchResult::Block(block_ref))
    }

    fn find_in_expr(&self, position: types::Position, expr_ref: ExprRef) -> Option<SearchResult> {
        match &self.typed_ast[expr_ref].value {
            &Expr::If { condition, block, else_block } => {
                if Self::expr_contains(self.typed_ast, condition, position) {
                    return self.find_in_expr(position, condition);
                } else if Self::block_contains(self.typed_ast, block, position) {
                    return self.find_in_block(position, block);
                } else if let Some(else_block) = else_block
                    && Self::expr_contains(self.typed_ast, else_block, position)
                {
                    return self.find_in_expr(position, else_block);
                }
            }
            &Expr::Block(block) => {
                if Self::block_contains(self.typed_ast, block, position) {
                    return self.find_in_block(position, block);
                }
            }
            Expr::Literal(_) | Expr::Var(_) | Expr::Error(_) | Expr::Nothing | Expr::TypeIndex { .. } => (),
            &Expr::Access { target, field } => {
                if Self::expr_contains(self.typed_ast, target, position) {
                    return self.find_in_expr(position, target);
                } else if field.span.contains(position.line, position.character) {
                    return Some(SearchResult::ExprField(expr_ref, AdtVariantRef::default(), field.value));
                }
            }
            &Expr::VTableAccess { target, func } => {
                if Self::expr_contains(self.typed_ast, target, position) {
                    return self.find_in_expr(position, target);
                } else if func.span.contains(position.line, position.character) {
                    return Some(SearchResult::ExprVFunc(expr_ref, func.value));
                }
            }
            &Expr::Index { target, index } => {
                if Self::expr_contains(self.typed_ast, target, position) {
                    return self.find_in_expr(position, target);
                } else if Self::expr_contains(self.typed_ast, index, position) {
                    return self.find_in_expr(position, index);
                }
            }
            &Expr::While { condition, block } => {
                if Self::expr_contains(self.typed_ast, condition, position) {
                    return self.find_in_expr(position, condition);
                } else if Self::block_contains(self.typed_ast, block, position) {
                    return self.find_in_block(position, block);
                }
            }
            Expr::Array(expr_refs) => {
                for &element in expr_refs {
                    if Self::expr_contains(self.typed_ast, element, position) {
                        return self.find_in_expr(position, element);
                    }
                }
            }
            &Expr::Binary { operator, lhs, rhs } => {
                if Self::expr_contains(self.typed_ast, lhs, position) {
                    return self.find_in_expr(position, lhs);
                } else if operator.span.contains(position.line, position.character) {
                    return Some(SearchResult::ExprOperator(expr_ref, operator.value as usize));
                } else if Self::expr_contains(self.typed_ast, rhs, position) {
                    return self.find_in_expr(position, rhs);
                }
            }
            Expr::Call { func, args } => {
                if let FuncSource::Expr(expr) = *func
                    && Self::expr_contains(self.typed_ast, expr, position)
                {
                    return self.find_in_expr(position, expr);
                }

                for &arg in args {
                    if Self::expr_contains(self.typed_ast, arg, position) {
                        return self.find_in_expr(position, arg);
                    }
                }
            }
            Expr::Closure { body, .. } => {
                if Self::block_contains(self.typed_ast, *body, position) {
                    return self.find_in_block(position, *body);
                }
            }
            &Expr::Cast { expr, ty } => {
                if Self::expr_contains(self.typed_ast, expr, position) {
                    return self.find_in_expr(position, expr);
                } else if ty.span.contains(position.line, position.character) {
                    return Some(SearchResult::ExprType(expr_ref, self.checker.core_types.cast_primitive(ty.value)));
                }
            }
            Expr::Construct { fields, variant, .. } => {
                for &(field, _, v) in fields {
                    if let Some((expr, span)) = v {
                        if Self::expr_contains(self.typed_ast, expr, position) {
                            return self.find_in_expr(position, expr);
                        } else if span.contains(position.line, position.character) {
                            return Some(SearchResult::ExprField(expr_ref, *variant, field));
                        }
                    }
                }
            }
            &Expr::IsPattern { target, .. } => {
                if Self::expr_contains(self.typed_ast, target, position) {
                    return self.find_in_expr(position, target);
                }
            }
        }

        Some(SearchResult::Expr(expr_ref))
    }

    fn expr_contains(ast: &TypedAST, expr: ExprRef, position: types::Position) -> bool {
        !matches!(ast[expr].value, Expr::Error(_)) && ast[expr].span.contains(position.line, position.character)
    }

    fn block_contains(ast: &TypedAST, block: BlockRef, position: types::Position) -> bool {
        ast[block].span.contains(position.line, position.character)
    }

    fn find(&self, position: types::Position) -> Option<SearchResult> {
        for (expr_ref, _) in self.typed_ast.exprs.iter().filter(|(_, expr)| !matches!(expr.value, Expr::Error(_))) {
            if Self::expr_contains(self.typed_ast, expr_ref, position) {
                return self.find_in_expr(position, expr_ref);
            }
        }

        None
    }
}

impl Drop for KnownState<'_> {
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.guard) };
    }
}

struct Backend {
    client: Client,
    state: Mutex<State>,
}

impl Backend {
    pub async fn require_state<'a>(&'a self) -> Option<KnownState<'a>> {
        let mut guard = self.state.lock().await;

        if guard.typed_ast.is_some() {
            let state = unsafe { transmute::<&mut State, &'a mut State>(&mut *guard) };

            Some(KnownState {
                entry_block: state.entry_block,
                typed_ast: unsafe { state.typed_ast.as_mut().unwrap_unchecked() },
                checker: &mut state.checker,
                guard: ManuallyDrop::new(guard),
            })
        } else {
            None
        }
    }

    pub async fn try_require_state(&self) -> Result<KnownState<'_>> {
        self.require_state().await.ok_or_else(Error::internal_error)
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: types::InitializeParams) -> Result<types::InitializeResult> {
        Ok(types::InitializeResult {
            server_info: None,
            capabilities: types::ServerCapabilities {
                text_document_sync: Some(types::TextDocumentSyncCapability::Kind(types::TextDocumentSyncKind::INCREMENTAL)),
                completion_provider: Some(types::CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                execute_command_provider: Some(types::ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    ..Default::default()
                }),
                workspace: Some(types::WorkspaceServerCapabilities {
                    workspace_folders: Some(types::WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(types::OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                hover_provider: Some(types::HoverProviderCapability::Simple(true)),
                rename_provider: Some(types::OneOf::Left(true)),
                ..types::ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: types::InitializedParams) {
        self.client.log_message(types::MessageType::INFO, "initialized!").await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: types::DidChangeWorkspaceFoldersParams) {
        self.client.log_message(types::MessageType::INFO, "workspace folders changed!").await;
    }

    async fn did_change_configuration(&self, _: types::DidChangeConfigurationParams) {
        self.client.log_message(types::MessageType::INFO, "configuration changed!").await;
    }

    async fn did_change_watched_files(&self, _: types::DidChangeWatchedFilesParams) {
        self.client.log_message(types::MessageType::INFO, "watched files have changed!").await;
    }

    async fn execute_command(&self, _: types::ExecuteCommandParams) -> Result<Option<types::LSPAny>> {
        self.client.log_message(types::MessageType::INFO, "command executed!").await;

        match self.client.apply_edit(types::WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(types::MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(types::MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(types::MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn hover(&self, params: types::HoverParams) -> Result<Option<types::Hover>> {
        let state = self.try_require_state().await?;
        let result = state.find(params.text_document_position_params.position);

        self.client.log_message(types::MessageType::INFO, format!("{params:#?}")).await;
        self.client.log_message(types::MessageType::INFO, format!("{result:?}")).await;

        Ok(result.map(|result| match result {
            SearchResult::Expr(expr) => types::Hover {
                contents: types::HoverContents::Markup(types::MarkupContent {
                    kind: types::MarkupKind::Markdown,
                    value: format!("# тебя переебало\n\nзадумайся.\n\n```rust\n{:#?}\n```", state.typed_ast[expr].value),
                }),
                range: None,
            },
            SearchResult::ExprField(expr, variant, field) => {
                let ty = state.typed_ast[expr].ty;

                if let &TypeInfo::Adt(adt_ref, ..) = state.checker.solver.get_info(ty) {
                    let adt_name = state.checker.adt_types[adt_ref].name.as_deref().unwrap_or_default();
                    let (field_name, field_ty, ..) = &state.checker.adt_types[adt_ref].variants[variant].fields[field];

                    types::Hover {
                        contents: types::HoverContents::Markup(types::MarkupContent {
                            kind: types::MarkupKind::Markdown,
                            value: format!(
                                "```rust\n{adt_name} -> {field_name}: {}\n```",
                                state.checker.display_of_field_type(field_ty, Some(ty))
                            ),
                        }),
                        range: None,
                    }
                } else {
                    types::Hover {
                        contents: types::HoverContents::Markup(types::MarkupContent {
                            kind: types::MarkupKind::Markdown,
                            value: format!("```rust\n{}\n```", state.checker.display_of_type(ty, None)),
                        }),
                        range: None,
                    }
                }
            }
            _ => types::Hover {
                contents: types::HoverContents::Markup(types::MarkupContent {
                    kind: types::MarkupKind::Markdown,
                    value: format!("# тебя переебало\n\nзадумайся.\n\n```rust\n{result:?}\n```"),
                }),
                range: None,
            },
        }))
    }

    async fn did_open(&self, doc: types::DidOpenTextDocumentParams) {
        self.client.log_message(types::MessageType::INFO, "file opened!").await;
        self.client.log_message(types::MessageType::INFO, &doc.text_document.text).await;

        let mut state = self.state.lock().await;
        let mut ast = TypedAST::default();

        match state.checker.type_check(doc.text_document.text, &mut ast) {
            Ok(entry_block) => state.entry_block = entry_block,
            Err(errors) => {
                let mut diags = Vec::new();

                self.client.log_message(types::MessageType::INFO, format!("{:#?}", errors.0)).await;

                for error in errors.0 {
                    diags.push(types::Diagnostic::new_simple(
                        types::Range::new(
                            types::Position::new(error.span.range.start_line, error.span.range.start_column),
                            types::Position::new(error.span.range.end_line, error.span.range.end_column),
                        ),
                        error.value.name().to_string(),
                    ));
                }

                self.client.publish_diagnostics(doc.text_document.uri, diags, None).await;
            }
        }

        self.client.log_message(types::MessageType::INFO, format!("{ast:#?}")).await;
        self.client
            .log_message(types::MessageType::INFO, format!("{:#?}", state.checker.adt_types))
            .await;

        state.typed_ast.replace(ast);
    }

    async fn did_change(&self, doc: types::DidChangeTextDocumentParams) {
        self.client
            .log_message(types::MessageType::INFO, format!("file changed! {:#?}", doc.content_changes))
            .await;
    }

    async fn did_save(&self, params: types::DidSaveTextDocumentParams) {
        self.client.log_message(types::MessageType::INFO, "file saved!").await;
        self.client.log_message(types::MessageType::INFO, format!("{params:#?}")).await;
    }

    async fn did_close(&self, _: types::DidCloseTextDocumentParams) {
        self.client.log_message(types::MessageType::INFO, "file closed!").await;
    }

    async fn rename(&self, params: types::RenameParams) -> Result<Option<types::WorkspaceEdit>> {
        self.client.log_message(types::MessageType::INFO, format!("{params:#?}")).await;

        Ok(None)
    }

    async fn completion(&self, _: types::CompletionParams) -> Result<Option<types::CompletionResponse>> {
        Ok(Some(types::CompletionResponse::Array(vec![
            types::CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            types::CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }
}

#[tokio::main]
async fn main() {
    let stdin = stdin();
    let stdout = stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        state: Mutex::new(State {
            entry_block: BlockRef::default(),
            typed_ast: None,
            checker: TypeChecker::new(),
        }),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
