mod compiler;
mod convert;
mod language;

use std::collections::BTreeSet;
use std::env;
use std::path::PathBuf;

use convert::LineIndex;
use dashmap::DashMap;
use language::{DocumentIndex, Symbol, SymbolKind as XiSymbolKind, TokenKind};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    documents: DashMap<Url, String>,
    compiler: Option<PathBuf>,
    module_dirs: Vec<PathBuf>,
}

#[derive(Default)]
struct ServerConfig {
    compiler: Option<PathBuf>,
    module_dirs: Vec<PathBuf>,
}

impl ServerConfig {
    fn from_args() -> Self {
        let mut config = Self::default();
        let mut args = env::args().skip(1);
        while let Some(arg) = args.next() {
            match arg.as_str() {
                "--compiler" => config.compiler = args.next().map(PathBuf::from),
                "--module-dir" => {
                    if let Some(path) = args.next() {
                        config.module_dirs.push(PathBuf::from(path));
                    }
                }
                _ => {}
            }
        }
        if config.compiler.is_none() {
            config.compiler = find_compiler();
        }
        config.module_dirs.extend(default_module_dirs());
        config.module_dirs.sort();
        config.module_dirs.dedup();
        config
    }
}

impl Backend {
    fn new(client: Client, config: ServerConfig) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            compiler: config.compiler,
            module_dirs: config.module_dirs,
        }
    }

    async fn refresh(&self, uri: Url, text: String) {
        self.documents.insert(uri.clone(), text.clone());
        let Some(compiler) = self.compiler.clone() else {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
            return;
        };
        let Ok(filename) = uri.to_file_path() else {
            return;
        };
        let checked_text = text.clone();
        let result = tokio::task::spawn_blocking(move || {
            compiler::check(&compiler, &filename, &checked_text)
        })
        .await;
        match result {
            Ok(Ok(diagnostics)) => {
                let current_text = self
                    .documents
                    .get(&uri)
                    .map(|entry| entry.clone())
                    .unwrap_or_default();
                if current_text != text {
                    return;
                }
                let text = current_text;
                let lines = LineIndex::new(&text);
                let diagnostics = diagnostics
                    .into_iter()
                    .map(|diagnostic| Diagnostic {
                        range: lines.range(diagnostic.span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: Some(NumberOrString::String(diagnostic.code)),
                        source: Some("xic".to_string()),
                        message: diagnostic.message,
                        ..Default::default()
                    })
                    .collect();
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
            Ok(Err(error)) => {
                self.client
                    .log_message(MessageType::ERROR, format!("Xi diagnostics: {error}"))
                    .await;
            }
            Err(error) => {
                self.client
                    .log_message(
                        MessageType::ERROR,
                        format!("Xi diagnostics task failed: {error}"),
                    )
                    .await;
            }
        }
    }

    fn module(&self, document_uri: &Url, name: &str) -> Option<LoadedModule> {
        let mut candidates = Vec::new();
        if let Ok(document_path) = document_uri.to_file_path() {
            if let Some(parent) = document_path.parent() {
                candidates.push(parent.join(format!("{name}.xi")));
            }
        }
        candidates.extend(
            self.module_dirs
                .iter()
                .map(|dir| dir.join(format!("{name}.xi"))),
        );
        for path in candidates {
            let Ok(source) = std::fs::read_to_string(&path) else {
                continue;
            };
            return Some(LoadedModule {
                index: DocumentIndex::new(&source),
                source,
                path,
            });
        }
        None
    }

    fn available_modules(&self, document_uri: &Url) -> Vec<String> {
        let mut dirs = self.module_dirs.clone();
        if let Ok(document_path) = document_uri.to_file_path() {
            if let Some(parent) = document_path.parent() {
                dirs.push(parent.to_path_buf());
            }
        }
        let mut modules = BTreeSet::new();
        for dir in dirs {
            let Ok(entries) = std::fs::read_dir(dir) else {
                continue;
            };
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|ext| ext.to_str()) == Some("xi") {
                    if let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) {
                        modules.insert(stem.to_string());
                    }
                }
            }
        }
        modules.into_iter().collect()
    }
}

struct LoadedModule {
    path: PathBuf,
    source: String,
    index: DocumentIndex,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "xi-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        ..Default::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![":".to_string(), "#".to_string()]),
                    ..Default::default()
                }),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let message = match &self.compiler {
            Some(path) => format!("xi-lsp initialized with compiler {}", path.display()),
            None => "xi-lsp initialized without xic; compiler diagnostics are disabled".to_string(),
        };
        self.client.log_message(MessageType::INFO, message).await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;
        self.refresh(document.uri, document.text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            self.refresh(params.text_document.uri, change.text).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params
            .text
            .or_else(|| self.documents.get(&uri).map(|entry| entry.clone()));
        if let Some(text) = text {
            self.refresh(uri, text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let request = params.text_document_position_params;
        let uri = request.text_document.uri;
        let Some(text) = self.documents.get(&uri).map(|entry| entry.clone()) else {
            return Ok(None);
        };
        let lines = LineIndex::new(&text);
        let offset = lines.offset(request.position);
        let index = DocumentIndex::new(&text);
        let Some((token_index, token)) = index.token_at(offset) else {
            return Ok(None);
        };

        if token.kind == TokenKind::Identifier {
            if let Some((module, member)) = module_member_at(&index, token_index) {
                if let Some(loaded) = self.module(&uri, module) {
                    if let Some(symbol) = loaded.index.symbol_named_before(member, usize::MAX) {
                        return Ok(Some(hover(
                            symbol_markdown(symbol),
                            lines.range(token.span),
                        )));
                    }
                }
            }
            if let Some(symbol) = index.symbol_named_before(&token.text, offset) {
                return Ok(Some(hover(
                    symbol_markdown(symbol),
                    lines.range(token.span),
                )));
            }
        }
        if let Some(documentation) = language::keyword_doc(&token.text) {
            return Ok(Some(hover(
                documentation.to_string(),
                lines.range(token.span),
            )));
        }
        if language::is_type_name(&token.text) {
            let base = token
                .text
                .trim_end_matches(|character: char| character.is_ascii_digit());
            if let Some(documentation) = language::keyword_doc(base) {
                return Ok(Some(hover(
                    documentation.to_string(),
                    lines.range(token.span),
                )));
            }
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let request = params.text_document_position_params;
        let uri = request.text_document.uri;
        let Some(text) = self.documents.get(&uri).map(|entry| entry.clone()) else {
            return Ok(None);
        };
        let lines = LineIndex::new(&text);
        let offset = lines.offset(request.position);
        let index = DocumentIndex::new(&text);
        let Some((token_index, token)) = index.token_at(offset) else {
            return Ok(None);
        };
        if token.kind != TokenKind::Identifier {
            return Ok(None);
        }

        if let Some((module, member)) = module_member_at(&index, token_index) {
            if let Some(loaded) = self.module(&uri, module) {
                if let Some(symbol) = loaded.index.symbol_named_before(member, usize::MAX) {
                    if let Ok(target_uri) = Url::from_file_path(&loaded.path) {
                        let target_lines = LineIndex::new(&loaded.source);
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri: target_uri,
                            range: target_lines.range(symbol.span),
                        })));
                    }
                }
            }
        }

        Ok(index
            .symbol_named_before(&token.text, offset)
            .map(|symbol| {
                GotoDefinitionResponse::Scalar(Location {
                    uri,
                    range: lines.range(symbol.span),
                })
            }))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let Some(text) = self.documents.get(&uri).map(|entry| entry.clone()) else {
            return Ok(None);
        };
        let lines = LineIndex::new(&text);
        let index = DocumentIndex::new(&text);
        let symbols = index
            .symbols
            .iter()
            .filter(|symbol| symbol.kind != XiSymbolKind::Variable)
            .map(|symbol| {
                #[allow(deprecated)]
                SymbolInformation {
                    name: symbol.name.clone(),
                    kind: lsp_symbol_kind(symbol.kind),
                    tags: None,
                    deprecated: None,
                    location: Location {
                        uri: uri.clone(),
                        range: lines.range(symbol.declaration_span),
                    },
                    container_name: None,
                }
            })
            .collect();
        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let Some(text) = self.documents.get(&uri).map(|entry| entry.clone()) else {
            return Ok(None);
        };
        let offset = LineIndex::new(&text).offset(position);
        if let Some(module) = module_prefix_before(&text, offset) {
            if let Some(loaded) = self.module(&uri, &module) {
                let items = loaded
                    .index
                    .symbols
                    .iter()
                    .filter(|symbol| symbol.kind != XiSymbolKind::Variable)
                    .map(completion_for_symbol)
                    .collect();
                return Ok(Some(CompletionResponse::Array(items)));
            }
        }

        let index = DocumentIndex::new(&text);
        let mut items: Vec<CompletionItem> = language::KEYWORDS
            .iter()
            .map(|keyword| simple_completion(keyword, CompletionItemKind::KEYWORD))
            .chain(
                language::TYPES
                    .iter()
                    .map(|ty| simple_completion(ty, CompletionItemKind::TYPE_PARAMETER)),
            )
            .collect();
        items.extend(index.symbols.iter().map(completion_for_symbol));
        items.extend(self.available_modules(&uri).into_iter().map(|module| {
            CompletionItem {
                label: module.clone(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some(format!("module {module}")),
                sort_text: Some(
                    if index.includes.contains(&module) {
                        "0"
                    } else {
                        "1"
                    }
                    .to_string()
                        + &module,
                ),
                ..Default::default()
            }
        }));
        Ok(Some(CompletionResponse::Array(items)))
    }
}

fn module_member_at(index: &DocumentIndex, token_index: usize) -> Option<(&str, &str)> {
    if token_index >= 2 && index.tokens[token_index - 1].text == "::" {
        Some((
            &index.tokens[token_index - 2].text,
            &index.tokens[token_index].text,
        ))
    } else {
        None
    }
}

fn module_prefix_before(text: &str, offset: usize) -> Option<String> {
    let prefix = &text[..floor_boundary(text, offset.min(text.len()))];
    let bytes = prefix.as_bytes();
    let mut cursor = bytes.len();
    while cursor > 0 && is_ident_byte(bytes[cursor - 1]) {
        cursor -= 1;
    }
    if cursor < 2 || &bytes[cursor - 2..cursor] != b"::" {
        return None;
    }
    let end = cursor - 2;
    let mut start = end;
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    (start < end).then(|| prefix[start..end].to_string())
}

fn floor_boundary(text: &str, mut offset: usize) -> usize {
    while offset > 0 && !text.is_char_boundary(offset) {
        offset -= 1;
    }
    offset
}

fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

fn hover(value: String, range: Range) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(range),
    }
}

fn symbol_markdown(symbol: &Symbol) -> String {
    let suffix = if symbol.kind == XiSymbolKind::Intrinsic {
        "\n\n*(compiler intrinsic)*"
    } else {
        ""
    };
    format!("```xi\n{}\n```{suffix}", symbol.detail)
}

fn completion_for_symbol(symbol: &Symbol) -> CompletionItem {
    CompletionItem {
        label: symbol.name.clone(),
        kind: Some(match symbol.kind {
            XiSymbolKind::Function | XiSymbolKind::Intrinsic => CompletionItemKind::FUNCTION,
            XiSymbolKind::Struct => CompletionItemKind::STRUCT,
            XiSymbolKind::Constant => CompletionItemKind::CONSTANT,
            XiSymbolKind::Variable => CompletionItemKind::VARIABLE,
        }),
        detail: Some(symbol.detail.clone()),
        ..Default::default()
    }
}

fn simple_completion(label: &str, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(kind),
        ..Default::default()
    }
}

fn lsp_symbol_kind(kind: XiSymbolKind) -> tower_lsp::lsp_types::SymbolKind {
    match kind {
        XiSymbolKind::Function | XiSymbolKind::Intrinsic => {
            tower_lsp::lsp_types::SymbolKind::FUNCTION
        }
        XiSymbolKind::Struct => tower_lsp::lsp_types::SymbolKind::STRUCT,
        XiSymbolKind::Constant => tower_lsp::lsp_types::SymbolKind::CONSTANT,
        XiSymbolKind::Variable => tower_lsp::lsp_types::SymbolKind::VARIABLE,
    }
}

fn find_compiler() -> Option<PathBuf> {
    let executable = if cfg!(windows) { "xic.exe" } else { "xic" };
    let mut candidates = Vec::new();
    if let Ok(current) = env::current_exe() {
        if let Some(parent) = current.parent() {
            candidates.push(parent.join(executable));
        }
    }
    if let Some(path) = env::var_os("PATH") {
        candidates.extend(env::split_paths(&path).map(|dir| dir.join(executable)));
    }
    if let Some(local_app_data) = env::var_os("LOCALAPPDATA") {
        candidates.push(
            PathBuf::from(local_app_data)
                .join("Xi")
                .join("bin")
                .join(executable),
        );
    }
    if let Ok(current_dir) = env::current_dir() {
        candidates.push(current_dir.join(executable));
    }
    candidates.into_iter().find(|path| path.is_file())
}

fn default_module_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(current) = env::current_exe() {
        if let Some(parent) = current.parent() {
            dirs.push(parent.join("modules"));
            dirs.push(parent.join("..").join("modules"));
        }
    }
    if let Some(value) = env::var_os("XI_MODULE_PATH") {
        dirs.extend(env::split_paths(&value));
    }
    if let Some(local_app_data) = env::var_os("LOCALAPPDATA") {
        dirs.push(PathBuf::from(local_app_data).join("Xi").join("modules"));
    } else if let Some(xdg) = env::var_os("XDG_DATA_HOME") {
        dirs.push(PathBuf::from(xdg).join("xi").join("modules"));
    } else if let Some(home) = env::var_os("HOME") {
        dirs.push(PathBuf::from(home).join(".local/share/xi/modules"));
    }
    dirs
}

#[tokio::main]
async fn main() {
    let config = ServerConfig::from_args();
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(move |client| Backend::new(client, config));
    Server::new(stdin, stdout, socket).serve(service).await;
}
