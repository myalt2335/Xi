use serde::Deserialize;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn contains(self, offset: usize) -> bool {
        self.start <= offset && offset < self.end
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    Identifier,
    Directive,
    String,
    Number,
    Symbol,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SymbolKind {
    Function,
    Struct,
    Constant,
    Intrinsic,
    Variable,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub span: Span,
    pub declaration_span: Span,
    pub detail: String,
}

#[derive(Clone, Debug)]
pub struct DocumentIndex {
    pub tokens: Vec<Token>,
    pub symbols: Vec<Symbol>,
    pub includes: Vec<String>,
}

impl DocumentIndex {
    pub fn new(source: &str) -> Self {
        let tokens = lex(source);
        let mut symbols = Vec::new();
        let mut includes = Vec::new();
        let mut brace_depth = 0usize;
        let mut index = 0usize;

        while index < tokens.len() {
            let token = &tokens[index];
            if token.text == "}" {
                brace_depth = brace_depth.saturating_sub(1);
            }

            if brace_depth == 0 {
                match token.text.as_str() {
                    "#include" => {
                        if let Some(module) = include_name(&tokens, index + 1) {
                            includes.push(module);
                        }
                    }
                    "f" | "fn" | "function" => {
                        if let Some(symbol) = function_symbol(source, &tokens, index) {
                            symbols.push(symbol);
                        }
                    }
                    "struct" | "cstruct" => {
                        if let Some(name) = tokens.get(index + 1).filter(|t| is_identifier(t)) {
                            symbols.push(Symbol {
                                name: name.text.clone(),
                                kind: SymbolKind::Struct,
                                span: name.span,
                                declaration_span: Span {
                                    start: token.span.start,
                                    end: name.span.end,
                                },
                                detail: format!("{} {}", token.text, name.text),
                            });
                        }
                    }
                    "intrinsic" => {
                        if let Some(name) = tokens.get(index + 1).filter(|t| is_identifier(t)) {
                            symbols.push(Symbol {
                                name: name.text.clone(),
                                kind: SymbolKind::Intrinsic,
                                span: name.span,
                                declaration_span: Span {
                                    start: token.span.start,
                                    end: name.span.end,
                                },
                                detail: format!("intrinsic {}", name.text),
                            });
                        }
                    }
                    "extern" | "cextern" => {
                        if tokens.get(index + 1).is_some_and(|next| next.text != "f") {
                            if let Some(symbol) = extern_constant_symbol(source, &tokens, index) {
                                symbols.push(symbol);
                            }
                        }
                    }
                    _ => {}
                }
            }

            if token.text == "{" {
                brace_depth += 1;
            }
            index += 1;
        }

        collect_parameters_and_locals(source, &tokens, &mut symbols);
        Self {
            tokens,
            symbols,
            includes,
        }
    }

    pub fn token_at(&self, offset: usize) -> Option<(usize, &Token)> {
        self.tokens
            .iter()
            .enumerate()
            .find(|(_, token)| token.span.contains(offset))
            .or_else(|| {
                offset.checked_sub(1).and_then(|previous| {
                    self.tokens
                        .iter()
                        .enumerate()
                        .find(|(_, token)| token.span.contains(previous))
                })
            })
    }

    pub fn symbol_named_before(&self, name: &str, offset: usize) -> Option<&Symbol> {
        self.symbols
            .iter()
            .filter(|symbol| symbol.name == name && symbol.span.start <= offset)
            .max_by_key(|symbol| symbol.span.start)
            .or_else(|| self.symbols.iter().find(|symbol| symbol.name == name))
    }
}

fn function_symbol(source: &str, tokens: &[Token], index: usize) -> Option<Symbol> {
    let name = tokens.get(index + 1).filter(|token| is_identifier(token))?;
    let mut cursor = index + 2;
    let mut paren_depth = 0usize;
    let mut end = name.span.end;
    while let Some(token) = tokens.get(cursor) {
        match token.text.as_str() {
            "(" => paren_depth += 1,
            ")" => paren_depth = paren_depth.saturating_sub(1),
            "{" | ";" if paren_depth == 0 => break,
            _ => {}
        }
        end = token.span.end;
        cursor += 1;
    }
    let start = declaration_prefix_start(tokens, index);
    Some(Symbol {
        name: name.text.clone(),
        kind: SymbolKind::Function,
        span: name.span,
        declaration_span: Span { start, end },
        detail: normalize_whitespace(&source[start..end]),
    })
}

fn declaration_prefix_start(tokens: &[Token], f_index: usize) -> usize {
    if f_index > 0 && matches!(tokens[f_index - 1].text.as_str(), "extern" | "cextern" | "c") {
        tokens[f_index - 1].span.start
    } else {
        tokens[f_index].span.start
    }
}

fn extern_constant_symbol(source: &str, tokens: &[Token], index: usize) -> Option<Symbol> {
    let mut cursor = index + 1;
    let mut equals = None;
    while let Some(token) = tokens.get(cursor) {
        if token.text == "=" {
            equals = Some(cursor);
            break;
        }
        if matches!(token.text.as_str(), ";" | "{" | "}") {
            break;
        }
        cursor += 1;
    }
    let equals = equals?;
    let name = tokens[index + 1..equals]
        .iter()
        .rev()
        .find(|token| is_identifier(token))?;
    let end = name.span.end;
    Some(Symbol {
        name: name.text.clone(),
        kind: SymbolKind::Constant,
        span: name.span,
        declaration_span: Span {
            start: tokens[index].span.start,
            end,
        },
        detail: normalize_whitespace(&source[tokens[index].span.start..end]),
    })
}

fn collect_parameters_and_locals(source: &str, tokens: &[Token], symbols: &mut Vec<Symbol>) {
    let mut index = 0usize;
    while index < tokens.len() {
        if matches!(tokens[index].text.as_str(), "f" | "fn" | "function") {
            if let Some(open) = tokens[index + 1..]
                .iter()
                .position(|token| token.text == "(")
                .map(|relative| index + 1 + relative)
            {
                if let Some(close) = matching_token(tokens, open, "(", ")") {
                    let mut chunk_start = open + 1;
                    for cursor in open + 1..=close {
                        if cursor == close || tokens[cursor].text == "," {
                            add_binding_from_chunk(source, &tokens[chunk_start..cursor], symbols);
                            chunk_start = cursor + 1;
                        }
                    }
                    index = close;
                }
            }
        } else if looks_like_type_start(&tokens[index].text)
            || (matches!(tokens[index].text.as_str(), "ref" | "link")
                && tokens
                    .get(index + 1)
                    .is_some_and(|t| looks_like_type_start(&t.text)))
        {
            let end = tokens[index..]
                .iter()
                .position(|token| matches!(token.text.as_str(), "=" | ";"))
                .map(|relative| index + relative);
            if let Some(end) = end.filter(|end| *end > index && *end - index < 12) {
                add_binding_from_chunk(source, &tokens[index..end], symbols);
            }
        }
        index += 1;
    }
}

fn add_binding_from_chunk(source: &str, chunk: &[Token], symbols: &mut Vec<Symbol>) {
    let Some(name) = chunk.iter().rev().find(|token| is_identifier(token)) else {
        return;
    };
    if is_type_name(&name.text) || is_keyword(&name.text) {
        return;
    }
    let start = chunk
        .first()
        .map_or(name.span.start, |token| token.span.start);
    symbols.push(Symbol {
        name: name.text.clone(),
        kind: SymbolKind::Variable,
        span: name.span,
        declaration_span: Span {
            start,
            end: name.span.end,
        },
        detail: normalize_whitespace(&source[start..name.span.end]),
    });
}

fn matching_token(tokens: &[Token], open: usize, left: &str, right: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().skip(open) {
        if token.text == left {
            depth += 1;
        } else if token.text == right {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(index);
            }
        }
    }
    None
}

fn include_name(tokens: &[Token], index: usize) -> Option<String> {
    let name = tokens.get(index).filter(|token| is_identifier(token))?;
    Some(name.text.clone())
}

fn is_identifier(token: &&Token) -> bool {
    token.kind == TokenKind::Identifier
}

pub fn lex(source: &str) -> Vec<Token> {
    let bytes = source.as_bytes();
    let mut tokens = Vec::new();
    let mut index = 0usize;
    while index < bytes.len() {
        if bytes[index].is_ascii_whitespace() {
            index += 1;
            continue;
        }
        if bytes[index..].starts_with(b"//") {
            index += 2;
            while index < bytes.len() && bytes[index] != b'\n' {
                index += 1;
            }
            continue;
        }
        if bytes[index..].starts_with(b"/*") {
            index = skip_block_comment(bytes, index);
            continue;
        }

        let start = index;
        let kind = if bytes[index] == b'"' {
            index += 1;
            while index < bytes.len() {
                if bytes[index] == b'\\' {
                    index = (index + 2).min(bytes.len());
                } else if bytes[index] == b'"' {
                    index += 1;
                    break;
                } else {
                    index += 1;
                }
            }
            TokenKind::String
        } else if bytes[index] == b'#' {
            index += 1;
            while index < bytes.len() && is_ident_continue(bytes[index]) {
                index += 1;
            }
            TokenKind::Directive
        } else if is_ident_start(bytes[index]) {
            index += 1;
            while index < bytes.len() && is_ident_continue(bytes[index]) {
                index += 1;
            }
            TokenKind::Identifier
        } else if bytes[index].is_ascii_digit() {
            index += 1;
            while index < bytes.len()
                && (bytes[index].is_ascii_alphanumeric() || matches!(bytes[index], b'_' | b'.'))
            {
                index += 1;
            }
            TokenKind::Number
        } else {
            index += operator_len(&bytes[index..]);
            TokenKind::Symbol
        };
        tokens.push(Token {
            kind,
            text: source[start..index].to_string(),
            span: Span { start, end: index },
        });
    }
    tokens
}

fn skip_block_comment(bytes: &[u8], mut index: usize) -> usize {
    let mut depth = 0usize;
    while index < bytes.len() {
        if bytes[index..].starts_with(b"/*") {
            depth += 1;
            index += 2;
        } else if bytes[index..].starts_with(b"*/") {
            depth = depth.saturating_sub(1);
            index += 2;
            if depth == 0 {
                break;
            }
        } else {
            index += 1;
        }
    }
    index
}

fn operator_len(bytes: &[u8]) -> usize {
    const TWO_CHAR: [&[u8]; 12] = [
        b"==", b"!=", b"<=", b">=", b"&&", b"||", b"<<", b"^^", b"::", b"->", b"+=", b"-=",
    ];
    if TWO_CHAR.iter().any(|operator| bytes.starts_with(operator)) {
        2
    } else {
        1
    }
}

fn is_ident_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_ident_continue(byte: u8) -> bool {
    is_ident_start(byte) || byte.is_ascii_digit()
}

fn normalize_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

pub fn is_keyword(word: &str) -> bool {
    KEYWORDS.contains(&word)
}

pub fn is_type_name(word: &str) -> bool {
    TYPES.contains(&word)
        || word
            .strip_prefix("int")
            .is_some_and(|width| !width.is_empty() && width.chars().all(|c| c.is_ascii_digit()))
        || word
            .strip_prefix("uint")
            .is_some_and(|width| !width.is_empty() && width.chars().all(|c| c.is_ascii_digit()))
        || word
            .strip_prefix("dec")
            .is_some_and(|width| !width.is_empty() && width.chars().all(|c| c.is_ascii_digit()))
}

fn looks_like_type_start(word: &str) -> bool {
    is_type_name(word) || (!is_keyword(word) && word.chars().next().is_some_and(char::is_uppercase))
}

pub fn keyword_doc(word: &str) -> Option<&'static str> {
    match word {
        "f" | "fn" | "function" => Some("**f/fn/function** — declares a function or closure."),
        "if" => Some("**if** — conditionally executes a branch."),
        "else" => Some("**else** — alternative branch of an `if`."),
        "switch" | "match" => {
            Some("**switch/match** — dispatches to `case` arms and an optional `default`.")
        }
        "case" => Some("**case** — a value arm inside `switch` or `match`."),
        "default" => Some("**default** — fallback arm inside `switch` or `match`."),
        "while" => Some("**while** — repeats while a condition is true."),
        "for" => Some("**for** — C-style `for (init; condition; step)` loop, or integer range loop: `for i in start..end` (inclusive) / `for i in start..<end` (exclusive)."),
        "return" => Some("**return** — exits the current function, optionally with a value."),
        "struct" => Some("**struct** — declares a garbage-collected Xi record type."),
        "cstruct" => Some("**cstruct** — declares a fixed-layout record for C interop."),
        "ref" => Some("**ref** — passes or binds a mutable reference."),
        "link" => Some("**link** — explicitly aliases another value."),
        "as" => Some("**as** — explicitly converts a value to another type."),
        "extern" => Some("**extern** — binds an Xi declaration to a runtime symbol."),
        "cextern" => Some("**cextern** — binds a declaration using the strict C ABI."),
        "c" => Some("**c** — introduces an inline C function or C preamble."),
        "preamble" => Some("**c preamble** — embeds C headers, macros, and helper declarations."),
        "intrinsic" => {
            Some("**intrinsic** — declares a module operation implemented by the compiler.")
        }
        "#include" => {
            Some("**#include** — imports a standard module or a sibling `.xi` source module.")
        }
        "#link" => Some("**#link** — links a native library into the generated executable."),
        "#subsystem" => {
            Some("**#subsystem** — selects the Windows PE subsystem (`console` or `windows`).")
        }
        "int" => Some("**int** — adaptive signed integer; `intN` is a fixed-width signed integer."),
        "uint" => Some("**uint** — adaptive unsigned integer; `uintN` is fixed-width."),
        "dec" => Some(
            "**dec** — adaptive decimal number; sized forms such as `dec32` are floating point.",
        ),
        "string" => Some("**string** — UTF-8 text."),
        "bool" => Some("**bool** — `true` or `false`."),
        "array" => Some("**array<T>** — a growable typed sequence."),
        "map" => Some("**map<K, V>** — a typed-key hash map."),
        "cptr" => Some("**cptr** — an opaque pointer to manually managed C memory."),
        "void" => Some("**void** — an extern function return type with no value."),
        "true" | "false" => Some("**bool literal**"),
        _ => None,
    }
}

pub const KEYWORDS: &[&str] = &[
    "f",
    "fn",
    "function",
    "if",
    "else",
    "switch",
    "match",
    "case",
    "default",
    "while",
    "for",
    "return",
    "struct",
    "cstruct",
    "ref",
    "link",
    "as",
    "extern",
    "cextern",
    "c",
    "preamble",
    "intrinsic",
    "true",
    "false",
    "#include",
    "#link",
    "#subsystem",
];

pub const TYPES: &[&str] = &[
    "int", "uint", "dec", "string", "bool", "array", "map", "cptr", "void",
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexes_current_declarations() {
        let source = r#"
#include io
cstruct POINT { int32 x; int32 y; }
cextern f get_pos(cptr out) -> int32 = "GetPos";
c f add(int32 a, int32 b) -> int32 { return a + b; }
extern dec pi = "xi_math_pi";
intrinsic alloc;
f main(int count) { int value = count; }
"#;
        let index = DocumentIndex::new(source);
        assert!(index.includes.contains(&"io".to_string()));
        for expected in ["POINT", "get_pos", "add", "pi", "alloc", "main", "count", "value"] {
            assert!(
                index.symbols.iter().any(|symbol| symbol.name == expected),
                "missing {expected}"
            );
        }
    }
}
