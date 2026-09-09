use tower_lsp::lsp_types::{Position, Range};

use crate::language::Span;

pub struct LineIndex<'a> {
    line_starts: Vec<usize>,
    source: &'a str,
}

impl<'a> LineIndex<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut line_starts = vec![0];
        for (offset, byte) in source.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(offset + 1);
            }
        }
        Self {
            line_starts,
            source,
        }
    }

    pub fn position(&self, offset: usize) -> Position {
        let offset = floor_char_boundary(self.source, offset.min(self.source.len()));
        let line = match self.line_starts.binary_search(&offset) {
            Ok(exact) => exact,
            Err(next) => next.saturating_sub(1),
        };
        let line_start = self.line_starts[line];
        let character = self.source[line_start..offset]
            .chars()
            .map(char::len_utf16)
            .sum::<usize>();
        Position::new(line as u32, character as u32)
    }

    pub fn range(&self, span: Span) -> Range {
        Range::new(self.position(span.start), self.position(span.end))
    }

    pub fn offset(&self, position: Position) -> usize {
        let line = position.line as usize;
        if line >= self.line_starts.len() {
            return self.source.len();
        }
        let line_start = self.line_starts[line];
        let line_end = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(self.source.len());
        let target = position.character as usize;
        let mut utf16_seen = 0;
        for (byte_offset, ch) in self.source[line_start..line_end].char_indices() {
            if utf16_seen >= target {
                return line_start + byte_offset;
            }
            utf16_seen += ch.len_utf16();
        }
        line_end
    }
}

fn floor_char_boundary(source: &str, mut offset: usize) -> usize {
    while offset > 0 && !source.is_char_boundary(offset) {
        offset -= 1;
    }
    offset
}
