use serde::{Deserialize, Serialize};

use crate::Positioned;

#[derive(Debug, Default, Deserialize, Serialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpanRange {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
}

impl SpanRange {
    #[must_use]
    pub const fn new(start_line: u32, start_column: u32, end_line: u32, end_column: u32) -> Self {
        Self {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }

    #[must_use]
    pub const fn from_single(line: u32, column: u32) -> Self {
        Self {
            start_line: line,
            start_column: column,
            end_line: line,
            end_column: column,
        }
    }

    pub const fn add_lines(&mut self, count: u32) {
        self.start_line += count;
        self.end_line += count;
    }

    pub const fn add_columns(&mut self, count: u32) {
        self.start_column += count;
        self.end_column += count;
    }

    pub const fn set_column(&mut self, value: u32) {
        self.start_column = value;
        self.end_column = value;
    }

    #[must_use]
    pub const fn between(&self, other: Self) -> Self {
        Self {
            start_line: self.start_line,
            start_column: self.start_column,
            end_line: other.end_line,
            end_column: other.end_column,
        }
    }
}

#[derive(Debug, Default, Deserialize, Serialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub range: SpanRange,
}

impl Span {
    #[must_use]
    pub const fn new(start: usize, end: usize, range: SpanRange) -> Self {
        Self { start, end, range }
    }

    #[must_use]
    pub const fn between(&self, to: Self) -> Self {
        Self {
            start: self.start,
            end: to.end,
            range: self.range.between(to.range),
        }
    }

    pub const fn wrap<T>(self, value: T) -> Positioned<T> {
        Positioned { value, span: self }
    }

    pub const fn contains_line(&self, line: u32) -> bool {
        line >= self.range.start_line && line <= self.range.end_line
    }

    pub const fn contains(&self, line: u32, column: u32) -> bool {
        if line == self.range.start_line {
            column >= self.range.start_column
        } else if line == self.range.end_line {
            column <= self.range.end_line
        } else {
            line > self.range.start_line && line < self.range.end_line
        }
    }
}
