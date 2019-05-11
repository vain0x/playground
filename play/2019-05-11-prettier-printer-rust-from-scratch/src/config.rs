use super::*;

impl PrintConfig {
    pub fn new() -> Self {
        Self {
            indent_size: 4,
            width: 80,
        }
    }

    pub fn with_indent_size(self, indent_size: usize) -> Self {
        Self {
            indent_size,
            ..self
        }
    }

    pub fn with_width(self, width: usize) -> Self {
        Self { width, ..self }
    }
}
