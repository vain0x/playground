use super::*;

impl PrinterConfig {
    pub fn new() -> Self {
        PrinterConfig {
            indent_size: 4,
            width: 80,
        }
    }

    pub fn with_indent_size(self, indent_size: usize) -> Self {
        PrinterConfig { indent_size, ..self }
    }

    pub fn with_width(self, width: usize) -> Self {
        PrinterConfig { width, ..self }
    }
}
