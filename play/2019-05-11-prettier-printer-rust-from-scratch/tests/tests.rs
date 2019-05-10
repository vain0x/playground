use prettier_printer::*;

#[test]
fn test_nil() {
    let config = PrinterConfig::new();
    let mut printer = Printer::new(config);
    assert_eq!(printer.pretty(), "");
}
