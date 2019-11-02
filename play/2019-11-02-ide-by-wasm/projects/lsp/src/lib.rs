#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

mod lsp;

#[no_mangle]
pub extern "C" fn main() {
    lsp::lsp_main::start_lsp_server();
}
