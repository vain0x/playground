#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

mod lsp;

use std::sync::Mutex;
use wasm_bindgen::prelude::*;

type Params = JsValue;

struct JsLogger;

impl log::Log for JsLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Debug
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            error(&format!("{} {}", record.level(), record.args()));
        }
    }

    fn flush(&self) {
        //
    }
}

lazy_static! {
    static ref HANDLER: Mutex<lsp::LspHandler> = Default::default();
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn error(msg: &str);

    #[wasm_bindgen(js_namespace = lsp_connection)]
    fn publish_diagnostics(params: JsValue);
}

#[wasm_bindgen]
pub fn init_logger() {
    let log_level = if cfg!(debug_assertions) {
        log::LevelFilter::Debug
    } else {
        log::LevelFilter::Warn
    };

    log::set_max_level(log_level);
    log::set_logger(&JsLogger).unwrap();

    debug!("Logger initialized.");
}

#[wasm_bindgen]
pub fn lsp_request(method: String, params: Params) -> Params {
    let mut h = HANDLER.lock().unwrap();

    match method.as_str() {
        "initialize" => Params::from_serde(&h.initialize(params.into_serde().unwrap())).unwrap(),
        _ => {
            warn!("Unknown request {}", method);
            JsValue::NULL
        }
    }
}

#[wasm_bindgen]
pub fn lsp_notification(method: String, params: Params) {
    let mut h = HANDLER.lock().unwrap();

    match method.as_str() {
        // "initialized" => h.did_initialize(params.into_serde().unwrap()),
        "textDocument/didOpen" => h.text_document_did_open(params.into_serde().unwrap()),
        "textDocument/didChange" => h.text_document_did_change(params.into_serde().unwrap()),
        "textDocument/didClose" => h.text_document_did_close(params.into_serde().unwrap()),
        _ => {
            warn!("Unknown notification '{}'", method);
        }
    }

    for d in h.publish_diagnostics() {
        publish_diagnostics(JsValue::from_serde(&d).unwrap());
    }
}
