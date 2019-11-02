pub(super) mod features;
pub(super) mod lsp_handler;
pub(super) mod lsp_model;

use serde::{Deserialize, Serialize};

pub(super) use lsp_handler::LspHandler;
pub(self) use lsp_model::LspModel;

#[derive(Serialize, Deserialize)]
pub(super) struct LspRequest<Params> {
    pub jsonrpc: String,
    pub id: i64,
    pub method: String,
    pub params: Params,
}

#[derive(Serialize, Deserialize)]
pub(super) struct LspResponse<Result> {
    pub jsonrpc: String,
    pub id: i64,
    pub result: Result,
}

#[derive(Serialize, Deserialize)]
pub(super) struct LspNotification<Params> {
    pub jsonrpc: String,
    pub method: String,
    pub params: Params,
}

/// LSP message (request or notification) without results/params
/// just for deserialization.
#[derive(Deserialize)]
pub(super) struct LspMessageOpaque {
    pub method: String,
}
