use crate::lsp::*;
use lsp_types::*;

#[derive(Default)]
pub(crate) struct LspHandler {
    model: LspModel,
    publish_diagnostics: Vec<PublishDiagnosticsParams>,
}

impl LspHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn initialize(&mut self, _params: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::Full),
                        ..TextDocumentSyncOptions::default()
                    },
                )),
                ..ServerCapabilities::default()
            },
        }
    }

    pub(crate) fn did_initialize(&self) {
        // Pass
    }

    pub(crate) fn shutdown(&mut self) {
        // Pass
    }

    pub(crate) fn did_exit(&mut self) {
        std::process::exit(0)
    }

    pub(crate) fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        let uri = doc.uri.to_owned();
        self.model.open_doc(doc.uri, doc.version, doc.text);

        self.text_document_did_open_or_change(&uri);
    }

    pub(crate) fn text_document_did_change(&mut self, params: DidChangeTextDocumentParams) {
        let text = (params.content_changes.into_iter())
            .next()
            .map(|c| c.text)
            .unwrap_or("".to_string());

        let doc = params.text_document;
        let uri = doc.uri.to_owned();
        let version = doc.version.unwrap_or(0);

        self.model.change_doc(doc.uri, version, text);

        self.text_document_did_open_or_change(&uri);
    }

    pub(crate) fn text_document_did_open_or_change(&mut self, uri: &Url) {
        let diagnostics = self.model.validate(uri);

        self.publish_diagnostics.push(PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics,
        });
    }

    pub(crate) fn text_document_did_close(&mut self, params: DidCloseTextDocumentParams) {
        self.model.close_doc(&params.text_document.uri);
    }

    pub(crate) fn publish_diagnostics<'a>(
        &'a mut self,
    ) -> impl IntoIterator<Item = PublishDiagnosticsParams> + 'a {
        self.publish_diagnostics.drain(..)
    }
}
