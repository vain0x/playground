use crate::lsp::features;
use lsp_types::*;
use std::collections::BTreeMap;

#[derive(Default)]
pub(super) struct LspModel {
    docs: BTreeMap<Url, String>,
}

impl LspModel {
    pub(super) fn open_doc(&mut self, uri: Url, _version: u64, text: String) {
        self.docs.insert(uri, text);
    }

    pub(super) fn change_doc(&mut self, uri: Url, version: u64, text: String) {
        self.open_doc(uri, version, text);
    }

    pub(super) fn close_doc(&mut self, uri: &Url) {
        self.docs.remove(&uri);
    }

    pub(super) fn validate(&mut self, uri: &Url) -> Vec<Diagnostic> {
        let text = match self.docs.get(uri) {
            None => {
                debug!("Doc {} is not compiled yet.", uri);
                return vec![];
            }
            Some(text) => text,
        };

        features::diagnostics::diagnostics(&text)
    }
}
