use rustc_serialize::json;
use super::Span;

#[derive(Debug, RustcDecodable)]
pub struct CompilerMessageCode {
    pub code: String
}

#[derive(Debug, RustcDecodable)]
pub struct CompilerMessage {
    pub message: String,
    pub code: Option<CompilerMessageCode>,
    pub level: String,
    pub spans: Vec<Span>,
}

impl CompilerMessage {
    pub fn from_json_str(text: &str) -> Option<CompilerMessage> {
        json::decode(text).ok()
    }
}