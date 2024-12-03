use serde::{Deserialize, Serialize};
use thiserror::Error;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::{
    lsp::IntoDiagnostic,
    source_range::{ModuleId, SourceRange},
};

/// How did the KCL execution fail
#[derive(thiserror::Error, Debug)]
pub enum ExecError {
    #[error("{0}")]
    Kcl(#[from] crate::KclError),
    #[error("Could not connect to engine: {0}")]
    Connection(#[from] ConnectionError),
    #[error("PNG snapshot could not be decoded: {0}")]
    BadPng(String),
}

/// How did KCL client fail to connect to the engine
#[derive(thiserror::Error, Debug)]
pub enum ConnectionError {
    #[error("Could not create a Zoo client: {0}")]
    CouldNotMakeClient(anyhow::Error),
    #[error("Could not establish connection to engine: {0}")]
    Establishing(anyhow::Error),
}

#[derive(Error, Debug, Serialize, Deserialize, ts_rs::TS, Clone, PartialEq, Eq)]
#[ts(export)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum KclError {
    #[error("lexical: {0:?}")]
    Lexical(KclErrorDetails),
    #[error("syntax: {0:?}")]
    Syntax(KclErrorDetails),
    #[error("semantic: {0:?}")]
    Semantic(KclErrorDetails),
    #[error("import cycle: {0:?}")]
    ImportCycle(KclErrorDetails),
    #[error("type: {0:?}")]
    Type(KclErrorDetails),
    #[error("unimplemented: {0:?}")]
    Unimplemented(KclErrorDetails),
    #[error("unexpected: {0:?}")]
    Unexpected(KclErrorDetails),
    #[error("value already defined: {0:?}")]
    ValueAlreadyDefined(KclErrorDetails),
    #[error("undefined value: {0:?}")]
    UndefinedValue(KclErrorDetails),
    #[error("invalid expression: {0:?}")]
    InvalidExpression(KclErrorDetails),
    #[error("engine: {0:?}")]
    Engine(KclErrorDetails),
    #[error("internal error, please report to KittyCAD team: {0:?}")]
    Internal(KclErrorDetails),
}

#[derive(thiserror::Error, Debug)]
#[error("{}", self.error.get_message())]
pub struct Report {
    pub error: KclError,
    pub kcl_source: String,
    pub filename: String,
}

impl miette::Diagnostic for Report {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        let family = match self.error {
            KclError::Lexical(_) => "Lexical",
            KclError::Syntax(_) => "Syntax",
            KclError::Semantic(_) => "Semantic",
            KclError::ImportCycle(_) => "ImportCycle",
            KclError::Type(_) => "Type",
            KclError::Unimplemented(_) => "Unimplemented",
            KclError::Unexpected(_) => "Unexpected",
            KclError::ValueAlreadyDefined(_) => "ValueAlreadyDefined",
            KclError::UndefinedValue(_) => "UndefinedValue",
            KclError::InvalidExpression(_) => "InvalidExpression",
            KclError::Engine(_) => "Engine",
            KclError::Internal(_) => "Internal",
        };
        let error_string = format!("KCL {family} error");
        Some(Box::new(error_string))
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.kcl_source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        let iter = self
            .error
            .source_ranges()
            .clone()
            .into_iter()
            .map(miette::SourceSpan::from)
            .map(|span| miette::LabeledSpan::new_with_span(None, span));
        Some(Box::new(iter))
    }
}

#[derive(Debug, Serialize, Deserialize, ts_rs::TS, Clone, PartialEq, Eq, thiserror::Error, miette::Diagnostic)]
#[error("{message}")]
#[ts(export)]
pub struct KclErrorDetails {
    #[serde(rename = "sourceRanges")]
    #[label(collection, "Errors")]
    pub source_ranges: Vec<SourceRange>,
    #[serde(rename = "msg")]
    pub message: String,
}

impl KclError {
    pub fn internal(message: String) -> KclError {
        KclError::Internal(KclErrorDetails {
            source_ranges: Default::default(),
            message,
        })
    }

    /// Get the error message.
    pub fn get_message(&self) -> String {
        format!("{}: {}", self.error_type(), self.message())
    }

    pub fn error_type(&self) -> &'static str {
        match self {
            KclError::Lexical(_) => "lexical",
            KclError::Syntax(_) => "syntax",
            KclError::Semantic(_) => "semantic",
            KclError::ImportCycle(_) => "import cycle",
            KclError::Type(_) => "type",
            KclError::Unimplemented(_) => "unimplemented",
            KclError::Unexpected(_) => "unexpected",
            KclError::ValueAlreadyDefined(_) => "value already defined",
            KclError::UndefinedValue(_) => "undefined value",
            KclError::InvalidExpression(_) => "invalid expression",
            KclError::Engine(_) => "engine",
            KclError::Internal(_) => "internal",
        }
    }

    pub fn source_ranges(&self) -> Vec<SourceRange> {
        match &self {
            KclError::Lexical(e) => e.source_ranges.clone(),
            KclError::Syntax(e) => e.source_ranges.clone(),
            KclError::Semantic(e) => e.source_ranges.clone(),
            KclError::ImportCycle(e) => e.source_ranges.clone(),
            KclError::Type(e) => e.source_ranges.clone(),
            KclError::Unimplemented(e) => e.source_ranges.clone(),
            KclError::Unexpected(e) => e.source_ranges.clone(),
            KclError::ValueAlreadyDefined(e) => e.source_ranges.clone(),
            KclError::UndefinedValue(e) => e.source_ranges.clone(),
            KclError::InvalidExpression(e) => e.source_ranges.clone(),
            KclError::Engine(e) => e.source_ranges.clone(),
            KclError::Internal(e) => e.source_ranges.clone(),
        }
    }

    /// Get the inner error message.
    pub fn message(&self) -> &str {
        match &self {
            KclError::Lexical(e) => &e.message,
            KclError::Syntax(e) => &e.message,
            KclError::Semantic(e) => &e.message,
            KclError::ImportCycle(e) => &e.message,
            KclError::Type(e) => &e.message,
            KclError::Unimplemented(e) => &e.message,
            KclError::Unexpected(e) => &e.message,
            KclError::ValueAlreadyDefined(e) => &e.message,
            KclError::UndefinedValue(e) => &e.message,
            KclError::InvalidExpression(e) => &e.message,
            KclError::Engine(e) => &e.message,
            KclError::Internal(e) => &e.message,
        }
    }

    pub fn override_source_ranges(&self, source_ranges: Vec<SourceRange>) -> Self {
        let mut new = self.clone();
        match &mut new {
            KclError::Lexical(e) => e.source_ranges = source_ranges,
            KclError::Syntax(e) => e.source_ranges = source_ranges,
            KclError::Semantic(e) => e.source_ranges = source_ranges,
            KclError::ImportCycle(e) => e.source_ranges = source_ranges,
            KclError::Type(e) => e.source_ranges = source_ranges,
            KclError::Unimplemented(e) => e.source_ranges = source_ranges,
            KclError::Unexpected(e) => e.source_ranges = source_ranges,
            KclError::ValueAlreadyDefined(e) => e.source_ranges = source_ranges,
            KclError::UndefinedValue(e) => e.source_ranges = source_ranges,
            KclError::InvalidExpression(e) => e.source_ranges = source_ranges,
            KclError::Engine(e) => e.source_ranges = source_ranges,
            KclError::Internal(e) => e.source_ranges = source_ranges,
        }

        new
    }

    pub fn add_source_ranges(&self, source_ranges: Vec<SourceRange>) -> Self {
        let mut new = self.clone();
        match &mut new {
            KclError::Lexical(e) => e.source_ranges.extend(source_ranges),
            KclError::Syntax(e) => e.source_ranges.extend(source_ranges),
            KclError::Semantic(e) => e.source_ranges.extend(source_ranges),
            KclError::ImportCycle(e) => e.source_ranges.extend(source_ranges),
            KclError::Type(e) => e.source_ranges.extend(source_ranges),
            KclError::Unimplemented(e) => e.source_ranges.extend(source_ranges),
            KclError::Unexpected(e) => e.source_ranges.extend(source_ranges),
            KclError::ValueAlreadyDefined(e) => e.source_ranges.extend(source_ranges),
            KclError::UndefinedValue(e) => e.source_ranges.extend(source_ranges),
            KclError::InvalidExpression(e) => e.source_ranges.extend(source_ranges),
            KclError::Engine(e) => e.source_ranges.extend(source_ranges),
            KclError::Internal(e) => e.source_ranges.extend(source_ranges),
        }

        new
    }
}

impl IntoDiagnostic for KclError {
    fn to_lsp_diagnostic(&self, code: &str) -> Diagnostic {
        let message = self.get_message();
        let source_ranges = self.source_ranges();

        // Limit to only errors in the top-level file.
        let module_id = ModuleId::default();
        let source_ranges = source_ranges
            .iter()
            .filter(|r| r.module_id() == module_id)
            .collect::<Vec<_>>();

        Diagnostic {
            range: source_ranges.first().map(|r| r.to_lsp_range(code)).unwrap_or_default(),
            severity: Some(self.severity()),
            code: None,
            // TODO: this is neat we can pass a URL to a help page here for this specific error.
            code_description: None,
            source: Some("kcl".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        }
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::ERROR
    }
}

/// This is different than to_string() in that it will serialize the Error
/// the struct as JSON so we can deserialize it on the js side.
impl From<KclError> for String {
    fn from(error: KclError) -> Self {
        serde_json::to_string(&error).unwrap()
    }
}

impl From<String> for KclError {
    fn from(error: String) -> Self {
        serde_json::from_str(&error).unwrap()
    }
}

#[cfg(feature = "pyo3")]
impl From<pyo3::PyErr> for KclError {
    fn from(error: pyo3::PyErr) -> Self {
        KclError::Internal(KclErrorDetails {
            source_ranges: vec![],
            message: error.to_string(),
        })
    }
}

#[cfg(feature = "pyo3")]
impl From<KclError> for pyo3::PyErr {
    fn from(error: KclError) -> Self {
        pyo3::exceptions::PyException::new_err(error.to_string())
    }
}
