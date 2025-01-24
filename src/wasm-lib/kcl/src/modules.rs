use std::{fmt, path::PathBuf};

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::{
    errors::{KclError, KclErrorDetails},
    execution::PreImportedGeometry,
    fs::{FileManager, FileSystem},
    parsing::ast::types::{ImportPath, Node, Program},
    source_range::SourceRange,
    ModuleId,
};

pub(crate) fn read_std(mod_name: &str) -> Option<&'static str> {
    match mod_name {
        "prelude" => Some(include_str!("../std/prelude.kcl")),
        "math" => Some(include_str!("../std/math.kcl")),
        _ => None,
    }
}

pub(crate) fn std_fn(fn_name: &str) -> crate::std::StdFn {
    // TODO auto-generate this list
    match fn_name {
        "cos" => |e, a| Box::pin(crate::std::math::cos(e, a)),
        "sin" => |e, a| Box::pin(crate::std::math::sin(e, a)),
        "tan" => |e, a| Box::pin(crate::std::math::tan(e, a)),
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ModuleLoader {
    /// The stack of import statements for detecting circular module imports.
    /// If this is empty, we're not currently executing an import statement.
    pub import_stack: Vec<PathBuf>,
}

impl ModuleLoader {
    pub(crate) fn cycle_check(&self, path: &ModulePath, source_range: SourceRange) -> Result<(), KclError> {
        if self.import_stack.contains(path.expect_path()) {
            return Err(KclError::ImportCycle(KclErrorDetails {
                message: format!(
                    "circular import of modules is not allowed: {} -> {}",
                    self.import_stack
                        .iter()
                        .map(|p| p.as_path().to_string_lossy())
                        .collect::<Vec<_>>()
                        .join(" -> "),
                    path,
                ),
                source_ranges: vec![source_range],
            }));
        }
        Ok(())
    }

    pub(crate) fn enter_module(&mut self, path: &ModulePath) {
        if let ModulePath::Local(ref path) = path {
            self.import_stack.push(path.clone());
        }
    }

    pub(crate) fn leave_module(&mut self, path: &ModulePath) {
        if let ModulePath::Local(ref path) = path {
            let popped = self.import_stack.pop().unwrap();
            assert_eq!(path, &popped);
        }
    }
}

/// Info about a module.  Right now, this is pretty minimal.  We hope to cache
/// modules here in the future.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct ModuleInfo {
    /// The ID of the module.
    pub(crate) id: ModuleId,
    /// Absolute path of the module's source file.
    pub(crate) path: ModulePath,
    pub(crate) repr: ModuleRepr,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum ModuleRepr {
    Root,
    Kcl(Node<Program>),
    Foreign(PreImportedGeometry),
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, Eq, PartialEq, Deserialize, Serialize, Hash)]
pub enum ModulePath {
    Local(PathBuf),
    Std(String),
}

impl ModulePath {
    pub(crate) fn expect_path(&self) -> &PathBuf {
        match self {
            ModulePath::Local(p) => p,
            _ => unreachable!(),
        }
    }

    pub(crate) async fn source(&self, fs: &FileManager, source_range: SourceRange) -> Result<String, KclError> {
        match self {
            ModulePath::Local(p) => fs.read_to_string(p, source_range).await,
            ModulePath::Std(name) => read_std(name)
                .ok_or_else(|| {
                    KclError::Semantic(KclErrorDetails {
                        message: format!("Cannot find standard library module to import: std::{name}."),
                        source_ranges: vec![source_range],
                    })
                })
                .map(str::to_owned),
        }
    }

    pub(crate) fn from_import_path(path: &ImportPath, project_directory: &Option<PathBuf>) -> Self {
        match path {
            ImportPath::Kcl { filename: path } | ImportPath::Foreign { path } => {
                let resolved_path = if let Some(project_dir) = project_directory {
                    project_dir.join(path)
                } else {
                    std::path::PathBuf::from(path)
                };
                ModulePath::Local(resolved_path)
            }
            ImportPath::Std { path } => {
                // For now we only support importing from singly-nested modules inside std.
                assert_eq!(path.len(), 2);
                assert_eq!(&path[0], "std");

                ModulePath::Std(path[1].clone())
            }
        }
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModulePath::Local(path) => path.display().fmt(f),
            ModulePath::Std(s) => write!(f, "std::{s}"),
        }
    }
}
