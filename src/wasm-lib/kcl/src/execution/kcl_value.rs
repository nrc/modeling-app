use std::collections::HashMap;

use anyhow::Result;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    errors::KclErrorDetails,
    exec::{ProgramMemory, Sketch},
    execution::{
        Face, Helix, ImportedGeometry, MemoryFunction, Metadata, Plane, SketchSet, Solid, SolidSet, TagIdentifier,
    },
    parsing::{
        ast::types::{
            DefaultParamVal, FunctionExpression, KclNone, Literal, LiteralValue, Node, TagDeclarator, TagNode,
        },
        token::NumericSuffix,
    },
    std::{args::Arg, FnAsArg},
    CompilationError, ExecState, ExecutorContext, KclError, ModuleId, SourceRange,
};

use super::EnvironmentRef;

pub type KclObjectFields = HashMap<String, KclValue>;

/// Any KCL value.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, ts_rs::TS, JsonSchema)]
#[ts(export)]
#[serde(tag = "type")]
pub enum KclValue {
    Uuid {
        value: ::uuid::Uuid,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    Bool {
        value: bool,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    Number {
        value: f64,
        ty: NumericType,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    Int {
        value: i64,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    String {
        value: String,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    Array {
        value: Vec<KclValue>,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    Object {
        value: KclObjectFields,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    TagIdentifier(Box<TagIdentifier>),
    TagDeclarator(crate::parsing::ast::types::BoxNode<TagDeclarator>),
    Plane(Box<Plane>),
    Face(Box<Face>),
    Sketch {
        value: Box<Sketch>,
    },
    Sketches {
        value: Vec<Box<Sketch>>,
    },
    Solid(Box<Solid>),
    Solids {
        value: Vec<Box<Solid>>,
    },
    Helix(Box<Helix>),
    ImportedGeometry(ImportedGeometry),
    #[ts(skip)]
    Function {
        /// Adam Chalmers speculation:
        /// Reference to a KCL stdlib function (written in Rust).
        /// Some if the KCL value is an alias of a stdlib function,
        /// None if it's a KCL function written/declared in KCL.
        #[serde(skip)]
        func: Option<MemoryFunction>,
        #[schemars(skip)]
        expression: crate::parsing::ast::types::BoxNode<FunctionExpression>,
        memory: Box<ProgramMemory>,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    Module {
        value: ModuleId,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
    KclNone {
        value: KclNone,
        #[serde(rename = "__meta")]
        meta: Vec<Metadata>,
    },
}

impl From<SketchSet> for KclValue {
    fn from(sg: SketchSet) -> Self {
        match sg {
            SketchSet::Sketch(value) => KclValue::Sketch { value },
            SketchSet::Sketches(value) => KclValue::Sketches { value },
        }
    }
}

impl From<Vec<Box<Sketch>>> for KclValue {
    fn from(sg: Vec<Box<Sketch>>) -> Self {
        KclValue::Sketches { value: sg }
    }
}

impl From<SolidSet> for KclValue {
    fn from(eg: SolidSet) -> Self {
        match eg {
            SolidSet::Solid(eg) => KclValue::Solid(eg),
            SolidSet::Solids(egs) => KclValue::Solids { value: egs },
        }
    }
}

impl From<Vec<Box<Solid>>> for KclValue {
    fn from(eg: Vec<Box<Solid>>) -> Self {
        if eg.len() == 1 {
            KclValue::Solid(eg[0].clone())
        } else {
            KclValue::Solids { value: eg }
        }
    }
}
impl From<KclValue> for Vec<SourceRange> {
    fn from(item: KclValue) -> Self {
        match item {
            KclValue::TagDeclarator(t) => vec![SourceRange::new(t.start, t.end, t.module_id)],
            KclValue::TagIdentifier(t) => to_vec_sr(&t.meta),
            KclValue::Solid(e) => to_vec_sr(&e.meta),
            KclValue::Solids { value } => value.iter().flat_map(|eg| to_vec_sr(&eg.meta)).collect(),
            KclValue::Sketch { value } => to_vec_sr(&value.meta),
            KclValue::Sketches { value } => value.iter().flat_map(|eg| to_vec_sr(&eg.meta)).collect(),
            KclValue::Helix(e) => to_vec_sr(&e.meta),
            KclValue::ImportedGeometry(i) => to_vec_sr(&i.meta),
            KclValue::Function { meta, .. } => to_vec_sr(&meta),
            KclValue::Plane(p) => to_vec_sr(&p.meta),
            KclValue::Face(f) => to_vec_sr(&f.meta),
            KclValue::Bool { meta, .. } => to_vec_sr(&meta),
            KclValue::Number { meta, .. } => to_vec_sr(&meta),
            KclValue::Int { meta, .. } => to_vec_sr(&meta),
            KclValue::String { meta, .. } => to_vec_sr(&meta),
            KclValue::Array { meta, .. } => to_vec_sr(&meta),
            KclValue::Object { meta, .. } => to_vec_sr(&meta),
            KclValue::Module { meta, .. } => to_vec_sr(&meta),
            KclValue::Uuid { meta, .. } => to_vec_sr(&meta),
            KclValue::KclNone { meta, .. } => to_vec_sr(&meta),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, ts_rs::TS, JsonSchema)]
#[ts(export)]
#[serde(tag = "type")]
pub enum NumericType {
    // Specified by the user (directly or indirectly)
    Known(NumericTypeCtor),
    // Unspecified, using defaults
    Default {
        len: UnitLen,
        angle: UnitAngle,
        src: Option<(String, EnvironmentRef)>,
    },
    // Exceeded the ability of the type system to track. The arg indicates the closest guess.
    Unknown(Option<NumericTypeCtor>),
    // Type info has been explicitly cast away.
    Any,
}

#[derive(Debug, Clone)]
struct NumericTypeAdjustment {
    pub result_type: NumericType,
    pub lhs: f64,
    pub rhs: f64,
    pub warning: Option<CompilationError>,
}

impl NumericTypeAdjustment {
    fn new(result_type: NumericType, lhs: f64, rhs: f64) -> Self {
        NumericTypeAdjustment {
            result_type,
            lhs,
            rhs,
            warning: None,
        }
    }

    fn with_warning(mut self, warning: CompilationError) -> Self {
        self.warning = Some(warning);
        self
    }
}

fn err(range: SourceRange) -> KclError {
    KclError::Semantic(KclErrorDetails {
        message: "Mismatched units of measure".to_owned(),
        source_ranges: vec![range],
    })
}

impl NumericType {
    pub fn degrees() -> Self {
        NumericType::Known(NumericTypeCtor::Angle(UnitAngle::Degrees))
    }

    pub fn radians() -> Self {
        NumericType::Known(NumericTypeCtor::Angle(UnitAngle::Radians))
    }

    pub fn mm() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitAngle::Mm))
    }

    pub fn cm() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitAngle::Cm))
    }

    pub fn m() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitAngle::M))
    }

    pub fn inches() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitAngle::Inches))
    }

    pub fn feet() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitAngle::Feet))
    }

    pub fn yards() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitAngle::Yards))
    }

    pub fn count() -> Self {
        NumericType::Known(NumericTypeCtor::Count)
    }

    pub fn internal_length() -> Self {
        NumericType::Known(NumericTypeCtor::Length(UnitLen::Mm))
    }

    pub fn internal_angle() -> Self {
        NumericType::Known(NumericTypeCtor::Angle(UnitAngle::Degrees))
    }

    // other is the type of a formal parameter or similar
    // treat default formal params like known ones
    // TODO call this for user-defined functions
    // TODO better error messages
    pub fn subtype(
        &self,
        other: &Self,
        value: f64,
        mem: &mut ProgramMemory,
        range: SourceRange,
    ) -> Result<NumericTypeAdjustment, KclError> {
        match (self, other) {
            (_, NumericType::Any) | (_, NumericType::Unknown(_)) => unreachable!(),
            (a, b) if a == b => Ok(NumericTypeAdjustment::new(other.clone(), value, 0.0)),
            (NumericType::Default { len: l1, angle: a1, .. }, NumericType::Default { len: l2, angle: a2, .. })
                if l1 == l2 && a1 == a2 =>
            {
                Ok(NumericTypeAdjustment::new(other.clone(), value, 0.0))
            }
            (NumericType::Any, a) => Ok(NumericTypeAdjustment::new(a.clone(), value, 0.0)),
            (NumericType::Unknown(_), _) => Err(err(range)),
            // We don't know what we'll end up with so we can't adjust the numeric value.
            (NumericType::Default { .. }, NumericType::Default { .. }) => Err(err(range)),
            (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Known(NumericTypeCtor::Length(b))) => {
                Ok(NumericTypeAdjustment::new(other.clone(), a.adjust_to(value, *b), 0.0))
            }
            (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Known(NumericTypeCtor::Angle(b))) => {
                Ok(NumericTypeAdjustment::new(other.clone(), a.adjust_to(value, *b), 0.0))
            }
            // Dimension mismatch
            (NumericType::Known(_), NumericType::Known(_)) => Err(err(range)),
            (NumericType::Known(NumericTypeCtor::Count), NumericType::Default { .. }) => Ok(
                NumericTypeAdjustment::new(NumericType::Known(NumericTypeCtor::Count), value, 0.0),
            ),
            (NumericType::Default { src, .. }, NumericType::Known(NumericTypeCtor::Count)) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Count);
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Count),
                    value,
                    0.0,
                ))
            }
            (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Default { len, .. }) => {
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Length(*len)),
                    a.adjust_to(value, *len),
                    0.0,
                ))
            }
            (NumericType::Default { len, src, .. }, NumericType::Known(NumericTypeCtor::Length(a))) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Length(*a));
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Length(*a)),
                    len.adjust_to(value, *a),
                    0.0,
                ))
            }
            (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Default { angle, .. }) => {
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Angle(*angle)),
                    a.adjust_to(value, *angle),
                    0.0,
                ))
            }
            (NumericType::Default { angle, src, .. }, NumericType::Known(NumericTypeCtor::Angle(a))) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Angle(*a));
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Angle(*a)),
                    angle.adjust_to(value, *a),
                    0.0,
                ))
            }
        }
    }

    // For addition, subtraction and other operations where the types should match.
    // TODO better error messages
    pub fn add(
        &self,
        other: &Self,
        lhs: f64,
        rhs: f64,
        mem: &mut ProgramMemory,
        range: SourceRange,
    ) -> Result<NumericTypeAdjustment, KclError> {
        match (self, other) {
            (a, b) if a == b => Ok(NumericTypeAdjustment::new(self.clone(), lhs, rhs)),
            (
                NumericType::Default {
                    len: l1,
                    angle: a1,
                    src: s1,
                },
                NumericType::Default {
                    len: l2,
                    angle: a2,
                    src: s2,
                },
            ) if l1 == l2 && a1 == a2 => {
                // We prioritiese preserving the source of self rather than other, but really we should be able to use both.
                Ok(NumericTypeAdjustment::new(
                    NumericType::Default {
                        len: *l1,
                        angle: *a1,
                        src: s1.clone().or(s2.clone()),
                    },
                    lhs,
                    rhs,
                ))
            }
            (NumericType::Any, a) | (a, NumericType::Any) => Ok(NumericTypeAdjustment::new(a.clone(), lhs, rhs)),
            (NumericType::Unknown(_), _) | (_, NumericType::Unknown(_)) => Err(err(range)),
            // We don't know what we'll end up with so we can't adjust the numeric value.
            (NumericType::Default { .. }, NumericType::Default { .. }) => Err(err(range)),
            (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Known(NumericTypeCtor::Length(b))) => {
                Ok(NumericTypeAdjustment::new(self.clone(), lhs, a.adjust_to(rhs, *b)))
            }
            (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Known(NumericTypeCtor::Angle(b))) => {
                Ok(NumericTypeAdjustment::new(self.clone(), lhs, a.adjust_to(rhs, *b)))
            }
            // Dimension mismatch
            (NumericType::Known(_), NumericType::Known(_)) => Err(err(range)),
            (NumericType::Known(NumericTypeCtor::Count), NumericType::Default { src, .. })
            | (NumericType::Default { src, .. }, NumericType::Known(NumericTypeCtor::Count)) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Count);
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Count),
                    lhs,
                    rhs,
                ))
            }
            (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Default { len, src, .. }) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Length(*a));
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Length(*a)),
                    lhs,
                    len.adjust_to(rhs, *a),
                ))
            }
            (NumericType::Default { len, src, .. }, NumericType::Known(NumericTypeCtor::Length(a))) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Length(*a));
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Length(*a)),
                    len.adjust_to(lhs, *a),
                    rhs,
                ))
            }
            (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Default { angle, src, .. }) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Angle(*a));
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Angle(*a)),
                    lhs,
                    angle.adjust_to(rhs, *a),
                ))
            }
            (NumericType::Default { angle, src, .. }, NumericType::Known(NumericTypeCtor::Angle(a))) => {
                if let Some((name, env)) = src {
                    mem.set_numeric_type(name, *env, NumericTypeCtor::Angle(*a));
                }
                Ok(NumericTypeAdjustment::new(
                    NumericType::Known(NumericTypeCtor::Angle(*a)),
                    angle.adjust_to(lhs, *a),
                    rhs,
                ))
            }
        }
    }

    // pub fn multiply(&self, other: &Self) ->  Result<Self, CompilationError> {
    //     match (self, other) {
    //         (NumericType::Known(NumericTypeCtor::Count), NumericType::Known(NumericTypeCtor::Count)) => Ok(NumericType::Known(NumericTypeCtor::Count)),
    //         (NumericType::Known(a), NumericType::Known(b)) if a == b => Ok(NumericType::Unknown(Some(a.clone()))),
    //         (a, b) if a == b => Ok(NumericType::Unknown(None)),
    //         (a, NumericType::Known(NumericTypeCtor::Count)) |
    //         (NumericType::Known(NumericTypeCtor::Count), a) => Ok(a.clone()),

    //         (NumericType::Any, a) | (a, NumericType::Any) => Ok(NumericType::Unknown(None)),
    //         (NumericType::Unknown(_), _) | (_, NumericType::Unknown(_)) => Ok(NumericType::Unknown(None)),
    //         // We don't know what we'll end up with so we can't adjust the numeric value.
    //         (NumericType::Default{ .. }, NumericType::Default { .. }) => Err(todo!()),

    //         (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Known(NumericTypeCtor::Length(b))) => {
    //             // TODO warn about mismatched units
    //             Ok(NumericType::Unknown(None))
    //         }
    //         (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Known(NumericTypeCtor::Angle(b))) => {
    //             // TODO warn about mismatched units
    //             Ok(NumericType::Unknown(None))
    //         }
    //         (NumericType::Known(_), NumericType::Known(_)) => Ok(NumericType::Unknown(None)),
    //         (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Default { .. }) |
    //         (NumericType::Default { .. }, NumericType::Known(NumericTypeCtor::Length(a))) => {
    //             // TODO assign the default to count
    //             Ok(NumericType::Known(NumericTypeCtor::Length(*a)))
    //         }
    //         (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Default { .. }) |
    //         (NumericType::Default { .. }, NumericType::Known(NumericTypeCtor::Angle(a))) => {
    //             // TODO assign the default to count
    //             Ok(NumericType::Known(NumericTypeCtor::Angle(*a)))
    //         }
    //     }
    // }

    // pub fn divide(&self, other: &Self) ->  Result<Self, CompilationError> {
    //     match (self, other) {
    //         (a, b) if a == b => Ok(NumericType::Known(NumericTypeCtor::Count)),
    //         (a, NumericType::Known(NumericTypeCtor::Count)) => Ok(a.clone()),
    //         (NumericType::Known(NumericTypeCtor::Count), _) => Ok(NumericType::Unknown(None)),
    //         (NumericType::Any, a) | (a, NumericType::Any) => Err(todo!()),
    //         (NumericType::Unknown(_), _) | (_, NumericType::Unknown(_)) => Err(todo!()),
    //         // We don't know what we'll end up with so we can't adjust the numeric value.
    //         (NumericType::Default{ .. }, NumericType::Default { .. }) => Err(todo!()),
    //         (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Known(NumericTypeCtor::Length(b))) => {
    //             // TODO Need to adjust the numeric value of other
    //             Ok(NumericType::Known(NumericTypeCtor::Count))
    //         }
    //         (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Known(NumericTypeCtor::Angle(b))) => {
    //             // TODO Need to adjust the numeric value of other
    //             Ok(NumericType::Known(NumericTypeCtor::Count))
    //         }
    //         (NumericType::Known(_), NumericType::Known(_)) => Ok(NumericType::Unknown(None)),
    //         (NumericType::Known(NumericTypeCtor::Length(a)), NumericType::Default { len, .. }) |
    //         (NumericType::Default { len, .. }, NumericType::Known(NumericTypeCtor::Length(a))) => {
    //             // TODO assign the default, possibly adjust the numeric value
    //             Ok(NumericType::Known(NumericTypeCtor::Count))
    //         }
    //         (NumericType::Known(NumericTypeCtor::Angle(a)), NumericType::Default { angle, .. }) |
    //         (NumericType::Default { angle, .. }, NumericType::Known(NumericTypeCtor::Angle(a))) => {
    //             // TODO assign the default, possibly adjust the numeric value
    //             Ok(NumericType::Known(NumericTypeCtor::Count))
    //         }
    //     }
    // }

    // pub fn assert(&self, other: &Self) -> Result<Self, CompilationError> {
    //     let NumericType::Known(ty) = other else {
    //         unreachable!()
    //     };

    //     match (self, ty) {
    //         (NumericType::Known(self_ty), ty) if self_ty == ty => Ok(self.clone()),
    //         (NumericType::Known(NumericTypeCtor::Length(_)), NumericTypeCtor::Length(_)) |
    //         (NumericType::Known(NumericTypeCtor::Angle(_)), NumericTypeCtor::Angle(_)) => {
    //             // TODO need to adjust the numeric value of self
    //             Ok(other.clone())
    //         }
    //         (NumericType::Default { .. }, NumericTypeCtor::Count) => Ok(other.clone()),
    //         (NumericType::Default { len, .. }, NumericTypeCtor::Length(_)) => {
    //             // TODO may need to adjust the numeric value of self
    //             Ok(other.clone())
    //         }
    //         (NumericType::Default { angle, .. }, NumericTypeCtor::Angle(_)) => {
    //             // TODO may need to adjust the numeric value of self
    //             Ok(other.clone())
    //         }
    //         (NumericType::Unknown(None), _) => Err(todo!()),
    //         (NumericType::Unknown(Some(self_ty)), ty) if self_ty == ty => Ok(other.clone()),
    //         (NumericType::Unknown(Some(NumericTypeCtor::Length(_))), NumericTypeCtor::Length(_)) |
    //         (NumericType::Unknown(Some(NumericTypeCtor::Angle(_))), NumericTypeCtor::Angle(_)) => {
    //             // TODO need to adjust the numeric value of self
    //             Ok(other.clone())
    //         }
    //         (NumericType::Any, _) => Ok(other.clone()),
    //         _ => Err(todo!())
    //     }
    // }

    pub fn from_parsed(suffix: NumericSuffix, settings: &super::MetaSettings) -> Self {
        match suffix {
            NumericSuffix::None => NumericType::Default {
                len: settings.default_length_units,
                angle: settings.default_angle_units,
                src: None,
            },
            NumericSuffix::Count => NumericType::Known(NumericTypeCtor::Count),
            NumericSuffix::Mm => NumericType::Known(NumericTypeCtor::Length(UnitLen::Mm)),
            NumericSuffix::Cm => NumericType::Known(NumericTypeCtor::Length(UnitLen::Cm)),
            NumericSuffix::M => NumericType::Known(NumericTypeCtor::Length(UnitLen::M)),
            NumericSuffix::Inch => NumericType::Known(NumericTypeCtor::Length(UnitLen::Inches)),
            NumericSuffix::Ft => NumericType::Known(NumericTypeCtor::Length(UnitLen::Feet)),
            NumericSuffix::Yd => NumericType::Known(NumericTypeCtor::Length(UnitLen::Yards)),
            NumericSuffix::Deg => NumericType::Known(NumericTypeCtor::Angle(UnitAngle::Degrees)),
            NumericSuffix::Rad => NumericType::Known(NumericTypeCtor::Angle(UnitAngle::Radians)),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, ts_rs::TS, JsonSchema, Eq)]
#[ts(export)]
#[serde(tag = "type")]
pub enum NumericTypeCtor {
    Count,
    Length(UnitLen),
    Angle(UnitAngle),
}

// TODO called UnitLen so as not to clash with UnitLength in settings)
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, ts_rs::TS, JsonSchema, Eq)]
#[ts(export)]
#[serde(tag = "type")]
pub enum UnitLen {
    Mm,
    Cm,
    M,
    Inches,
    Feet,
    Yards,
}

impl UnitLen {
    fn adjust_to(self, value: f64, to: UnitLen) -> f64 {
        if self == to {
            return value;
        }

        use UnitLen::*;
        let (base, base_unit) = match self {
            Mm => (value, Mm),
            Cm => (value * 10.0, Mm),
            M => (value * 1000.0, Mm),
            Inches => (value, Inches),
            Feet => (value * 12.0, Inches),
            Yards => (value * 36.0, Inches),
        };
        let (base, base_unit) = match (base_unit, to) {
            (Mm, Inches) | (Mm, Feet) | (Mm, Yards) => (base / 25.4, Inches),
            (Inches, Mm) | (Inches, Cm) | (Inches, M) => (base * 25.4, Mm),
            _ => (base, base_unit),
        };

        match (base_unit, to) {
            (Mm, Mm) => base,
            (Mm, Cm) => base / 10.0,
            (Mm, M) => base / 1000.0,
            (Inches, Inches) => base,
            (Inches, Feet) => base / 12.0,
            (Inches, Yards) => base / 36.0,
            _ => unreachable!(),
        }
    }
}

impl TryFrom<NumericSuffix> for UnitLen {
    type Error = ();

    fn try_from(suffix: NumericSuffix) -> std::result::Result<Self, Self::Error> {
        match suffix {
            NumericSuffix::Mm => Ok(Self::Mm),
            NumericSuffix::Cm => Ok(Self::Cm),
            NumericSuffix::M => Ok(Self::M),
            NumericSuffix::Inch => Ok(Self::Inches),
            NumericSuffix::Ft => Ok(Self::Feet),
            NumericSuffix::Yd => Ok(Self::Yards),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, ts_rs::TS, JsonSchema, Eq)]
#[ts(export)]
#[serde(tag = "type")]
pub enum UnitAngle {
    Degrees,
    Radians,
}

impl UnitAngle {
    fn adjust_to(self, value: f64, to: UnitAngle) -> f64 {
        use std::f64::consts::PI;
        use UnitAngle::*;
        match (self, to) {
            (Degrees, Degrees) => value,
            (Degrees, Radians) => (value / 180.0) * PI,
            (Radians, Degrees) => 180.0 * value / PI,
            (Radians, Radians) => value,
        }
    }
}

impl TryFrom<NumericSuffix> for UnitAngle {
    type Error = ();

    fn try_from(suffix: NumericSuffix) -> std::result::Result<Self, Self::Error> {
        match suffix {
            NumericSuffix::Deg => Ok(Self::Degrees),
            NumericSuffix::Rad => Ok(Self::Radians),
            _ => Err(()),
        }
    }
}

fn to_vec_sr(meta: &[Metadata]) -> Vec<SourceRange> {
    meta.iter().map(|m| m.source_range).collect()
}

impl From<&KclValue> for Vec<SourceRange> {
    fn from(item: &KclValue) -> Self {
        match item {
            KclValue::TagDeclarator(t) => vec![SourceRange::new(t.start, t.end, t.module_id)],
            KclValue::TagIdentifier(t) => to_vec_sr(&t.meta),
            KclValue::Solid(e) => to_vec_sr(&e.meta),
            KclValue::Solids { value } => value.iter().flat_map(|eg| to_vec_sr(&eg.meta)).collect(),
            KclValue::Sketch { value } => to_vec_sr(&value.meta),
            KclValue::Sketches { value } => value.iter().flat_map(|eg| to_vec_sr(&eg.meta)).collect(),
            KclValue::Helix(x) => to_vec_sr(&x.meta),
            KclValue::ImportedGeometry(i) => to_vec_sr(&i.meta),
            KclValue::Function { meta, .. } => to_vec_sr(meta),
            KclValue::Plane(p) => to_vec_sr(&p.meta),
            KclValue::Face(f) => to_vec_sr(&f.meta),
            KclValue::Bool { meta, .. } => to_vec_sr(meta),
            KclValue::Number { meta, .. } => to_vec_sr(meta),
            KclValue::Int { meta, .. } => to_vec_sr(meta),
            KclValue::String { meta, .. } => to_vec_sr(meta),
            KclValue::Uuid { meta, .. } => to_vec_sr(meta),
            KclValue::Array { meta, .. } => to_vec_sr(meta),
            KclValue::Object { meta, .. } => to_vec_sr(meta),
            KclValue::Module { meta, .. } => to_vec_sr(meta),
            KclValue::KclNone { meta, .. } => to_vec_sr(meta),
        }
    }
}

impl KclValue {
    pub(crate) fn metadata(&self) -> Vec<Metadata> {
        match self {
            KclValue::Uuid { value: _, meta } => meta.clone(),
            KclValue::Bool { value: _, meta } => meta.clone(),
            KclValue::Number { value: _, ty: _, meta } => meta.clone(),
            KclValue::Int { value: _, meta } => meta.clone(),
            KclValue::String { value: _, meta } => meta.clone(),
            KclValue::Array { value: _, meta } => meta.clone(),
            KclValue::Object { value: _, meta } => meta.clone(),
            KclValue::TagIdentifier(x) => x.meta.clone(),
            KclValue::TagDeclarator(x) => vec![x.metadata()],
            KclValue::Plane(x) => x.meta.clone(),
            KclValue::Face(x) => x.meta.clone(),
            KclValue::Sketch { value } => value.meta.clone(),
            KclValue::Sketches { value } => value.iter().flat_map(|sketch| &sketch.meta).copied().collect(),
            KclValue::Solid(x) => x.meta.clone(),
            KclValue::Solids { value } => value.iter().flat_map(|sketch| &sketch.meta).copied().collect(),
            KclValue::Helix(x) => x.meta.clone(),
            KclValue::ImportedGeometry(x) => x.meta.clone(),
            KclValue::Function { meta, .. } => meta.clone(),
            KclValue::Module { meta, .. } => meta.clone(),
            KclValue::KclNone { meta, .. } => meta.clone(),
        }
    }

    pub(crate) fn function_def_source_range(&self) -> Option<SourceRange> {
        let KclValue::Function { expression, .. } = self else {
            return None;
        };
        // TODO: It would be nice if we could extract the source range starting
        // at the fn, but that's the variable declaration.
        Some(expression.as_source_range())
    }

    pub(crate) fn get_solid_set(&self) -> Result<SolidSet> {
        match self {
            KclValue::Solid(e) => Ok(SolidSet::Solid(e.clone())),
            KclValue::Solids { value } => Ok(SolidSet::Solids(value.clone())),
            KclValue::Array { value, .. } => {
                let solids: Vec<_> = value
                    .iter()
                    .enumerate()
                    .map(|(i, v)| {
                        v.as_solid().map(|v| v.to_owned()).map(Box::new).ok_or_else(|| {
                            anyhow::anyhow!(
                                "expected this array to only contain solids, but element {i} was actually {}",
                                v.human_friendly_type()
                            )
                        })
                    })
                    .collect::<Result<_, _>>()?;
                Ok(SolidSet::Solids(solids))
            }
            _ => anyhow::bail!("Not a solid or solids: {:?}", self),
        }
    }

    #[allow(unused)]
    pub(crate) fn none() -> Self {
        Self::KclNone {
            value: Default::default(),
            meta: Default::default(),
        }
    }

    /// Human readable type name used in error messages.  Should not be relied
    /// on for program logic.
    pub(crate) fn human_friendly_type(&self) -> &'static str {
        match self {
            KclValue::Uuid { .. } => "Unique ID (uuid)",
            KclValue::TagDeclarator(_) => "TagDeclarator",
            KclValue::TagIdentifier(_) => "TagIdentifier",
            KclValue::Solid(_) => "Solid",
            KclValue::Solids { .. } => "Solids",
            KclValue::Sketch { .. } => "Sketch",
            KclValue::Sketches { .. } => "Sketches",
            KclValue::Helix(_) => "Helix",
            KclValue::ImportedGeometry(_) => "ImportedGeometry",
            KclValue::Function { .. } => "Function",
            KclValue::Plane(_) => "Plane",
            KclValue::Face(_) => "Face",
            KclValue::Bool { .. } => "boolean (true/false value)",
            KclValue::Number { .. } => "number",
            KclValue::Int { .. } => "integer",
            KclValue::String { .. } => "string (text)",
            KclValue::Array { .. } => "array (list)",
            KclValue::Object { .. } => "object",
            KclValue::Module { .. } => "module",
            KclValue::KclNone { .. } => "None",
        }
    }

    /// Put the number into a KCL value.
    pub const fn from_number(f: f64, ty: NumericType, meta: Vec<Metadata>) -> Self {
        Self::Number { value: f, ty, meta }
    }

    /// Put the point into a KCL value.
    pub fn from_point2d(p: [f64; 2], ty: NumericType, meta: Vec<Metadata>) -> Self {
        Self::Array {
            value: vec![
                Self::Number {
                    value: p[0],
                    ty: ty.clone(),
                    meta: meta.clone(),
                },
                Self::Number {
                    value: p[1],
                    ty,
                    meta: meta.clone(),
                },
            ],
            meta,
        }
    }

    pub(crate) fn as_usize(&self) -> Option<usize> {
        match self {
            KclValue::Int { value, .. } if *value > 0 => Some(*value as usize),
            KclValue::Number { value, .. } => crate::try_f64_to_usize(*value),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            KclValue::Int { value, .. } => Some(*value),
            KclValue::Number { value, .. } => crate::try_f64_to_i64(*value),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&KclObjectFields> {
        if let KclValue::Object { value, meta: _ } = &self {
            Some(value)
        } else {
            None
        }
    }

    pub fn into_object(self) -> Option<KclObjectFields> {
        if let KclValue::Object { value, meta: _ } = self {
            Some(value)
        } else {
            None
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        if let KclValue::String { value, meta: _ } = &self {
            Some(value)
        } else {
            None
        }
    }

    pub fn as_array(&self) -> Option<&[KclValue]> {
        if let KclValue::Array { value, meta: _ } = &self {
            Some(value)
        } else {
            None
        }
    }

    pub fn as_point2d(&self) -> Option<[f64; 2]> {
        let arr = self.as_array()?;
        if arr.len() != 2 {
            return None;
        }
        let x = arr[0].as_f64()?;
        let y = arr[1].as_f64()?;
        Some([x, y])
    }

    pub fn as_uuid(&self) -> Option<uuid::Uuid> {
        if let KclValue::Uuid { value, meta: _ } = &self {
            Some(*value)
        } else {
            None
        }
    }

    pub fn as_plane(&self) -> Option<&Plane> {
        if let KclValue::Plane(value) = &self {
            Some(value)
        } else {
            None
        }
    }

    pub fn as_solid(&self) -> Option<&Solid> {
        if let KclValue::Solid(value) = &self {
            Some(value)
        } else {
            None
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        if let KclValue::Number { value, .. } = &self {
            Some(*value)
        } else if let KclValue::Int { value, .. } = &self {
            Some(*value as f64)
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let KclValue::Bool { value, meta: _ } = &self {
            Some(*value)
        } else {
            None
        }
    }

    /// If this value fits in a u32, return it.
    pub fn get_u32(&self, source_ranges: Vec<SourceRange>) -> Result<u32, KclError> {
        let u = self.as_int().and_then(|n| u64::try_from(n).ok()).ok_or_else(|| {
            KclError::Semantic(KclErrorDetails {
                message: "Expected an integer >= 0".to_owned(),
                source_ranges: source_ranges.clone(),
            })
        })?;
        u32::try_from(u).map_err(|_| {
            KclError::Semantic(KclErrorDetails {
                message: "Number was too big".to_owned(),
                source_ranges,
            })
        })
    }

    /// If this value is of type function, return it.
    pub fn get_function(&self) -> Option<FnAsArg<'_>> {
        let KclValue::Function {
            func,
            expression,
            memory,
            meta: _,
        } = &self
        else {
            return None;
        };
        Some(FnAsArg {
            func: func.as_ref(),
            expr: expression.to_owned(),
            memory: memory.to_owned(),
        })
    }

    /// Get a tag identifier from a memory item.
    pub fn get_tag_identifier(&self) -> Result<TagIdentifier, KclError> {
        match self {
            KclValue::TagIdentifier(t) => Ok(*t.clone()),
            _ => Err(KclError::Semantic(KclErrorDetails {
                message: format!("Not a tag identifier: {:?}", self),
                source_ranges: self.clone().into(),
            })),
        }
    }

    /// Get a tag declarator from a memory item.
    pub fn get_tag_declarator(&self) -> Result<TagNode, KclError> {
        match self {
            KclValue::TagDeclarator(t) => Ok((**t).clone()),
            _ => Err(KclError::Semantic(KclErrorDetails {
                message: format!("Not a tag declarator: {:?}", self),
                source_ranges: self.clone().into(),
            })),
        }
    }

    /// Get an optional tag from a memory item.
    pub fn get_tag_declarator_opt(&self) -> Result<Option<TagNode>, KclError> {
        match self {
            KclValue::TagDeclarator(t) => Ok(Some((**t).clone())),
            _ => Err(KclError::Semantic(KclErrorDetails {
                message: format!("Not a tag declarator: {:?}", self),
                source_ranges: self.clone().into(),
            })),
        }
    }

    /// If this KCL value is a bool, retrieve it.
    pub fn get_bool(&self) -> Result<bool, KclError> {
        let Self::Bool { value: b, .. } = self else {
            return Err(KclError::Type(KclErrorDetails {
                source_ranges: self.into(),
                message: format!("Expected bool, found {}", self.human_friendly_type()),
            }));
        };
        Ok(*b)
    }

    /// If this memory item is a function, call it with the given arguments, return its val as Ok.
    /// If it's not a function, return Err.
    pub async fn call_fn(
        &self,
        args: Vec<Arg>,
        exec_state: &mut ExecState,
        ctx: ExecutorContext,
    ) -> Result<Option<KclValue>, KclError> {
        let KclValue::Function {
            func,
            expression,
            memory: closure_memory,
            meta,
        } = &self
        else {
            return Err(KclError::Semantic(KclErrorDetails {
                message: "not a in memory function".to_string(),
                source_ranges: vec![],
            }));
        };
        if let Some(func) = func {
            func(
                args,
                closure_memory.as_ref().clone(),
                expression.clone(),
                meta.clone(),
                exec_state,
                ctx,
            )
            .await
        } else {
            crate::execution::call_user_defined_function(
                args,
                closure_memory.as_ref(),
                expression.as_ref(),
                exec_state,
                &ctx,
            )
            .await
        }
    }

    /// If this is a function, call it by applying keyword arguments.
    /// If it's not a function, returns an error.
    pub async fn call_fn_kw(
        &self,
        args: crate::std::Args,
        exec_state: &mut ExecState,
        ctx: ExecutorContext,
        callsite: SourceRange,
    ) -> Result<Option<KclValue>, KclError> {
        let KclValue::Function {
            func,
            expression,
            memory: closure_memory,
            meta: _,
        } = &self
        else {
            return Err(KclError::Semantic(KclErrorDetails {
                message: "cannot call this because it isn't a function".to_string(),
                source_ranges: vec![callsite],
            }));
        };
        if let Some(_func) = func {
            todo!("Implement calling KCL stdlib fns that are aliased. Part of https://github.com/KittyCAD/modeling-app/issues/4600");
        } else {
            crate::execution::call_user_defined_function_kw(
                args.kw_args,
                closure_memory.as_ref(),
                expression.as_ref(),
                exec_state,
                &ctx,
            )
            .await
        }
    }

    pub fn from_literal(literal: &Node<Literal>, settings: &super::MetaSettings) -> Self {
        Self::from_literal_inner(&literal.inner, vec![literal.metadata()], settings)
    }

    pub fn from_default_param_val(v: &DefaultParamVal, settings: &super::MetaSettings) -> Self {
        match v {
            DefaultParamVal::KclNone(kcl_none) => Self::KclNone {
                value: *kcl_none,
                // TODO: This function should actually take metadata.
                meta: Default::default(),
            },
            DefaultParamVal::Literal(literal) => Self::from_literal_inner(literal, Default::default(), settings),
        }
    }

    fn from_literal_inner(literal: &Literal, meta: Vec<Metadata>, settings: &super::MetaSettings) -> Self {
        match &literal.value {
            LiteralValue::Number { value, suffix } => KclValue::Number {
                value: *value,
                ty: NumericType::from_parsed(*suffix, settings),
                meta,
            },
            LiteralValue::String(value) => KclValue::String {
                value: value.clone(),
                meta,
            },
            LiteralValue::Bool(value) => KclValue::Bool {
                value: value.clone(),
                meta,
            },
        }
    }
}
