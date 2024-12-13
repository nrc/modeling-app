//! Functions related to polar coordinates.

use anyhow::Result;
use derive_docs::stdlib;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    errors::KclError,
    execution::{kcl_value::NumericType, ExecState, KclValue, Metadata},
    std::Args,
};

/// Data for polar coordinates.
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, ts_rs::TS, JsonSchema)]
#[ts(export)]
#[serde(rename_all = "camelCase")]
pub struct PolarCoordsData {
    /// The angle of the line (in degrees).
    pub angle: f64,
    /// The length of the line.
    pub length: f64,
}

/// Convert from polar/sphere coordinates to cartesian coordinates.
pub async fn polar(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let data: PolarCoordsData = args.get_data()?;
    let result = inner_polar(&data)?;

    Ok(KclValue::Array {
        value: vec![
            // TODO units
            args.make_user_val_from_f64(result[0], NumericType::internal_length()),
            args.make_user_val_from_f64(result[1], NumericType::internal_length()),
        ],
        meta: vec![Metadata {
            source_range: args.source_range,
        }],
    })
}

/// Convert polar/sphere (azimuth, elevation, distance) coordinates to
/// cartesian (x/y grid) coordinates.
///
/// ```no_run
/// exampleSketch = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> line(polar({angle: 30, length: 5}), %, $thing)
///   |> line([0, 5], %)
///   |> line([segEndX(thing), 0], %)
///   |> line([-20, 10], %)
///   |> close(%)
///  
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "polar",
}]
fn inner_polar(data: &PolarCoordsData) -> Result<[f64; 2], KclError> {
    let angle = data.angle.to_radians();
    let x = data.length * angle.cos();
    let y = data.length * angle.sin();
    Ok([x, y])
}
