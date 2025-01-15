//! Standard library lofts.

use std::num::NonZeroU32;

use anyhow::Result;
use derive_docs::stdlib;
use kcmc::{each_cmd as mcmd, length_unit::LengthUnit, ModelingCmd};
use kittycad_modeling_cmds as kcmc;

use crate::{
    errors::{KclError, KclErrorDetails},
    execution::{kcl_value::NumericType, ExecState, KclValue, Sketch, Solid, Type},
    std::{extrude::do_post_extrude, fillet::default_tolerance, Args},
};

const DEFAULT_V_DEGREE: u32 = 2;

/// Create a 3D surface or solid by interpolating between two or more sketches.
pub async fn loft(exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let sketches = args.get_unlabeled_kw_arg("sketches")?;
    let v_degree = args
        .get_kw_arg_opt_typed("vDegree", &Type::Int, exec_state.mut_memory())
        .unwrap_or(DEFAULT_V_DEGREE);
    // Attempt to approximate rational curves (such as arcs) using a bezier.
    // This will remove banding around interpolations between arcs and non-arcs.  It may produce errors in other scenarios
    // Over time, this field won't be necessary.
    let bez_approximate_rational = args.get_kw_arg_opt("bezApproximateRational").unwrap_or(false);
    // This can be set to override the automatically determined topological base curve, which is usually the first section encountered.
    let base_curve_index = args.get_kw_arg_opt_typed("baseCurveIndex", &Type::Int, exec_state.mut_memory());
    // Tolerance for the loft operation.
    let tolerance: Option<f64> =
        args.get_kw_arg_opt_typed("tolerance", &NumericType::count().into(), exec_state.mut_memory());

    let solid = inner_loft(
        sketches,
        v_degree,
        bez_approximate_rational,
        base_curve_index,
        tolerance,
        exec_state,
        args,
    )
    .await?;
    Ok(KclValue::Solid(solid))
}

/// Create a 3D surface or solid by interpolating between two or more sketches.
///
/// The sketches need to closed and on the same plane.
///
/// ```no_run
/// // Loft a square and a triangle.
/// squareSketch = startSketchOn('XY')
///     |> startProfileAt([-100, 200], %)
///     |> line([200, 0], %)
///     |> line([0, -200], %)
///     |> line([-200, 0], %)
///     |> lineTo([profileStartX(%), profileStartY(%)], %)
///     |> close(%)
///
/// triangleSketch = startSketchOn(offsetPlane('XY', 75))
///     |> startProfileAt([0, 125], %)
///     |> line([-15, -30], %)
///     |> line([30, 0], %)
///     |> lineTo([profileStartX(%), profileStartY(%)], %)
///     |> close(%)
///
/// loft([squareSketch, triangleSketch])
/// ```
///
/// ```no_run
/// // Loft a square, a circle, and another circle.
/// squareSketch = startSketchOn('XY')
///     |> startProfileAt([-100, 200], %)
///     |> line([200, 0], %)
///     |> line([0, -200], %)
///     |> line([-200, 0], %)
///     |> lineTo([profileStartX(%), profileStartY(%)], %)
///     |> close(%)
///
/// circleSketch0 = startSketchOn(offsetPlane('XY', 75))
///     |> circle({ center = [0, 100], radius = 50 }, %)
///
/// circleSketch1 = startSketchOn(offsetPlane('XY', 150))
///     |> circle({ center = [0, 100], radius = 20 }, %)
///
/// loft([squareSketch, circleSketch0, circleSketch1])
/// ```
///
/// ```no_run
/// // Loft a square, a circle, and another circle with options.
/// squareSketch = startSketchOn('XY')
///     |> startProfileAt([-100, 200], %)
///     |> line([200, 0], %)
///     |> line([0, -200], %)
///     |> line([-200, 0], %)
///     |> lineTo([profileStartX(%), profileStartY(%)], %)
///     |> close(%)
///
/// circleSketch0 = startSketchOn(offsetPlane('XY', 75))
///     |> circle({ center = [0, 100], radius = 50 }, %)
///
/// circleSketch1 = startSketchOn(offsetPlane('XY', 150))
///     |> circle({ center = [0, 100], radius = 20 }, %)
///
/// loft([squareSketch, circleSketch0, circleSketch1],
///     baseCurveIndex = 0,
///     bezApproximateRational = false,
///     tolerance = 0.000001,
///     vDegree = 2,
/// )
/// ```
#[stdlib {
    name = "loft",
    feature_tree_operation = true,
    keywords = true,
    unlabeled_first = true,
    arg_docs = {
        sketches = "Which sketches to loft. Must include at least 2 sketches.",
        v_degree = "Degree of the interpolation. Must be greater than zero. For example, use 2 for quadratic, or 3 for cubic interpolation in the V direction. This defaults to 2, if not specified.",
        bez_approximate_rational = "Attempt to approximate rational curves (such as arcs) using a bezier. This will remove banding around interpolations between arcs and non-arcs. It may produce errors in other scenarios Over time, this field won't be necessary.",
        base_curve_index = "This can be set to override the automatically determined topological base curve, which is usually the first section encountered.",
        tolerance = "Tolerance for the loft operation.",
    }
}]
async fn inner_loft(
    sketches: Vec<Sketch>,
    v_degree: u32,
    bez_approximate_rational: bool,
    base_curve_index: Option<u32>,
    tolerance: Option<f64>,
    exec_state: &mut ExecState,
    args: Args,
) -> Result<Box<Solid>, KclError> {
    // Make sure we have at least two sketches.
    if sketches.len() < 2 {
        return Err(KclError::Semantic(KclErrorDetails {
            message: format!(
                "Loft requires at least two sketches, but only {} were provided.",
                sketches.len()
            ),
            source_ranges: vec![args.source_range],
        }));
    }

    if v_degree == 0 {
        return Err(KclError::Semantic(KclErrorDetails {
            message: "Degree of interpolation (vDegree) argument to loft must be greater than zero.".to_owned(),
            source_ranges: vec![args.source_range],
        }));
    }
    let v_degree = NonZeroU32::new(v_degree).unwrap();

    let id = exec_state.next_uuid();
    args.batch_modeling_cmd(
        id,
        ModelingCmd::from(mcmd::Loft {
            section_ids: sketches.iter().map(|group| group.id).collect(),
            base_curve_index,
            bez_approximate_rational,
            tolerance: LengthUnit(tolerance.unwrap_or(default_tolerance(&args.ctx.settings.units))),
            v_degree,
        }),
    )
    .await?;

    // Using the first sketch as the base curve, idk we might want to change this later.
    let mut sketch = sketches[0].clone();
    // Override its id with the loft id so we can get its faces later
    sketch.id = id;
    do_post_extrude(sketch, 0.0, exec_state, args).await
}
