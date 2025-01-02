//! Functions related to mathematics.

use anyhow::Result;
use derive_docs::stdlib;

use crate::{
    errors::{KclError, KclErrorDetails},
    execution::{ExecState, KclValue},
    std::Args,
};

use super::args::FromArgs;

/// Compute the remainder after dividing `num` by `div`.
/// If `num` is negative, the result will be too.
pub async fn rem(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let n = args.get_unlabeled_kw_arg("number to divide")?;
    let d = args.get_kw_arg("divisor")?;
    let remainder = inner_rem(n, d);

    Ok(args.make_user_val_from_f64(remainder))
}

/// Compute the remainder after dividing `num` by `div`.
/// If `num` is negative, the result will be too.
///
/// ```no_run
/// assertEqual(rem( 7,  divisor =  4),  3, 0.01, "remainder is 3" )
/// assertEqual(rem(-7,  divisor =  4), -3, 0.01, "remainder is -3")
/// assertEqual(rem( 7,  divisor = -4),  3, 0.01, "remainder is 3" )
/// assertEqual(rem( 6,    divisor = 2.5), 1,   0.01, "remainder is 1" )
/// assertEqual(rem( 6.5,  divisor = 2.5), 1.5, 0.01, "remainder is 1.5" )
/// assertEqual(rem( 6.5,  divisor = 2),   0.5, 0.01, "remainder is 0.5" )
/// ```
#[stdlib {
    name = "rem",
    tags = ["math"],
    keywords = true,
    unlabeled_first = true,
    arg_docs = {
        num = "The number which will be divided by `divisor`.",
        divisor = "The number which will divide `num`.",
    }
}]
fn inner_rem(num: f64, divisor: f64) -> f64 {
    num % divisor
}

/// Compute the cosine of a number (in radians).
pub async fn cos(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_cos(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the cosine of a number (in radians).
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 30,
///     length = 3 / cos(toRadians(30)),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///  
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "cos",
    tags = ["math"],
}]
fn inner_cos(num: f64) -> Result<f64, KclError> {
    Ok(num.cos())
}

/// Compute the sine of a number (in radians).
pub async fn sin(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_sin(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the sine of a number (in radians).
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = 15 / sin(toDegrees(135)),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "sin",
    tags = ["math"],
}]
fn inner_sin(num: f64) -> Result<f64, KclError> {
    Ok(num.sin())
}

/// Compute the tangent of a number (in radians).
pub async fn tan(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_tan(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the tangent of a number (in radians).
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = 50 * tan(1/2),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "tan",
    tags = ["math"],
}]
fn inner_tan(num: f64) -> Result<f64, KclError> {
    Ok(num.tan())
}

/// Return the value of `pi`. Archimedes’ constant (π).
pub async fn pi(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let result = inner_pi()?;

    Ok(args.make_user_val_from_f64(result))
}

/// Return the value of `pi`. Archimedes’ constant (π).
///
/// ```no_run
/// circumference = 70
///
/// exampleSketch = startSketchOn("XZ")
///  |> circle({ center = [0, 0], radius = circumference/ (2 * pi()) }, %)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "pi",
    tags = ["math"],
}]
fn inner_pi() -> Result<f64, KclError> {
    Ok(std::f64::consts::PI)
}

/// Compute the square root of a number.
pub async fn sqrt(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_sqrt(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the square root of a number.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = sqrt(2500),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "sqrt",
    tags = ["math"],
}]
fn inner_sqrt(num: f64) -> Result<f64, KclError> {
    Ok(num.sqrt())
}

/// Compute the absolute value of a number.
pub async fn abs(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_abs(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the absolute value of a number.
///
/// ```no_run
/// myAngle = -120
///
/// sketch001 = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> line([8, 0], %)
///   |> angledLine({
///     angle = abs(myAngle),
///     length = 5,
///   }, %)
///   |> line([-5, 0], %)
///   |> angledLine({
///     angle = myAngle,
///     length = 5,
///   }, %)
///   |> close(%)
///
/// baseExtrusion = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "abs",
    tags = ["math"],
}]
fn inner_abs(num: f64) -> Result<f64, KclError> {
    Ok(num.abs())
}

/// Round a number to the nearest integer.
pub async fn round(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_round(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Round a number to the nearest integer.
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///    |> startProfileAt([0, 0], %)
///    |> lineTo([12, 10], %)
///    |> line([round(7.02986), 0], %)
///    |> yLineTo(0, %)
///    |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "round",
    tags = ["math"],
}]
fn inner_round(num: f64) -> Result<f64, KclError> {
    Ok(num.round())
}

/// Compute the largest integer less than or equal to a number.
pub async fn floor(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_floor(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the largest integer less than or equal to a number.
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///    |> startProfileAt([0, 0], %)
///    |> lineTo([12, 10], %)
///    |> line([floor(7.02986), 0], %)
///    |> yLineTo(0, %)
///    |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "floor",
    tags = ["math"],
}]
fn inner_floor(num: f64) -> Result<f64, KclError> {
    Ok(num.floor())
}

/// Compute the smallest integer greater than or equal to a number.
pub async fn ceil(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_ceil(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the smallest integer greater than or equal to a number.
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> lineTo([12, 10], %)
///   |> line([ceil(7.02986), 0], %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "ceil",
    tags = ["math"],
}]
fn inner_ceil(num: f64) -> Result<f64, KclError> {
    Ok(num.ceil())
}

/// Compute the minimum of the given arguments.
pub async fn min(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let nums = args.get_number_array()?;
    let result = inner_min(nums);

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the minimum of the given arguments.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 70,
///     length = min(15, 31, 4, 13, 22)
///   }, %)
///   |> line([20, 0], %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "min",
    tags = ["math"],
}]
fn inner_min(args: Vec<f64>) -> f64 {
    let mut min = f64::MAX;
    for arg in args.iter() {
        if *arg < min {
            min = *arg;
        }
    }

    min
}

/// Compute the maximum of the given arguments.
pub async fn max(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let nums = args.get_number_array()?;
    let result = inner_max(nums);

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the maximum of the given arguments.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 70,
///     length = max(15, 31, 4, 13, 22)
///   }, %)
///   |> line([20, 0], %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "max",
    tags = ["math"],
}]
fn inner_max(args: Vec<f64>) -> f64 {
    let mut max = f64::MIN;
    for arg in args.iter() {
        if *arg > max {
            max = *arg;
        }
    }

    max
}

/// Compute the number to a power.
pub async fn pow(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let nums = args.get_number_array()?;
    if nums.len() > 2 {
        return Err(KclError::Type(KclErrorDetails {
            message: format!("expected 2 arguments, got {}", nums.len()),
            source_ranges: vec![args.source_range],
        }));
    }

    if nums.len() <= 1 {
        return Err(KclError::Type(KclErrorDetails {
            message: format!("expected 2 arguments, got {}", nums.len()),
            source_ranges: vec![args.source_range],
        }));
    }

    let result = inner_pow(nums[0], nums[1])?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the number to a power.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = pow(5, 2),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "pow",
    tags = ["math"],
}]
fn inner_pow(num: f64, pow: f64) -> Result<f64, KclError> {
    Ok(num.powf(pow))
}

/// Compute the arccosine of a number (in radians).
pub async fn acos(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_acos(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the arccosine of a number (in radians).
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = toDegrees(acos(0.5)),
///     length = 10,
///   }, %)
///   |> line([5, 0], %)
///   |> lineTo([12, 0], %)
///   |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "acos",
    tags = ["math"],
}]
fn inner_acos(num: f64) -> Result<f64, KclError> {
    Ok(num.acos())
}

/// Compute the arcsine of a number (in radians).
pub async fn asin(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_asin(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the arcsine of a number (in radians).
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = toDegrees(asin(0.5)),
///     length = 20,
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "asin",
    tags = ["math"],
}]
fn inner_asin(num: f64) -> Result<f64, KclError> {
    Ok(num.asin())
}

/// Compute the arctangent of a number (in radians).
pub async fn atan(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_atan(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the arctangent of a number (in radians).
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = toDegrees(atan(1.25)),
///     length = 20,
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "atan",
    tags = ["math"],
}]
fn inner_atan(num: f64) -> Result<f64, KclError> {
    Ok(num.atan())
}

/// Compute the four quadrant arctangent of Y and X (in radians).
pub async fn atan2(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let (y, x) = FromArgs::from_args(&args, 0)?;
    let result = inner_atan2(y, x)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the four quadrant arctangent of Y and X (in radians).
///
/// ```no_run
/// sketch001 = startSketchOn('XZ')
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = toDegrees(atan2(1.25, 2)),
///     length = 20,
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// extrude001 = extrude(5, sketch001)
/// ```
#[stdlib {
    name = "atan2",
    tags = ["math"],
}]
fn inner_atan2(y: f64, x: f64) -> Result<f64, KclError> {
    Ok(y.atan2(x))
}

/// Compute the logarithm of the number with respect to an arbitrary base.
///
/// The result might not be correctly rounded owing to implementation
/// details; `log2()` can produce more accurate results for base 2,
/// and `log10()` can produce more accurate results for base 10.
pub async fn log(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let nums = args.get_number_array()?;
    if nums.len() > 2 {
        return Err(KclError::Type(KclErrorDetails {
            message: format!("expected 2 arguments, got {}", nums.len()),
            source_ranges: vec![args.source_range],
        }));
    }

    if nums.len() <= 1 {
        return Err(KclError::Type(KclErrorDetails {
            message: format!("expected 2 arguments, got {}", nums.len()),
            source_ranges: vec![args.source_range],
        }));
    }
    let result = inner_log(nums[0], nums[1])?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the logarithm of the number with respect to an arbitrary base.
///
/// The result might not be correctly rounded owing to implementation
/// details; `log2()` can produce more accurate results for base 2,
/// and `log10()` can produce more accurate results for base 10.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> line([log(100, 5), 0], %)
///   |> line([5, 8], %)
///   |> line([-10, 0], %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "log",
    tags = ["math"],
}]
fn inner_log(num: f64, base: f64) -> Result<f64, KclError> {
    Ok(num.log(base))
}

/// Compute the base 2 logarithm of the number.
pub async fn log2(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_log2(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the base 2 logarithm of the number.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> line([log2(100), 0], %)
///   |> line([5, 8], %)
///   |> line([-10, 0], %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "log2",
    tags = ["math"],
}]
fn inner_log2(num: f64) -> Result<f64, KclError> {
    Ok(num.log2())
}

/// Compute the base 10 logarithm of the number.
pub async fn log10(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_log10(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the base 10 logarithm of the number.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> line([log10(100), 0], %)
///   |> line([5, 8], %)
///   |> line([-10, 0], %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "log10",
    tags = ["math"],
}]
fn inner_log10(num: f64) -> Result<f64, KclError> {
    Ok(num.log10())
}

/// Compute the natural logarithm of the number.
pub async fn ln(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_ln(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Compute the natural logarithm of the number.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> line([ln(100), 15], %)
///   |> line([5, -6], %)
///   |> line([-10, -10], %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "ln",
    tags = ["math"],
}]
fn inner_ln(num: f64) -> Result<f64, KclError> {
    Ok(num.ln())
}

/// Return the value of Euler’s number `e`.
pub async fn e(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let result = inner_e()?;

    Ok(args.make_user_val_from_f64(result))
}

/// Return the value of Euler’s number `e`.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 30,
///     length = 2 * e() ^ 2,
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///  
/// example = extrude(10, exampleSketch)
/// ```
#[stdlib {
    name = "e",
    tags = ["math"],
}]
fn inner_e() -> Result<f64, KclError> {
    Ok(std::f64::consts::E)
}

/// Return the value of `tau`. The full circle constant (τ). Equal to 2π.
pub async fn tau(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let result = inner_tau()?;

    Ok(args.make_user_val_from_f64(result))
}

/// Return the value of `tau`. The full circle constant (τ). Equal to 2π.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = 10 * tau(),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "tau",
    tags = ["math"],
}]
fn inner_tau() -> Result<f64, KclError> {
    Ok(std::f64::consts::TAU)
}

/// Converts a number from degrees to radians.
pub async fn to_radians(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_to_radians(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Converts a number from degrees to radians.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = 70 * cos(toRadians(45)),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "toRadians",
    tags = ["math"],
}]
fn inner_to_radians(num: f64) -> Result<f64, KclError> {
    Ok(num.to_radians())
}

/// Converts a number from radians to degrees.
pub async fn to_degrees(_exec_state: &mut ExecState, args: Args) -> Result<KclValue, KclError> {
    let num = args.get_number()?;
    let result = inner_to_degrees(num)?;

    Ok(args.make_user_val_from_f64(result))
}

/// Converts a number from radians to degrees.
///
/// ```no_run
/// exampleSketch = startSketchOn("XZ")
///   |> startProfileAt([0, 0], %)
///   |> angledLine({
///     angle = 50,
///     length = 70 * cos(toDegrees(pi()/4)),
///   }, %)
///   |> yLineTo(0, %)
///   |> close(%)
///
/// example = extrude(5, exampleSketch)
/// ```
#[stdlib {
    name = "toDegrees",
    tags = ["math"],
}]
fn inner_to_degrees(num: f64) -> Result<f64, KclError> {
    Ok(num.to_degrees())
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_inner_max() {
        let nums = vec![4.0, 5.0, 6.0];
        let result = inner_max(nums);
        assert_eq!(result, 6.0);
    }

    #[test]
    fn test_inner_max_with_neg() {
        let nums = vec![4.0, -5.0];
        let result = inner_max(nums);
        assert_eq!(result, 4.0);
    }

    #[test]
    fn test_inner_min() {
        let nums = vec![4.0, 5.0, 6.0];
        let result = inner_min(nums);
        assert_eq!(result, 4.0);
    }

    #[test]
    fn test_inner_min_with_neg() {
        let nums = vec![4.0, -5.0];
        let result = inner_min(nums);
        assert_eq!(result, -5.0);
    }
}
