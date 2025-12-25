//! # Rosetta Validator
//!
//! Validates that transpiled Rust code produces numerically equivalent
//! results to the original source code.
//!
//! ## Validation Strategies
//!
//! 1. **Bit-exact**: Output must be identical bit-for-bit
//! 2. **Relative tolerance**: |a - b| / max(|a|, |b|) < epsilon
//! 3. **Absolute tolerance**: |a - b| < epsilon
//! 4. **ULP comparison**: Units in Last Place for floating point

use thiserror::Error;

/// Validation errors
#[derive(Debug, Error)]
pub enum ValidationError {
    #[error("Values differ: expected {expected}, got {actual}")]
    ValueMismatch { expected: f64, actual: f64 },

    #[error("Array length mismatch: expected {expected}, got {actual}")]
    LengthMismatch { expected: usize, actual: usize },

    #[error("Relative error {error} exceeds tolerance {tolerance}")]
    ToleranceExceeded { error: f64, tolerance: f64 },

    #[error("Execution error: {0}")]
    ExecutionError(String),
}

/// Validation result
#[derive(Debug, Clone)]
pub struct ValidationResult {
    /// Whether validation passed
    pub passed: bool,
    /// Maximum relative error observed
    pub max_relative_error: f64,
    /// Maximum absolute error observed
    pub max_absolute_error: f64,
    /// Number of values compared
    pub values_compared: usize,
    /// Number of values that matched
    pub values_matched: usize,
    /// Detailed mismatches (if any)
    pub mismatches: Vec<Mismatch>,
}

/// A single value mismatch
#[derive(Debug, Clone)]
pub struct Mismatch {
    /// Index or name of mismatched value
    pub location: String,
    /// Expected value
    pub expected: f64,
    /// Actual value
    pub actual: f64,
    /// Relative error
    pub relative_error: f64,
}

/// Numerical validator
pub struct NumericalValidator {
    /// Relative tolerance
    pub relative_tolerance: f64,
    /// Absolute tolerance
    pub absolute_tolerance: f64,
    /// Maximum ULP difference for bit comparison
    pub max_ulp: u64,
}

impl Default for NumericalValidator {
    fn default() -> Self {
        Self {
            relative_tolerance: 1e-10,
            absolute_tolerance: 1e-15,
            max_ulp: 4,
        }
    }
}

impl NumericalValidator {
    /// Create a validator with custom tolerances
    pub fn new(relative: f64, absolute: f64) -> Self {
        Self {
            relative_tolerance: relative,
            absolute_tolerance: absolute,
            max_ulp: 4,
        }
    }

    /// Compare two floating point values
    pub fn compare(&self, expected: f64, actual: f64) -> bool {
        // Handle NaN
        if expected.is_nan() && actual.is_nan() {
            return true;
        }

        // Handle infinities
        if expected.is_infinite() || actual.is_infinite() {
            return expected == actual;
        }

        // Absolute comparison for values near zero
        let abs_diff = (expected - actual).abs();
        if abs_diff < self.absolute_tolerance {
            return true;
        }

        // Relative comparison
        let max_val = expected.abs().max(actual.abs());
        if max_val > 0.0 {
            let rel_diff = abs_diff / max_val;
            if rel_diff < self.relative_tolerance {
                return true;
            }
        }

        // ULP comparison for very precise matching
        let expected_bits = expected.to_bits();
        let actual_bits = actual.to_bits();
        let ulp_diff = if expected_bits > actual_bits {
            expected_bits - actual_bits
        } else {
            actual_bits - expected_bits
        };

        ulp_diff <= self.max_ulp
    }

    /// Validate two arrays
    pub fn validate_arrays(&self, expected: &[f64], actual: &[f64]) -> ValidationResult {
        if expected.len() != actual.len() {
            return ValidationResult {
                passed: false,
                max_relative_error: f64::INFINITY,
                max_absolute_error: f64::INFINITY,
                values_compared: 0,
                values_matched: 0,
                mismatches: vec![],
            };
        }

        let mut max_rel_err = 0.0_f64;
        let mut max_abs_err = 0.0_f64;
        let mut matched = 0;
        let mut mismatches = Vec::new();

        for (i, (&exp, &act)) in expected.iter().zip(actual.iter()).enumerate() {
            let abs_err = (exp - act).abs();
            let rel_err = if exp.abs().max(act.abs()) > 0.0 {
                abs_err / exp.abs().max(act.abs())
            } else {
                0.0
            };

            max_abs_err = max_abs_err.max(abs_err);
            max_rel_err = max_rel_err.max(rel_err);

            if self.compare(exp, act) {
                matched += 1;
            } else {
                mismatches.push(Mismatch {
                    location: format!("[{}]", i),
                    expected: exp,
                    actual: act,
                    relative_error: rel_err,
                });
            }
        }

        ValidationResult {
            passed: mismatches.is_empty(),
            max_relative_error: max_rel_err,
            max_absolute_error: max_abs_err,
            values_compared: expected.len(),
            values_matched: matched,
            mismatches,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_match() {
        let validator = NumericalValidator::default();
        assert!(validator.compare(1.0, 1.0));
    }

    #[test]
    fn test_within_tolerance() {
        let validator = NumericalValidator::new(1e-9, 1e-12);
        assert!(validator.compare(1.0, 1.0 + 1e-10));
    }

    #[test]
    fn test_array_validation() {
        let validator = NumericalValidator::default();
        let expected = vec![1.0, 2.0, 3.0];
        let actual = vec![1.0, 2.0, 3.0];

        let result = validator.validate_arrays(&expected, &actual);
        assert!(result.passed);
        assert_eq!(result.values_matched, 3);
    }
}
