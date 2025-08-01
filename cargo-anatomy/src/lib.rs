//! Utilities for analyzing Rust crates and computing package metrics.

mod analysis;
mod metrics;
mod utils;
pub use utils::error_with_location;

pub use metrics::{
    evaluate_metrics, evaluate_metrics_with, AbstractionEval, AbstractionThresholds, ClassInfo,
    ClassKind, CohesionEval, CohesionThresholds, Config, CrateKind, DistanceEval,
    DistanceThresholds, Evaluation, EvaluationThresholds, Metrics, MetricsResult, StabilityEval,
};

pub use analysis::{
    analyze_files, analyze_package, analyze_workspace, analyze_workspace_details,
    analyze_workspace_details_with_thresholds, analyze_workspace_with_thresholds, collect_defined,
    collect_methods, collect_trait_bounds, dependency_cycles, parse_package, CrateDetail,
};

#[cfg(test)]
mod tests;
