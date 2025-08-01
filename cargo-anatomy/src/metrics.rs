use serde::{Deserialize, Serialize};

/// Kind of a Rust item defined in a crate.
#[derive(Debug, Serialize, Clone)]
pub enum ClassKind {
    Struct,
    Enum,
    Trait,
    TypeAlias,
    Macro,
}
/// Information about a type defined in a crate.
#[derive(Debug, Serialize, Clone)]
pub struct ClassInfo {
    pub name: String,
    pub kind: ClassKind,
}

/// Whether a crate is part of the workspace or an external dependency.
#[derive(Debug, Serialize, Clone, Copy, PartialEq, Eq)]
pub enum CrateKind {
    Workspace,
    External,
}

/// Metrics describing the coupling and cohesion of a crate.
#[derive(Debug, Serialize, Clone)]
pub struct Metrics {
    pub r: usize,
    pub n: usize,
    pub h: f64,
    pub ca: usize,
    pub ce: usize,
    pub a: f64,
    pub i: f64,
    pub d: f64,
    pub d_prime: f64,
}

/// Qualitative labels for abstraction.
#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum AbstractionEval {
    Abstract,
    Mixed,
    Concrete,
}

/// Qualitative labels for cohesion.
#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum CohesionEval {
    High,
    Low,
}

/// Qualitative labels for stability.
#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum StabilityEval {
    Stable,
    Moderate,
    Unstable,
}

/// Qualitative labels for normalized distance.
#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum DistanceEval {
    Good,
    Balanced,
    Painful,
    Useless,
}

/// Labels evaluating package metrics.
#[derive(Debug, Serialize, Clone)]
pub struct Evaluation {
    pub a: AbstractionEval,
    pub h: CohesionEval,
    pub i: StabilityEval,
    pub d_prime: DistanceEval,
}

/// Metrics accompanied by evaluation labels used for output.
#[derive(Debug, Serialize, Clone)]
pub struct MetricsResult {
    pub metrics: Metrics,
    pub evaluation: Evaluation,
}

/// Threshold values used when evaluating metrics.
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct EvaluationThresholds {
    #[serde(default)]
    pub abstraction: AbstractionThresholds,
    #[serde(default)]
    pub cohesion: CohesionThresholds,
    #[serde(default)]
    pub instability: InstabilityThresholds,
    #[serde(default)]
    pub distance: DistanceThresholds,
}

/// Root structure for configuration files.
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct Config {
    #[serde(default)]
    pub evaluation: EvaluationThresholds,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AbstractionThresholds {
    #[serde(default = "default_abstract_min")]
    pub abstract_min: f64,
    #[serde(default = "default_concrete_max")]
    pub concrete_max: f64,
}

fn default_abstract_min() -> f64 {
    0.7
}

fn default_concrete_max() -> f64 {
    0.3
}

impl Default for AbstractionThresholds {
    fn default() -> Self {
        Self {
            abstract_min: default_abstract_min(),
            concrete_max: default_concrete_max(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CohesionThresholds {
    #[serde(default = "default_high_gt")]
    pub high_gt: f64,
}

fn default_high_gt() -> f64 {
    1.0
}

impl Default for CohesionThresholds {
    fn default() -> Self {
        Self {
            high_gt: default_high_gt(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct InstabilityThresholds {
    #[serde(default = "default_unstable_min")]
    pub unstable_min: f64,
    #[serde(default = "default_stable_max")]
    pub stable_max: f64,
}

fn default_unstable_min() -> f64 {
    0.7
}

fn default_stable_max() -> f64 {
    0.3
}

impl Default for InstabilityThresholds {
    fn default() -> Self {
        Self {
            unstable_min: default_unstable_min(),
            stable_max: default_stable_max(),
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DistanceThresholds {
    #[serde(default = "default_good_max")]
    pub good_max: f64,
    #[serde(default = "default_bad_min")]
    pub bad_min: f64,
}

fn default_good_max() -> f64 {
    0.4
}

fn default_bad_min() -> f64 {
    0.6
}

impl Default for DistanceThresholds {
    fn default() -> Self {
        Self {
            good_max: default_good_max(),
            bad_min: default_bad_min(),
        }
    }
}

/// Assign qualitative labels to numerical metrics.
///
/// Thresholds loosely follow the metrics described in Robert C. Martin's *Agile Software Development*.
pub fn evaluate_metrics(m: &Metrics) -> Evaluation {
    evaluate_metrics_with(m, &EvaluationThresholds::default())
}

/// Assign qualitative labels to numerical metrics using custom thresholds.
pub fn evaluate_metrics_with(m: &Metrics, t: &EvaluationThresholds) -> Evaluation {
    let a_label = if m.a >= t.abstraction.abstract_min {
        AbstractionEval::Abstract
    } else if m.a <= t.abstraction.concrete_max {
        AbstractionEval::Concrete
    } else {
        AbstractionEval::Mixed
    };

    let h_label = if m.h > t.cohesion.high_gt {
        CohesionEval::High
    } else {
        CohesionEval::Low
    };

    let i_label = if m.i >= t.instability.unstable_min {
        StabilityEval::Unstable
    } else if m.i <= t.instability.stable_max {
        StabilityEval::Stable
    } else {
        StabilityEval::Moderate
    };

    let d_label = if m.d_prime <= t.distance.good_max {
        DistanceEval::Good
    } else if m.d_prime >= t.distance.bad_min {
        if m.a + m.i - 1.0 >= 0.0 {
            DistanceEval::Useless
        } else {
            DistanceEval::Painful
        }
    } else {
        DistanceEval::Balanced
    };

    Evaluation {
        a: a_label,
        h: h_label,
        i: i_label,
        d_prime: d_label,
    }
}
