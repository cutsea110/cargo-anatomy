//! CLI entry point for the cargo-anatomy tool.
use cargo_anatomy::{analyze_workspace_details_with_thresholds, parse_package, CrateKind};
use getopts::Options;
use log::info;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

const METRICS_HELP: &[&str] = &[
    "Metrics:",
    "  N  - number of classes",
    "  R  - number of internal class relationships",
    "  H  - relational cohesion: (R + 1)/N",
    "  Ca - afferent coupling: external classes that depend on this crate",
    "  Ce - efferent coupling: classes in this crate depending on other workspace crates",
    "  A  - abstraction: traits / N",
    "  I  - instability: Ce / (Ce + Ca)",
    "  D  - distance from main sequence: |A + I - 1| / sqrt(2)",
    "  D' - normalized distance: |A + I - 1|",
];

const EVALUATION_HELP: &[&str] = &[
    "Evaluation:",
    "  A  - >=0.7 abstract, <=0.3 concrete, otherwise mixed",
    "  H  - >1.0 high, otherwise low",
    "  I  - >=0.7 unstable, <=0.3 stable, otherwise moderate",
    "  D' - <=0.4 good; >=0.6 useless if A+I-1 >= 0 else painful; otherwise balanced",
];

const CONFIG_TEMPLATE: &str = "# Configuration for cargo-anatomy\n\n\
[evaluation]\n\
  [evaluation.abstraction]\n\
  # Minimum ratio considered abstract\n\
  abstract_min = 0.7\n\
  # Maximum ratio considered concrete\n\
  concrete_max = 0.3\n\
\n\
  [evaluation.cohesion]\n\
  # Values greater than this are high cohesion\n\
  high_gt = 1.0\n\
\n\
  [evaluation.instability]\n\
  # Minimum ratio considered unstable\n\
  unstable_min = 0.7\n\
  # Maximum ratio considered stable\n\
  stable_max = 0.3\n\
\n\
  [evaluation.distance]\n\
  # Maximum normalized distance considered good\n\
  good_max = 0.4\n\
  # Minimum normalized distance considered bad\n\
  bad_min = 0.6\n";

fn print_help_to(opts: &Options, mut w: impl Write) -> io::Result<()> {
    let brief = format!(
        "cargo-anatomy {}\nUsage: cargo anatomy [options]",
        env!("CARGO_PKG_VERSION")
    );
    write!(w, "{}", opts.usage(&brief))?;
    writeln!(w)?;

    writeln!(w, "{}", METRICS_HELP.join("\n"))?;

    writeln!(w, "{}", EVALUATION_HELP.join("\n"))?;
    Ok(())
}

fn print_help(opts: &Options) {
    let _ = print_help_to(opts, io::stdout());
}

fn init_config<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn std::error::Error>> {
    let p = path.as_ref();
    if p.exists() {
        eprintln!("{} already exists", p.display());
        return Ok(());
    }
    std::fs::write(p, CONFIG_TEMPLATE)?;
    println!("created {}", p.display());
    Ok(())
}

fn crate_target_name(pkg: &cargo_metadata::Package) -> String {
    for target in &pkg.targets {
        if target
            .kind
            .iter()
            .any(|k| matches!(k, cargo_metadata::TargetKind::Lib))
        {
            return target.name.clone();
        }
    }
    pkg.targets
        .first()
        .map(|t| t.name.clone())
        .unwrap_or_else(|| pkg.name.replace('-', "_"))
}

#[derive(Clone, Serialize)]
struct CrateDetails {
    kind: cargo_anatomy::CrateKind,
    classes: Vec<cargo_anatomy::ClassInfo>,
    internal_depends_on: std::collections::HashMap<String, Vec<String>>,
    internal_depended_by: std::collections::HashMap<String, Vec<String>>,
    external_depends_on:
        std::collections::HashMap<String, std::collections::HashMap<String, Vec<String>>>,
    external_depended_by:
        std::collections::HashMap<String, std::collections::HashMap<String, Vec<String>>>,
}

#[derive(Clone, Serialize)]
struct OutputEntry {
    crate_name: String,
    metrics: cargo_anatomy::Metrics,
    evaluation: cargo_anatomy::Evaluation,
    #[serde(skip_serializing_if = "Option::is_none")]
    details: Option<CrateDetails>,
}

#[derive(Serialize)]
struct Warnings {
    dependency_cycles: Vec<Vec<String>>,
}

#[derive(Serialize, Clone)]
struct ToolInfo {
    version: &'static str,
    target: String,
}

#[derive(Serialize, Clone)]
struct Meta {
    #[serde(rename = "cargo-anatomy")]
    cargo_anatomy: ToolInfo,
    config: cargo_anatomy::Config,
}

#[derive(Serialize)]
struct OutputRoot<T: Serialize> {
    meta: Meta,
    crates: Vec<T>,
    warnings: Warnings,
}

mod graphviz_dot {
    use super::{CrateDetails, OutputEntry, OutputRoot};
    use std::collections::HashSet;

    fn efferent_couples(details: &CrateDetails, target: &str) -> usize {
        details
            .external_depends_on
            .iter()
            .filter(|(_, map)| map.contains_key(target))
            .count()
    }

    pub(super) fn to_string(
        root: &OutputRoot<OutputEntry>,
        name_map: &[(String, String)],
        label_edges: bool,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut out = String::new();
        out.push_str("digraph cargo_anatomy {\n");
        out.push_str("    rankdir=LR;\n");
        out.push_str("    node [shape=box];\n");

        for (i, (crate_name, _)) in name_map.iter().enumerate() {
            if let Some(entry) = root.crates.get(i) {
                let m = &entry.metrics;
                let e = &entry.evaluation;
                out.push_str(&format!(
                    "    \"{}\" [label=\"{}\\nn={} r={} h={:.2}\\nca={} ce={} a={:.2} i={:.2} d'={:.2}\\nA={:?} H={:?} I={:?} D'={:?}\"];\n",
                    crate_name,
                    crate_name,
                    m.n,
                    m.r,
                    m.h,
                    m.ca,
                    m.ce,
                    m.a,
                    m.i,
                    m.d_prime,
                    e.a,
                    e.h,
                    e.i,
                    e.d_prime,
                ));
            }
        }

        let mut edges = HashSet::new();

        for (i, (src, _)) in name_map.iter().enumerate() {
            if let Some(src_entry) = root.crates.get(i) {
                if let Some(src_details) = &src_entry.details {
                    for maps in src_details.external_depends_on.values() {
                        for dst in maps.keys() {
                            if !edges.insert((src.clone(), dst.clone())) {
                                continue;
                            }
                            let ec = efferent_couples(src_details, dst);
                            if label_edges {
                                out.push_str(&format!(
                                    "    \"{}\" -> \"{}\" [taillabel=\"{}\"];\n",
                                    src, dst, ec
                                ));
                            } else {
                                out.push_str(&format!("    \"{}\" -> \"{}\";\n", src, dst));
                            }
                        }
                    }
                }
            }
        }

        out.push_str("}\n");
        Ok(out)
    }
}

mod mermaid {
    use super::{CrateDetails, OutputEntry, OutputRoot};
    use std::collections::HashSet;

    fn efferent_couples(details: &CrateDetails, target: &str) -> usize {
        details
            .external_depends_on
            .iter()
            .filter(|(_, map)| map.contains_key(target))
            .count()
    }

    fn sanitize(name: &str) -> String {
        name.replace('-', "_")
    }

    pub(super) fn to_string(
        root: &OutputRoot<OutputEntry>,
        name_map: &[(String, String)],
        label_edges: bool,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut out = String::new();
        out.push_str("graph LR\n");

        for (i, (crate_name, _)) in name_map.iter().enumerate() {
            if let Some(entry) = root.crates.get(i) {
                let id = sanitize(crate_name);
                let m = &entry.metrics;
                let e = &entry.evaluation;
                out.push_str(&format!(
                    "    {}[\"{}<br/>n={} r={} h={:.2}<br/>ca={} ce={} a={:.2} i={:.2} d'={:.2}<br/>A={:?} H={:?} I={:?} D'={:?}\"]\n",
                    id,
                    crate_name,
                    m.n,
                    m.r,
                    m.h,
                    m.ca,
                    m.ce,
                    m.a,
                    m.i,
                    m.d_prime,
                    e.a,
                    e.h,
                    e.i,
                    e.d_prime,
                ));
            }
        }

        let mut edges = HashSet::new();

        for (i, (src, _)) in name_map.iter().enumerate() {
            if let Some(src_entry) = root.crates.get(i) {
                if let Some(src_details) = &src_entry.details {
                    for maps in src_details.external_depends_on.values() {
                        for dst in maps.keys() {
                            if !edges.insert((src.clone(), dst.clone())) {
                                continue;
                            }
                            let id_src = sanitize(src);
                            let id_dst = sanitize(dst);
                            let ec = efferent_couples(src_details, dst);
                            if label_edges {
                                out.push_str(&format!("    {} --|{}|--> {}\n", id_src, ec, id_dst));
                            } else {
                                out.push_str(&format!("    {} --> {}\n", id_src, id_dst));
                            }
                        }
                    }
                }
            }
        }

        Ok(out)
    }
}

trait IntoOutput: Clone + Serialize {
    fn into_output(self, package_name: String) -> OutputEntry;
}

#[derive(Clone, Serialize)]
struct MetricsWithEval {
    metrics: cargo_anatomy::Metrics,
    evaluation: cargo_anatomy::Evaluation,
}

impl IntoOutput for MetricsWithEval {
    fn into_output(self, package_name: String) -> OutputEntry {
        OutputEntry {
            crate_name: package_name,
            metrics: self.metrics,
            evaluation: self.evaluation,
            details: None,
        }
    }
}

impl IntoOutput for cargo_anatomy::CrateDetail {
    fn into_output(self, package_name: String) -> OutputEntry {
        OutputEntry {
            crate_name: package_name,
            metrics: self.metrics,
            evaluation: self.evaluation,
            details: Some(CrateDetails {
                kind: self.kind,
                classes: self.classes,
                internal_depends_on: self.internal_depends_on,
                internal_depended_by: self.internal_depended_by,
                external_depends_on: self.external_depends_on,
                external_depended_by: self.external_depended_by,
            }),
        }
    }
}

fn emit_results<T>(
    map: std::collections::HashMap<String, T>,
    name_map: &[(String, String)],
    cycles: Vec<Vec<String>>,
    format: &str,
    label_edges: bool,
    config: cargo_anatomy::Config,
) -> Result<(), Box<dyn std::error::Error>>
where
    T: IntoOutput,
{
    let mut out = Vec::new();
    for (crate_name, package_name) in name_map {
        if let Some(item) = map.get(crate_name) {
            out.push(item.clone().into_output(package_name.clone()));
        }
    }
    let root = OutputRoot {
        meta: Meta {
            cargo_anatomy: ToolInfo {
                version: env!("CARGO_PKG_VERSION"),
                target: format!("{}/{}", std::env::consts::OS, std::env::consts::ARCH),
            },
            config,
        },
        crates: out,
        warnings: Warnings {
            dependency_cycles: cycles,
        },
    };
    let out_str = match format {
        "json" => cargo_anatomy::loc_try!(serde_json::to_string(&root)),
        "yaml" => cargo_anatomy::loc_try!(serde_yaml::to_string(&root)),
        "dot" => cargo_anatomy::loc_try!(graphviz_dot::to_string(&root, name_map, label_edges)),
        "mermaid" => cargo_anatomy::loc_try!(mermaid::to_string(&root, name_map, label_edges)),
        other => {
            eprintln!("unknown output format: {}", other);
            return Ok(());
        }
    };
    println!("{}", out_str);
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::Builder::from_default_env()
        .format_source_path(true)
        .format_line_number(true)
        .init();
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 && args[1] == "init" {
        let path = Path::new(".anatomy.toml");
        return init_config(path);
    }

    let mut opts = Options::new();
    opts.optflag("a", "all", "Show classes and dependency graphs");
    opts.optflag(
        "x",
        "include-external",
        "Include external dependencies in analysis (slower)",
    );
    opts.optflag("V", "version", "Show version information");
    opts.optopt(
        "o",
        "output",
        "Output format: json, yaml, dot or mermaid",
        "FORMAT",
    );
    opts.optopt("c", "config", "Path to evaluation config file", "FILE");
    opts.optflag("?", "", "Show this help message");
    opts.optflag("h", "help", "Show this help message");
    opts.optopt("", "h-lt", "Fail if H < VAL (experimental)", "VAL");
    opts.optopt("", "h-le", "Fail if H <= VAL (experimental)", "VAL");
    opts.optopt("", "d-prime-gt", "Fail if D' > VAL (experimental)", "VAL");
    opts.optopt("", "d-prime-ge", "Fail if D' >= VAL (experimental)", "VAL");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("{}", f);
            print_help(&opts);
            return Ok(());
        }
    };

    if matches.opt_present("V") || matches.opt_present("version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    if matches.opt_present("?") || matches.opt_present("h") || matches.opt_present("help") {
        print_help(&opts);
        return Ok(());
    }

    let show_all = matches.opt_present("a") || matches.opt_present("all");
    let include_external = matches.opt_present("x") || matches.opt_present("include-external");
    let format = matches
        .opt_str("o")
        .or_else(|| matches.opt_str("output"))
        .unwrap_or_else(|| "json".to_string());
    let h_lt = matches
        .opt_str("h-lt")
        .map(|s| s.parse::<f64>())
        .transpose()?;
    let h_le = matches
        .opt_str("h-le")
        .map(|s| s.parse::<f64>())
        .transpose()?;
    let d_prime_gt = matches
        .opt_str("d-prime-gt")
        .map(|s| s.parse::<f64>())
        .transpose()?;
    let d_prime_ge = matches
        .opt_str("d-prime-ge")
        .map(|s| s.parse::<f64>())
        .transpose()?;
    let mut cmd = cargo_metadata::MetadataCommand::new();
    if !include_external {
        cmd.no_deps();
    }
    let metadata = cargo_anatomy::loc_try!(cmd.exec());

    let config_path: Option<PathBuf> = matches
        .opt_str("c")
        .or_else(|| matches.opt_str("config"))
        .map(PathBuf::from)
        .or_else(|| {
            let default = Path::new(&metadata.workspace_root).join(".anatomy.toml");
            if default.exists() {
                Some(default)
            } else {
                None
            }
        });

    let eval_thresholds = if let Some(ref path) = config_path {
        match std::fs::read_to_string(path) {
            Ok(s) => match toml::from_str::<cargo_anatomy::Config>(&s) {
                Ok(cfg) => cfg.evaluation,
                Err(e) => {
                    eprintln!("failed to parse config: {}", e);
                    cargo_anatomy::EvaluationThresholds::default()
                }
            },
            Err(e) => {
                eprintln!("failed to read config: {}", e);
                cargo_anatomy::EvaluationThresholds::default()
            }
        }
    } else {
        cargo_anatomy::EvaluationThresholds::default()
    };
    let config_used = cargo_anatomy::Config {
        evaluation: eval_thresholds.clone(),
    };
    info!(
        "found {} workspace members",
        metadata.workspace_members.len()
    );

    // Parse all crates first
    let mut crates = Vec::new();
    let mut name_map = Vec::new();
    let mut kind_map = HashMap::new();
    let mut seen = HashSet::new();

    let packages: Vec<&cargo_metadata::Package> = if include_external {
        metadata.packages.iter().collect()
    } else {
        metadata
            .workspace_members
            .iter()
            .map(|id| &metadata[id])
            .collect()
    };

    for package in packages {
        let crate_name = crate_target_name(package);
        if !seen.insert(crate_name.clone()) {
            continue;
        }
        let files = cargo_anatomy::loc_try!(parse_package(package));
        name_map.push((crate_name.clone(), package.name.to_string()));
        let kind = if metadata.workspace_members.contains(&package.id) {
            CrateKind::Workspace
        } else {
            CrateKind::External
        };
        kind_map.insert(crate_name.clone(), kind);
        crates.push((crate_name, files));
    }

    let mut details_map = analyze_workspace_details_with_thresholds(&crates, &eval_thresholds);
    for (name, detail) in details_map.iter_mut() {
        if let Some(k) = kind_map.get(name) {
            detail.kind = *k;
        }
    }
    let cycles = cargo_anatomy::dependency_cycles(&details_map);

    let mut threshold_failed = false;
    for detail in details_map.values() {
        if let Some(th) = h_lt {
            if detail.metrics.h < th {
                threshold_failed = true;
            }
        }
        if let Some(th) = h_le {
            if detail.metrics.h <= th {
                threshold_failed = true;
            }
        }
        if let Some(th) = d_prime_gt {
            if detail.metrics.d_prime > th {
                threshold_failed = true;
            }
        }
        if let Some(th) = d_prime_ge {
            if detail.metrics.d_prime >= th {
                threshold_failed = true;
            }
        }
    }

    if show_all {
        cargo_anatomy::loc_try!(emit_results(
            details_map,
            &name_map,
            cycles,
            &format,
            show_all,
            config_used.clone()
        ));
    } else {
        let metrics_map: HashMap<String, MetricsWithEval> = details_map
            .into_iter()
            .map(|(k, v)| {
                let eval = cargo_anatomy::evaluate_metrics_with(&v.metrics, &eval_thresholds);
                (
                    k,
                    MetricsWithEval {
                        metrics: v.metrics,
                        evaluation: eval,
                    },
                )
            })
            .collect();
        for (_, package_name) in &name_map {
            info!("processing crate {}", package_name);
        }
        cargo_anatomy::loc_try!(emit_results(
            metrics_map,
            &name_map,
            cycles,
            &format,
            false,
            config_used
        ));
    }
    if threshold_failed {
        std::process::exit(1);
    }
    Ok(())
}
