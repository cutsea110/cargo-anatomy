//! CLI entry point for the cargo-anatomy tool.
use cargo_anatomy::{analyze_workspace_details_with_thresholds, parse_package, CrateKind};
use clap::{Arg, ArgAction, Command};
use log::info;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
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

    fn sanitize(name: &str) -> String {
        name.replace('-', "_")
    }

    pub(super) fn to_string(
        root: &OutputRoot<OutputEntry>,
        name_map: &[(String, String)],
        label_edges: bool,
        show_types: bool,
        show_types_crates: &std::collections::HashSet<String>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut out = String::new();
        out.push_str("digraph cargo_anatomy {\n");
        out.push_str("    rankdir=LR;\n");
        out.push_str("    node [shape=box];\n");

        let show_all_crates = show_types && show_types_crates.is_empty();

        for (i, (crate_name, _)) in name_map.iter().enumerate() {
            if let Some(entry) = root.crates.get(i) {
                let m = &entry.metrics;
                let e = &entry.evaluation;
                let detailed = show_all_crates || show_types_crates.contains(crate_name);
                if detailed {
                    out.push_str(&format!(
                        "    subgraph cluster_{} {{\n",
                        sanitize(crate_name)
                    ));
                    out.push_str(&format!(
                        "        label=\"{}\\nn={} r={} h={:.2}\\nca={} ce={} a={:.2} i={:.2} d'={:.2}\\nA={:?} H={:?} I={:?} D'={:?}\";\n",
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
                    if let Some(details) = &entry.details {
                        for class in &details.classes {
                            let node_id = sanitize(&format!("{}_{}", crate_name, class.name));
                            out.push_str(&format!(
                                "        \"{}\" [label=\"{}\"];\n",
                                node_id, class.name
                            ));
                        }
                    }
                    out.push_str("    }\n");
                } else {
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
        }

        let mut edges = HashSet::new();

        for (i, (src, _)) in name_map.iter().enumerate() {
            if let Some(src_entry) = root.crates.get(i) {
                if let Some(src_details) = &src_entry.details {
                    let src_detailed = show_all_crates || show_types_crates.contains(src);
                    if src_detailed {
                        for (s, targets) in &src_details.internal_depends_on {
                            for t in targets {
                                let id_src = sanitize(&format!("{}_{}", src, s));
                                let id_dst = sanitize(&format!("{}_{}", src, t));
                                out.push_str(&format!("    \"{}\" -> \"{}\";\n", id_src, id_dst));
                            }
                        }
                    }
                    for (s, maps) in &src_details.external_depends_on {
                        for (dst_crate, types) in maps {
                            if !name_map.iter().any(|(c, _)| c == dst_crate) {
                                continue;
                            }
                            let dst_detailed =
                                show_all_crates || show_types_crates.contains(dst_crate);
                            if src_detailed && dst_detailed {
                                for t in types {
                                    let id_src = sanitize(&format!("{}_{}", src, s));
                                    let id_dst = sanitize(&format!("{}_{}", dst_crate, t));
                                    out.push_str(&format!(
                                        "    \"{}\" -> \"{}\";\n",
                                        id_src, id_dst
                                    ));
                                }
                            } else if src_detailed && !dst_detailed {
                                let id_src = sanitize(&format!("{}_{}", src, s));
                                out.push_str(&format!(
                                    "    \"{}\" -> \"{}\";\n",
                                    id_src, dst_crate
                                ));
                            } else if !src_detailed && dst_detailed {
                                for t in types {
                                    let id_dst = sanitize(&format!("{}_{}", dst_crate, t));
                                    out.push_str(&format!("    \"{}\" -> \"{}\";\n", src, id_dst));
                                }
                            } else {
                                if !edges.insert((src.clone(), dst_crate.clone())) {
                                    continue;
                                }
                                let ec = efferent_couples(src_details, dst_crate);
                                if label_edges {
                                    out.push_str(&format!(
                                        "    \"{}\" -> \"{}\" [taillabel=\"{}\"];\n",
                                        src, dst_crate, ec
                                    ));
                                } else {
                                    out.push_str(&format!(
                                        "    \"{}\" -> \"{}\";\n",
                                        src, dst_crate
                                    ));
                                }
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
        show_types: bool,
        show_types_crates: &std::collections::HashSet<String>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut out = String::new();
        out.push_str("graph LR\n");
        let show_all_crates = show_types && show_types_crates.is_empty();
        for (i, (crate_name, _)) in name_map.iter().enumerate() {
            if let Some(entry) = root.crates.get(i) {
                let id = sanitize(crate_name);
                let m = &entry.metrics;
                let e = &entry.evaluation;
                let detailed = show_all_crates || show_types_crates.contains(crate_name);
                if detailed {
                    out.push_str(&format!(
                        "    subgraph {}[\"{}<br/>n={} r={} h={:.2}<br/>ca={} ce={} a={:.2} i={:.2} d'={:.2}<br/>A={:?} H={:?} I={:?} D'={:?}\"]\n",
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
                    if let Some(details) = &entry.details {
                        use std::collections::HashSet;
                        let mut nodes: HashSet<&str> =
                            details.classes.iter().map(|c| c.name.as_str()).collect();
                        for key in details.internal_depends_on.keys() {
                            nodes.insert(key);
                        }
                        for key in details.internal_depended_by.keys() {
                            nodes.insert(key);
                        }
                        for key in details.external_depends_on.keys() {
                            nodes.insert(key);
                        }
                        for key in details.external_depended_by.keys() {
                            nodes.insert(key);
                        }
                        for name in nodes.into_iter().filter(|n| *n != "__crate_root") {
                            let node_id = sanitize(&format!("{}_{}", crate_name, name));
                            out.push_str(&format!("        {}[\"{}\"]\n", node_id, name));
                        }
                    }
                    out.push_str("    end\n");
                } else {
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
        }

        let mut edges = HashSet::new();

        for (i, (src, _)) in name_map.iter().enumerate() {
            if let Some(src_entry) = root.crates.get(i) {
                if let Some(src_details) = &src_entry.details {
                    let src_detailed = show_all_crates || show_types_crates.contains(src);
                    if src_detailed {
                        for (s, targets) in &src_details.internal_depends_on {
                            for t in targets {
                                let id_src = sanitize(&format!("{}_{}", src, s));
                                let id_dst = sanitize(&format!("{}_{}", src, t));
                                out.push_str(&format!("    {} --> {}\n", id_src, id_dst));
                            }
                        }
                    }
                    for (s, maps) in &src_details.external_depends_on {
                        for (dst_crate, types) in maps {
                            if !name_map.iter().any(|(c, _)| c == dst_crate) {
                                continue;
                            }
                            let dst_detailed =
                                show_all_crates || show_types_crates.contains(dst_crate);
                            if src_detailed && dst_detailed {
                                for t in types {
                                    let id_src = sanitize(&format!("{}_{}", src, s));
                                    let id_dst = sanitize(&format!("{}_{}", dst_crate, t));
                                    out.push_str(&format!("    {} --> {}\n", id_src, id_dst));
                                }
                            } else if src_detailed && !dst_detailed {
                                let id_src = sanitize(&format!("{}_{}", src, s));
                                let id_dst = sanitize(dst_crate);
                                out.push_str(&format!("    {} --> {}\n", id_src, id_dst));
                            } else if !src_detailed && dst_detailed {
                                let id_src = sanitize(src);
                                for t in types {
                                    let id_dst = sanitize(&format!("{}_{}", dst_crate, t));
                                    out.push_str(&format!("    {} --> {}\n", id_src, id_dst));
                                }
                            } else {
                                if !edges.insert((src.clone(), dst_crate.clone())) {
                                    continue;
                                }
                                let id_src = sanitize(src);
                                let id_dst = sanitize(dst_crate);
                                let ec = efferent_couples(src_details, dst_crate);
                                if label_edges {
                                    out.push_str(&format!(
                                        "    {} --|{}|--> {}\n",
                                        id_src, ec, id_dst
                                    ));
                                } else {
                                    out.push_str(&format!("    {} --> {}\n", id_src, id_dst));
                                }
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

#[allow(clippy::too_many_arguments)]
fn emit_results<T>(
    map: std::collections::HashMap<String, T>,
    name_map: &[(String, String)],
    cycles: Vec<Vec<String>>,
    format: &str,
    label_edges: bool,
    show_types: bool,
    show_types_crates: &std::collections::HashSet<String>,
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
        "dot" => cargo_anatomy::loc_try!(graphviz_dot::to_string(
            &root,
            name_map,
            label_edges,
            show_types,
            show_types_crates,
        )),
        "mermaid" => {
            cargo_anatomy::loc_try!(mermaid::to_string(
                &root,
                name_map,
                label_edges,
                show_types,
                show_types_crates
            ))
        }
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

    // When invoked as a cargo subcommand ("cargo anatomy"), cargo passes
    // "anatomy" as the first argument to this binary. Strip that token so
    // clap only sees the actual options provided by the user.
    let mut args: Vec<std::ffi::OsString> = std::env::args_os().collect();
    if args.get(1).map(|a| a == "anatomy").unwrap_or(false) {
        args.remove(1);
    }

    let matches = Command::new("cargo-anatomy")
        .version(env!("CARGO_PKG_VERSION"))
        .disable_help_flag(true)
        .after_help(format!(
            "{}\n\n{}",
            METRICS_HELP.join("\n"),
            EVALUATION_HELP.join("\n")
        ))
        .subcommand(
            Command::new("init")
                .about("Generate a template evaluation config")
                .arg(
                    Arg::new("path")
                        .value_name("PATH")
                        .help("Where to create the config file")
                        .required(false),
                ),
        )
        .arg(
            Arg::new("all")
                .short('a')
                .long("all")
                .help("Show classes and dependency graphs")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("include-external")
                .short('x')
                .long("include-external")
                .help("Include external dependencies in analysis (slower)")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("show-types-crates")
                .long("show-types-crates")
                .help("Comma-separated list of crates to render with type-level detail")
                .value_name("CRATES")
                .value_delimiter(',')
                .action(ArgAction::Append),
        )
        .arg(
            Arg::new("show-types-crates-all")
                .long("show-types-crates-all")
                .help("Render types for all workspace crates (experimental)")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("output")
                .short('o')
                .long("output")
                .help("Output format: json, yaml, dot or mermaid")
                .value_name("FORMAT")
                .default_value("json"),
        )
        .arg(
            Arg::new("config")
                .short('c')
                .long("config")
                .help("Path to evaluation config file")
                .value_name("FILE"),
        )
        .arg(
            Arg::new("h-lt")
                .long("h-lt")
                .value_name("VAL")
                .help("Fail if H < VAL (experimental)"),
        )
        .arg(
            Arg::new("h-le")
                .long("h-le")
                .value_name("VAL")
                .help("Fail if H <= VAL (experimental)"),
        )
        .arg(
            Arg::new("d-prime-gt")
                .long("d-prime-gt")
                .value_name("VAL")
                .help("Fail if D' > VAL (experimental)"),
        )
        .arg(
            Arg::new("d-prime-ge")
                .long("d-prime-ge")
                .value_name("VAL")
                .help("Fail if D' >= VAL (experimental)"),
        )
        .arg(
            Arg::new("help")
                .short('h')
                .action(ArgAction::Help)
                .long("help")
                .visible_short_alias('?')
                .help("Show this help message"),
        )
        .get_matches_from(args);

    if let Some(("init", sub_m)) = matches.subcommand() {
        let path = sub_m
            .get_one::<String>("path")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from(".anatomy.toml"));
        return init_config(path);
    }

    let show_all = matches.get_flag("all");
    let include_external = matches.get_flag("include-external");
    let show_types_crates_all = matches.get_flag("show-types-crates-all");
    let raw_show_types_crates: std::collections::HashSet<String> = matches
        .get_many::<String>("show-types-crates")
        .into_iter()
        .flatten()
        .map(|s| s.to_string())
        .collect();
    let format = matches
        .get_one::<String>("output")
        .map(|s| s.as_str())
        .unwrap_or("json");
    let h_lt = matches
        .get_one::<String>("h-lt")
        .and_then(|s| s.parse::<f64>().ok());
    let h_le = matches
        .get_one::<String>("h-le")
        .and_then(|s| s.parse::<f64>().ok());
    let d_prime_gt = matches
        .get_one::<String>("d-prime-gt")
        .and_then(|s| s.parse::<f64>().ok());
    let d_prime_ge = matches
        .get_one::<String>("d-prime-ge")
        .and_then(|s| s.parse::<f64>().ok());

    let mut cmd = cargo_metadata::MetadataCommand::new();
    if !include_external {
        cmd.no_deps();
    }
    let metadata = cargo_anatomy::loc_try!(cmd.exec());

    let config_path: Option<PathBuf> = matches
        .get_one::<String>("config")
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

    let mut show_types_crates = std::collections::HashSet::new();
    for name in &raw_show_types_crates {
        if let Some((crate_name, _)) = name_map.iter().find(|(c, p)| c == name || p == name) {
            show_types_crates.insert(crate_name.clone());
        }
    }
    if show_types_crates_all {
        for (name, kind) in &kind_map {
            if *kind == CrateKind::Workspace {
                show_types_crates.insert(name.clone());
            }
        }
    }
    let show_types = !show_types_crates.is_empty();

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
            format,
            show_all,
            show_types,
            &show_types_crates,
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
            format,
            false,
            show_types,
            &show_types_crates,
            config_used
        ));
    }
    if threshold_failed {
        std::process::exit(1);
    }
    Ok(())
}
