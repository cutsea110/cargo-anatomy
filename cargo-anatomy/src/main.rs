//! CLI entry point for the cargo-anatomy tool.
use cargo_anatomy::{analyze_workspace_details, parse_package, CrateKind};
use env_logger;
use getopts::Options;
use log::info;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

fn print_help_to(opts: &Options, mut w: impl Write) -> io::Result<()> {
    let brief = format!(
        "cargo-anatomy {}\nUsage: cargo anatomy [options]",
        env!("CARGO_PKG_VERSION")
    );
    write!(w, "{}", opts.usage(&brief))?;
    writeln!(w)?;

    let metrics = [
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
    writeln!(w, "{}", metrics.join("\n"))?;

    let evaluation = [
        "Evaluation:",
        "  A  - >=0.7 abstract, <=0.3 concrete, otherwise mixed",
        "  H  - >1.0 high, otherwise low",
        "  I  - >=0.7 unstable, <=0.3 stable, otherwise moderate",
        "  D' - <=0.4 good; >=0.6 useless if A+I-1 >= 0 else painful; otherwise balanced",
    ];
    writeln!(w, "{}", evaluation.join("\n"))?;
    Ok(())
}

fn print_help(opts: &Options) {
    let _ = print_help_to(opts, io::stdout());
}

fn crate_target_name(pkg: &cargo_metadata::Package) -> String {
    for target in &pkg.targets {
        if target.kind.iter().any(|k| k == "lib") {
            return target.name.clone();
        }
    }
    pkg.targets
        .get(0)
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

#[derive(Serialize)]
struct OutputRoot<T: Serialize> {
    crates: Vec<T>,
    warnings: Warnings,
}

mod graphviz_dot {
    use super::{CrateDetails, OutputEntry, OutputRoot};
    use std::collections::{HashMap, HashSet};

    fn efferent_couples(details: &CrateDetails, target: &str) -> usize {
        let mut set = HashSet::new();
        for (class, map) in &details.external_depends_on {
            if map.contains_key(target) {
                set.insert(class.clone());
            }
        }
        set.len()
    }

    fn afferent_couples(details: &CrateDetails, src: &str) -> usize {
        let mut set = HashSet::new();
        for (class, map) in &details.external_depends_on {
            if map.contains_key(src) {
                set.insert(class.clone());
            }
        }
        set.len()
    }

    pub(super) fn to_string(
        root: &OutputRoot<OutputEntry>,
        name_map: &[(String, String)],
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
        // Map crate name to its details for quick lookup
        let mut details_by_name: HashMap<&str, &crate::CrateDetails> = HashMap::new();
        for (i, (name, _)) in name_map.iter().enumerate() {
            if let Some(entry) = root.crates.get(i) {
                if let Some(d) = &entry.details {
                    details_by_name.insert(name.as_str(), d);
                }
            }
        }

        for (i, (src, _)) in name_map.iter().enumerate() {
            if let Some(src_entry) = root.crates.get(i) {
                if let Some(src_details) = &src_entry.details {
                    for maps in src_details.external_depends_on.values() {
                        for (dst, _) in maps {
                            if !edges.insert((src.clone(), dst.clone())) {
                                continue;
                            }
                            let ec = efferent_couples(src_details, dst);
                            let ac = if let Some(dst_details) = details_by_name.get(dst.as_str()) {
                                afferent_couples(dst_details, src)
                            } else {
                                0
                            };
                            out.push_str(&format!(
                                "    \"{}\" -> \"{}\" [label=\"Ca={} Ce={}\"];\n",
                                src, dst, ac, ec
                            ));
                        }
                    }
                }
            }
        }

        out.push_str("}\n");
        Ok(out)
    }
}

trait IntoOutput: Clone + Serialize {
    fn into_output(self, package_name: String) -> OutputEntry;
}

impl IntoOutput for cargo_anatomy::Metrics {
    fn into_output(self, package_name: String) -> OutputEntry {
        let eval = cargo_anatomy::evaluate_metrics(&self);
        OutputEntry {
            crate_name: package_name,
            metrics: self,
            evaluation: eval,
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
        crates: out,
        warnings: Warnings {
            dependency_cycles: cycles,
        },
    };
    let out_str = match format {
        "json" => cargo_anatomy::loc_try!(serde_json::to_string(&root)),
        "yaml" => cargo_anatomy::loc_try!(serde_yaml::to_string(&root)),
        "dot" => cargo_anatomy::loc_try!(graphviz_dot::to_string(&root, name_map)),
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

    let mut opts = Options::new();
    opts.optflag("a", "all", "Show classes and dependency graphs");
    opts.optflag(
        "x",
        "include-external",
        "Include external dependencies in analysis (slower)",
    );
    opts.optflag("V", "version", "Show version information");
    opts.optopt("o", "output", "Output format: json, yaml or dot", "FORMAT");
    opts.optflag("?", "", "Show this help message");
    opts.optflag("h", "help", "Show this help message");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            eprintln!("{}", f.to_string());
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
    let mut cmd = cargo_metadata::MetadataCommand::new();
    if !include_external {
        cmd.no_deps();
    }
    let metadata = cargo_anatomy::loc_try!(cmd.exec());
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
        name_map.push((crate_name.clone(), package.name.clone()));
        let kind = if metadata.workspace_members.contains(&package.id) {
            CrateKind::Workspace
        } else {
            CrateKind::External
        };
        kind_map.insert(crate_name.clone(), kind);
        crates.push((crate_name, files));
    }

    let mut details_map = analyze_workspace_details(&crates);
    for (name, detail) in details_map.iter_mut() {
        if let Some(k) = kind_map.get(name) {
            detail.kind = *k;
        }
    }
    let cycles = cargo_anatomy::dependency_cycles(&details_map);

    if show_all || format == "dot" {
        cargo_anatomy::loc_try!(emit_results(details_map, &name_map, cycles, &format));
    } else {
        let metrics_map: HashMap<String, cargo_anatomy::Metrics> = details_map
            .into_iter()
            .map(|(k, v)| (k, v.metrics))
            .collect();
        for (_, package_name) in &name_map {
            info!("processing crate {}", package_name);
        }
        cargo_anatomy::loc_try!(emit_results(metrics_map, &name_map, cycles, &format));
    }
    Ok(())
}
