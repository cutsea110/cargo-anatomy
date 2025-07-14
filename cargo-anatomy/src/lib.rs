use std::collections::{HashMap, HashSet};
use std::fs;

use log::{debug, info};

use serde::Serialize;
use syn::{File, visit::Visit};
use walkdir::WalkDir;

#[derive(Debug, Serialize, Clone)]
pub enum ClassKind {
    Struct,
    Enum,
    Trait,
    TypeAlias,
}

#[derive(Debug, Serialize, Clone)]
pub struct ClassInfo {
    pub name: String,
    pub kind: ClassKind,
}

#[derive(Debug, Serialize, Clone)]
pub struct Metrics {
    pub n: usize,
    pub h: f64,
    pub ca: usize,
    pub ce: usize,
    pub a: f64,
    pub i: f64,
    pub d: f64,
    pub d_prime: f64,
}

#[derive(Debug, Serialize, Clone)]
pub struct CrateDetail {
    pub metrics: Metrics,
    pub classes: Vec<ClassInfo>,
    pub internal_depends_on: HashMap<String, Vec<String>>, // type -> types it depends on
    pub internal_depended_by: HashMap<String, Vec<String>>, // type -> types depending on it
    pub external_depends_on: HashMap<String, HashMap<String, Vec<String>>>, // type -> crate -> types
    pub external_depended_by: HashMap<String, HashMap<String, Vec<String>>>, // type -> crate -> types
}

pub fn collect_defined(files: &[File]) -> (HashMap<String, ClassKind>, usize) {
    let mut defined = HashMap::new();
    let mut abstract_count = 0usize;

    for file in files {
        for item in &file.items {
            match item {
                syn::Item::Struct(item) => {
                    defined.insert(item.ident.to_string(), ClassKind::Struct);
                }
                syn::Item::Enum(item) => {
                    defined.insert(item.ident.to_string(), ClassKind::Enum);
                }
                syn::Item::Trait(item) => {
                    defined.insert(item.ident.to_string(), ClassKind::Trait);
                    abstract_count += 1;
                }
                syn::Item::Type(item) => {
                    defined.insert(item.ident.to_string(), ClassKind::TypeAlias);
                }
                _ => {}
            }
        }
    }
    (defined, abstract_count)
}

pub fn parse_package(
    package: &cargo_metadata::Package,
) -> Result<Vec<File>, Box<dyn std::error::Error>> {
    info!("reading crate {}", package.name);
    let manifest_dir = package.manifest_path.parent().unwrap();
    let src = manifest_dir.join("src");
    let mut files = Vec::new();
    for entry in WalkDir::new(src) {
        let entry = entry?;
        if entry.file_type().is_file()
            && entry.path().extension().map(|s| s == "rs").unwrap_or(false)
        {
            debug!("parsing {}", entry.path().display());
            let content = fs::read_to_string(entry.path())?;
            let file = syn::parse_file(&content)?;
            files.push(file);
        }
    }
    Ok(files)
}

pub fn analyze_package(
    package: &cargo_metadata::Package,
    workspace_types: &HashSet<String>,
) -> Result<Metrics, Box<dyn std::error::Error>> {
    let files = parse_package(package)?;
    Ok(analyze_files(&files, workspace_types))
}

pub fn analyze_files(files: &[File], workspace_types: &HashSet<String>) -> Metrics {
    debug!("collecting definitions from {} files", files.len());
    let (defined, abstract_count) = collect_defined(files);

    let class_count = defined.len();

    let mut visitor = RefVisitor {
        defined: &defined,
        workspace: workspace_types,
        internal: 0,
        external: 0,
    };
    for file in files {
        visitor.visit_file(file);
    }

    let n = class_count;
    let r = visitor.internal;
    let ca = 0usize; // not computed in this function
    let ce = visitor.external;

    let h = if n > 0 {
        (r as f64 + 1.0) / n as f64
    } else {
        0.0
    };
    let a = if n > 0 {
        abstract_count as f64 / n as f64
    } else {
        0.0
    };
    let i = if ca + ce > 0 {
        ce as f64 / (ca + ce) as f64
    } else {
        0.0
    };
    let d_prime = (a + i - 1.0).abs();
    let d = d_prime / 2f64.sqrt();

    debug!(
        "metrics: N={} R={} Ca={} Ce={} A={:.3} I={:.3} D={:.3} D'={:.3}",
        n, r, ca, ce, a, i, d, d_prime
    );

    Metrics {
        n,
        h,
        ca,
        ce,
        a,
        i,
        d,
        d_prime,
    }
}

/// Analyse multiple crates together so cross-crate dependencies can be counted.
pub fn analyze_workspace(crates: &[(String, Vec<File>)]) -> HashMap<String, Metrics> {
    analyze_workspace_details(crates)
        .into_iter()
        .map(|(k, v)| (k, v.metrics))
        .collect()
}

pub fn analyze_workspace_details(crates: &[(String, Vec<File>)]) -> HashMap<String, CrateDetail> {
    debug!("analysing {} crates", crates.len());

    let mut type_map = HashMap::new();
    let mut crate_defined = HashMap::new();
    let mut crate_abstract = HashMap::new();

    for (name, files) in crates {
        let (defined, abstract_count) = collect_defined(files);
        for ty in defined.keys() {
            type_map.insert(ty.clone(), name.clone());
        }
        crate_defined.insert(name.clone(), defined);
        crate_abstract.insert(name.clone(), abstract_count);
    }

    let mut internal_refs: HashMap<String, HashMap<String, HashSet<String>>> = HashMap::new();
    let mut external_refs: HashMap<String, HashMap<String, HashMap<String, HashSet<String>>>> =
        HashMap::new();
    let mut internal_counts = HashMap::new();
    let mut ce_counts = HashMap::new();
    let mut ca_counts: HashMap<String, usize> = HashMap::new();

    for (name, files) in crates {
        let defined = crate_defined.get(name).unwrap();
        let mut visitor = DetailVisitor {
            current: None,
            defined,
            type_map: &type_map,
            internal: HashMap::new(),
            external: HashMap::new(),
            internal_count: 0,
            ce_count: 0,
            ca_counts: &mut ca_counts,
        };
        for f in files {
            visitor.visit_file(f);
        }
        internal_refs.insert(name.clone(), visitor.internal);
        external_refs.insert(name.clone(), visitor.external);
        internal_counts.insert(name.clone(), visitor.internal_count);
        ce_counts.insert(name.clone(), visitor.ce_count);
    }

    let mut internal_rev: HashMap<String, HashMap<String, HashSet<String>>> = HashMap::new();
    for (crate_name, map) in &internal_refs {
        for (from, tos) in map {
            for to in tos {
                internal_rev
                    .entry(crate_name.clone())
                    .or_default()
                    .entry(to.clone())
                    .or_default()
                    .insert(from.clone());
            }
        }
    }

    let mut external_rev: HashMap<String, HashMap<String, HashMap<String, HashSet<String>>>> =
        HashMap::new();
    for (src_crate, map) in &external_refs {
        for (from, crates_map) in map {
            for (target_crate, types) in crates_map {
                for ty in types {
                    external_rev
                        .entry(target_crate.clone())
                        .or_default()
                        .entry(ty.clone())
                        .or_default()
                        .entry(src_crate.clone())
                        .or_default()
                        .insert(from.clone());
                }
            }
        }
    }

    let mut result = HashMap::new();
    for (name, _) in crates {
        let n = crate_defined.get(name).map(|s| s.len()).unwrap_or(0);
        let r = *internal_counts.get(name).unwrap_or(&0);
        let ce = *ce_counts.get(name).unwrap_or(&0);
        let ca = *ca_counts.get(name).unwrap_or(&0);
        let a_count = *crate_abstract.get(name).unwrap_or(&0);
        let h = if n > 0 {
            (r as f64 + 1.0) / n as f64
        } else {
            0.0
        };
        let a = if n > 0 {
            a_count as f64 / n as f64
        } else {
            0.0
        };
        let i = if ca + ce > 0 {
            ce as f64 / (ca + ce) as f64
        } else {
            0.0
        };
        let d_prime = (a + i - 1.0).abs();
        let d = d_prime / 2f64.sqrt();

        let classes = crate_defined
            .get(name)
            .unwrap()
            .iter()
            .map(|(n, k)| ClassInfo {
                name: n.clone(),
                kind: k.clone(),
            })
            .collect();

        let to_vec_map = |map: &HashMap<String, HashSet<String>>| {
            map.iter()
                .map(|(k, v)| (k.clone(), v.iter().cloned().collect::<Vec<_>>()))
                .collect::<HashMap<_, _>>()
        };

        let to_vec_nested = |map: &HashMap<String, HashMap<String, HashSet<String>>>| {
            map.iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        v.iter()
                            .map(|(k2, set)| (k2.clone(), set.iter().cloned().collect::<Vec<_>>()))
                            .collect::<HashMap<_, _>>(),
                    )
                })
                .collect::<HashMap<_, _>>()
        };

        result.insert(
            name.clone(),
            CrateDetail {
                metrics: Metrics {
                    n,
                    h,
                    ca,
                    ce,
                    a,
                    i,
                    d,
                    d_prime,
                },
                classes,
                internal_depends_on: to_vec_map(internal_refs.get(name).unwrap_or(&HashMap::new())),
                internal_depended_by: to_vec_map(internal_rev.get(name).unwrap_or(&HashMap::new())),
                external_depends_on: to_vec_nested(
                    external_refs.get(name).unwrap_or(&HashMap::new()),
                ),
                external_depended_by: to_vec_nested(
                    external_rev.get(name).unwrap_or(&HashMap::new()),
                ),
            },
        );
    }

    result
}

struct RefVisitor<'a> {
    defined: &'a HashMap<String, ClassKind>,
    workspace: &'a HashSet<String>,
    internal: usize,
    external: usize,
}

impl<'ast> Visit<'ast> for RefVisitor<'_> {
    fn visit_path(&mut self, node: &'ast syn::Path) {
        if let Some(seg) = node.segments.last() {
            let name = seg.ident.to_string();
            if self.defined.contains_key(&name) {
                self.internal += 1;
            } else if self.workspace.contains(&name) {
                self.external += 1;
            }
        }
        syn::visit::visit_path(self, node);
    }
}

struct DetailVisitor<'a> {
    current: Option<String>,
    defined: &'a HashMap<String, ClassKind>,
    type_map: &'a HashMap<String, String>,
    internal: HashMap<String, HashSet<String>>, // from -> to
    external: HashMap<String, HashMap<String, HashSet<String>>>, // from -> crate -> types
    internal_count: usize,
    ce_count: usize,
    ca_counts: &'a mut HashMap<String, usize>,
}

impl<'ast> Visit<'ast> for DetailVisitor<'_> {
    fn visit_item_struct(&mut self, i: &'ast syn::ItemStruct) {
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_struct(self, i);
        self.current = None;
    }
    fn visit_item_enum(&mut self, i: &'ast syn::ItemEnum) {
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_enum(self, i);
        self.current = None;
    }
    fn visit_item_trait(&mut self, i: &'ast syn::ItemTrait) {
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_trait(self, i);
        self.current = None;
    }
    fn visit_item_type(&mut self, i: &'ast syn::ItemType) {
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_type(self, i);
        self.current = None;
    }
    fn visit_item_impl(&mut self, i: &'ast syn::ItemImpl) {
        if let syn::Type::Path(tp) = &*i.self_ty {
            if let Some(seg) = tp.path.segments.last() {
                let name = seg.ident.to_string();
                if self.defined.contains_key(&name) {
                    self.current = Some(name);
                    syn::visit::visit_item_impl(self, i);
                    self.current = None;
                    return;
                }
            }
        }
        syn::visit::visit_item_impl(self, i);
    }
    fn visit_path(&mut self, node: &'ast syn::Path) {
        if let Some(seg) = node.segments.last() {
            let name = seg.ident.to_string();
            if let Some(current) = &self.current {
                if self.defined.contains_key(&name) {
                    self.internal_count += 1;
                    self.internal
                        .entry(current.clone())
                        .or_default()
                        .insert(name.clone());
                } else if let Some(owner) = self.type_map.get(&name) {
                    self.ce_count += 1;
                    *self.ca_counts.entry(owner.clone()).or_insert(0) += 1;
                    self.external
                        .entry(current.clone())
                        .or_default()
                        .entry(owner.clone())
                        .or_default()
                        .insert(name.clone());
                }
            }
        }
        syn::visit::visit_path(self, node);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_metrics() {
        let src = r#"
            pub struct A {
                b: B,
                map: std::collections::HashMap<String, String>,
            }
            pub struct B;
            pub trait MyTrait {
                fn do_it(&self, a: A);
            }
        "#;
        let file: syn::File = syn::parse_str(src).unwrap();
        let defs = collect_defined(&[file.clone()]);
        let workspace: HashSet<String> = defs.0.keys().cloned().collect();
        let metrics = analyze_files(&[file], &workspace);
        assert_eq!(metrics.n, 3);
        assert_eq!(metrics.ca, 0);
        assert_eq!(metrics.ce, 0); // HashMap is outside workspace
        assert!((metrics.h - ((2.0 + 1.0) / 3.0)).abs() < 1e-6);
        assert!((metrics.a - (1.0 / 3.0)).abs() < 1e-6);
        // With no cross-crate dependencies, instability is defined as zero
        assert!(metrics.i.abs() < 1e-6);
    }

    #[test]
    fn cross_crate_metrics() {
        let src_a = "pub struct A;";
        let src_b = "pub struct B { a: crate_a::A }";
        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];
        let metrics = analyze_workspace(&crates);
        let metrics_a = metrics.get("crate_a").unwrap();
        let metrics_b = metrics.get("crate_b").unwrap();

        assert_eq!(metrics_a.ca, 1);
        assert_eq!(metrics_a.ce, 0);
        assert_eq!(metrics_b.ce, 1);
    }

    #[test]
    fn detailed_info() {
        let src_a = "pub struct A;";
        let src_b = "pub struct B { a: crate_a::A }";
        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];
        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        assert_eq!(a_info.classes.len(), 1);
        assert_eq!(a_info.classes[0].name, "A");
        assert!(
            a_info
                .external_depended_by
                .get("A")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.contains(&"B".to_string()))
                .unwrap_or(false)
        );
    }

    #[test]
    fn trait_bound_dependency() {
        let src_a = "pub trait Foo {}";
        let src_b = "use crate_a::Foo; pub struct Bar<U: Foo>(U);";

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];

        let metrics = analyze_workspace(&crates);
        let metrics_a = metrics.get("crate_a").unwrap();
        let metrics_b = metrics.get("crate_b").unwrap();

        assert_eq!(metrics_a.ca, 1);
        assert_eq!(metrics_b.ce, 1);
    }
}
