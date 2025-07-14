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

pub fn collect_methods(files: &[File]) -> HashMap<(String, String), String> {
    let mut map = HashMap::new();
    fn ret_ty(output: &syn::ReturnType, self_ty: &str) -> Option<String> {
        match output {
            syn::ReturnType::Type(_, ty) => match &**ty {
                syn::Type::Path(p) => p.path.segments.last().map(|s| {
                    if s.ident == "Self" {
                        self_ty.to_string()
                    } else {
                        s.ident.to_string()
                    }
                }),
                _ => None,
            },
            _ => None,
        }
    }

    for file in files {
        for item in &file.items {
            match item {
                syn::Item::Impl(imp) => {
                    if let syn::Type::Path(tp) = &*imp.self_ty {
                        if let Some(seg) = tp.path.segments.last() {
                            let self_ty = seg.ident.to_string();
                            for item in &imp.items {
                                if let syn::ImplItem::Fn(m) = item {
                                    if let Some(ret) = ret_ty(&m.sig.output, &self_ty) {
                                        map.insert((self_ty.clone(), m.sig.ident.to_string()), ret);
                                    }
                                }
                            }
                        }
                    }
                }
                syn::Item::Trait(t) => {
                    let trait_name = t.ident.to_string();
                    for item in &t.items {
                        if let syn::TraitItem::Fn(m) = item {
                            if let Some(ret) = ret_ty(&m.sig.output, &trait_name) {
                                map.insert((trait_name.clone(), m.sig.ident.to_string()), ret);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    map
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
    let mut method_map: HashMap<(String, String), String> = HashMap::new();

    for (name, files) in crates {
        let (defined, abstract_count) = collect_defined(files);
        let methods = collect_methods(files);
        method_map.extend(methods);
        for ty in defined.keys() {
            type_map.insert(ty.clone(), name.clone());
        }
        crate_defined.insert(name.clone(), defined);
        crate_abstract.insert(name.clone(), abstract_count);
    }

    let mut internal_refs: HashMap<String, HashMap<String, HashSet<String>>> = HashMap::new();
    let mut external_refs: HashMap<String, HashMap<String, HashMap<String, HashSet<String>>>> =
        HashMap::new();

    for (name, files) in crates {
        let defined = crate_defined.get(name).unwrap();
        let mut visitor = DetailVisitor {
            current: None,
            defined,
            type_map: &type_map,
            internal: HashMap::new(),
            external: HashMap::new(),
            methods: &method_map,
        };
        for f in files {
            visitor.visit_file(f);
        }
        internal_refs.insert(name.clone(), visitor.internal);
        external_refs.insert(name.clone(), visitor.external);
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
        let r = internal_refs
            .get(name)
            .map(|m| m.values().map(|s| s.len()).sum())
            .unwrap_or(0);

        let mut ce_set = HashSet::new();
        if let Some(map) = external_refs.get(name) {
            for crate_map in map.values() {
                for (c, types) in crate_map {
                    for ty in types {
                        ce_set.insert(format!("{}::{}", c, ty));
                    }
                }
            }
        }
        let ce = ce_set.len();

        let mut ca_set = HashSet::new();
        if let Some(map) = external_rev.get(name) {
            for crate_map in map.values() {
                for (c, from_set) in crate_map {
                    for src in from_set {
                        ca_set.insert(format!("{}::{}", c, src));
                    }
                }
            }
        }
        let ca = ca_set.len();

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
    methods: &'a HashMap<(String, String), String>,
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
                    self.internal
                        .entry(current.clone())
                        .or_default()
                        .insert(name.clone());
                } else if let Some(owner) = self.type_map.get(&name) {
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

    fn visit_expr_call(&mut self, node: &'ast syn::ExprCall) {
        if let syn::Expr::Path(p) = &*node.func {
            if p.path.segments.len() >= 2 {
                let func = p.path.segments.last().unwrap().ident.to_string();
                let ty = p.path.segments[p.path.segments.len() - 2].ident.to_string();
                if let Some(current) = &self.current {
                    if self.defined.contains_key(&ty) {
                        self.internal
                            .entry(current.clone())
                            .or_default()
                            .insert(ty.clone());
                    } else if let Some(owner) = self.type_map.get(&ty) {
                        self.external
                            .entry(current.clone())
                            .or_default()
                            .entry(owner.clone())
                            .or_default()
                            .insert(ty.clone());
                    }
                }
                let _ = self.methods.get(&(ty, func));
            } else if let Some(seg) = p.path.segments.last() {
                let name = seg.ident.to_string();
                if let Some(current) = &self.current {
                    if self.defined.contains_key(&name) {
                        self.internal
                            .entry(current.clone())
                            .or_default()
                            .insert(name.clone());
                    } else if let Some(owner) = self.type_map.get(&name) {
                        self.external
                            .entry(current.clone())
                            .or_default()
                            .entry(owner.clone())
                            .or_default()
                            .insert(name.clone());
                    }
                }
            }
        }
        syn::visit::visit_expr_call(self, node);
    }

    fn visit_expr_method_call(&mut self, node: &'ast syn::ExprMethodCall) {
        if let Some(current) = &self.current {
            if let Some(receiver_ty) = self.infer_expr_type(&node.receiver) {
                if self.defined.contains_key(&receiver_ty) {
                    self.internal
                        .entry(current.clone())
                        .or_default()
                        .insert(receiver_ty.clone());
                } else if let Some(owner) = self.type_map.get(&receiver_ty) {
                    self.external
                        .entry(current.clone())
                        .or_default()
                        .entry(owner.clone())
                        .or_default()
                        .insert(receiver_ty.clone());
                }
            }
        }
        syn::visit::visit_expr_method_call(self, node);
    }
}

impl<'a> DetailVisitor<'a> {
    fn infer_expr_type(&self, expr: &syn::Expr) -> Option<String> {
        match expr {
            syn::Expr::Call(call) => {
                if let syn::Expr::Path(p) = &*call.func {
                    if p.path.segments.len() >= 2 {
                        let func = p.path.segments.last().unwrap().ident.to_string();
                        let ty = p.path.segments[p.path.segments.len() - 2].ident.to_string();
                        if let Some(ret) = self.methods.get(&(ty.clone(), func.clone())) {
                            return Some(ret.clone());
                        }
                    }
                    if let Some(seg) = p.path.segments.last() {
                        let name = seg.ident.to_string();
                        if self.defined.contains_key(&name) || self.type_map.contains_key(&name) {
                            return Some(name);
                        }
                    }
                }
                None
            }
            syn::Expr::MethodCall(mc) => {
                if let Some(receiver_ty) = self.infer_expr_type(&mc.receiver) {
                    if let Some(ret) = self
                        .methods
                        .get(&(receiver_ty.clone(), mc.method.to_string()))
                    {
                        return Some(ret.clone());
                    }
                    return Some(receiver_ty);
                }

                // Try to infer from a uniquely named method when receiver type is
                // unknown. This allows chained calls like `self.inner.dao()`
                // where `dao` is defined only on a trait.
                let mut ret = None;
                for ((_, name), r) in self.methods.iter() {
                    if name == &mc.method.to_string() {
                        if ret.is_some() {
                            // Ambiguous method name
                            return None;
                        }
                        // Returning the trait or type's return type
                        ret = Some(r.clone());
                    }
                }
                ret
            }
            syn::Expr::Path(p) => {
                if p.path.segments.len() == 1 && p.path.segments[0].ident == "self" {
                    return self.current.clone();
                }
                if let Some(seg) = p.path.segments.last() {
                    let name = seg.ident.to_string();
                    if self.defined.contains_key(&name) || self.type_map.contains_key(&name) {
                        return Some(name);
                    }
                }
                None
            }
            _ => None,
        }
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
        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert_eq!(a_info.metrics.ca, 1);
        assert_eq!(a_info.metrics.ce, 0);
        assert_eq!(b_info.metrics.ce, 1);

        assert_eq!(
            b_info
                .external_depends_on
                .get("B")
                .and_then(|m| m.get("crate_a"))
                .map(|v| v.contains(&"A".to_string()))
                .unwrap_or(false),
            true
        );
        assert_eq!(
            a_info
                .external_depended_by
                .get("A")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.contains(&"B".to_string()))
                .unwrap_or(false),
            true
        );
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

        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert_eq!(a_info.metrics.ca, 1);
        assert_eq!(b_info.metrics.ce, 1);

        assert!(
            b_info
                .external_depends_on
                .get("Bar")
                .and_then(|m| m.get("crate_a"))
                .map(|v| v.contains(&"Foo".to_string()))
                .unwrap_or(false)
        );
        assert!(
            a_info
                .external_depended_by
                .get("Foo")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.contains(&"Bar".to_string()))
                .unwrap_or(false)
        );
    }

    #[test]
    fn unique_counts() {
        let src_a = "pub struct A;";
        let src_b = "pub struct B { a1: crate_a::A, a2: crate_a::A }";
        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];
        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert_eq!(b_info.metrics.ce, 1);
        assert_eq!(a_info.metrics.ca, 1);

        assert!(
            b_info
                .external_depends_on
                .get("B")
                .and_then(|m| m.get("crate_a"))
                .map(|v| v.len() == 1 && v.contains(&"A".to_string()))
                .unwrap_or(false)
        );
        assert!(
            a_info
                .external_depended_by
                .get("A")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.len() == 1 && v.contains(&"B".to_string()))
                .unwrap_or(false)
        );
    }

    #[test]
    fn method_call_dependency() {
        let src_a = r#"
            pub struct Dao;
            impl Dao {
                pub fn new() -> Self { Dao }
                pub fn delete(&self) {}
            }
        "#;
        let src_b = r#"
            pub struct Use;
            impl Use {
                pub fn run() {
                    crate_a::Dao::new().delete();
                }
            }
        "#;

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];

        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert_eq!(b_info.metrics.ce, 1);
        assert_eq!(a_info.metrics.ca, 1);

        assert!(
            b_info
                .external_depends_on
                .get("Use")
                .and_then(|m| m.get("crate_a"))
                .map(|v| v.contains(&"Dao".to_string()))
                .unwrap_or(false)
        );
        assert!(
            a_info
                .external_depended_by
                .get("Dao")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.contains(&"Use".to_string()))
                .unwrap_or(false)
        );
    }

    #[test]
    fn chained_method_call_dependency() {
        let src_a = r#"
            pub struct Dao;
            pub trait HaveDao {
                fn dao(&self) -> Dao;
            }
            impl Dao {
                pub fn delete(&self) {}
            }
        "#;
        let src_b = r#"
            use crate_a::{Dao, HaveDao};
            pub struct Use<D: HaveDao> { inner: D }
            impl<D: HaveDao> Use<D> {
                pub fn run(&self) {
                    self.inner.dao().delete();
                }
            }
        "#;

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];

        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert_eq!(b_info.metrics.ce, 2);
        assert_eq!(b_info.metrics.ca, 0);
        assert_eq!(a_info.metrics.ca, 1);

        let b_deps = b_info
            .external_depends_on
            .get("Use")
            .and_then(|m| m.get("crate_a"))
            .cloned()
            .unwrap_or_default();
        assert!(b_deps.contains(&"Dao".to_string()));
        assert!(b_deps.contains(&"HaveDao".to_string()));

        assert!(
            a_info
                .external_depended_by
                .get("Dao")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.contains(&"Use".to_string()))
                .unwrap_or(false)
        );
        assert!(
            a_info
                .external_depended_by
                .get("HaveDao")
                .and_then(|m| m.get("crate_b"))
                .map(|v| v.contains(&"Use".to_string()))
                .unwrap_or(false)
        );
    }
}
