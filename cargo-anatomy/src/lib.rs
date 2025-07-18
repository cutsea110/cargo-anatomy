//! Utilities for analyzing Rust crates and computing package metrics.
use std::collections::{HashMap, HashSet};
use std::fs;

use log::{debug, info};

use serde::Serialize;
use std::io;
use std::panic::Location;
use syn::{visit::Visit, File};
use walkdir::WalkDir;

/// Wrap an error with file and line information.
#[track_caller]
pub fn error_with_location<E>(err: E) -> Box<dyn std::error::Error>
where
    E: std::fmt::Display,
{
    let loc = Location::caller();
    Box::new(io::Error::new(
        io::ErrorKind::Other,
        format!("{} at {}:{}", err, loc.file(), loc.line()),
    ))
}

/// Try expression and attach location info on error.
#[macro_export]
macro_rules! loc_try {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => {
                return Err($crate::error_with_location(err));
            }
        }
    };
}

fn has_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        if a.path().is_ident("test") {
            true
        } else if a.path().is_ident("cfg") {
            match &a.meta {
                syn::Meta::List(l) => l.tokens.to_string().contains("test"),
                _ => false,
            }
        } else {
            false
        }
    })
}

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

/// Assign qualitative labels to metrics.
pub fn evaluate_metrics(m: &Metrics) -> Evaluation {
    let a_label = if m.a >= 0.7 {
        AbstractionEval::Abstract
    } else if m.a <= 0.3 {
        AbstractionEval::Concrete
    } else {
        AbstractionEval::Mixed
    };

    let h_label = if m.h > 1.0 {
        CohesionEval::High
    } else {
        CohesionEval::Low
    };

    let i_label = if m.i >= 0.7 {
        StabilityEval::Unstable
    } else if m.i <= 0.3 {
        StabilityEval::Stable
    } else {
        StabilityEval::Moderate
    };

    let d_label = if m.d_prime <= 0.4 {
        DistanceEval::Good
    } else if m.d_prime >= 0.6 {
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

/// Detailed analysis results for a single crate.
#[derive(Debug, Serialize, Clone)]
pub struct CrateDetail {
    pub kind: CrateKind,
    pub metrics: Metrics,
    pub evaluation: Evaluation,
    pub classes: Vec<ClassInfo>,
    pub internal_depends_on: HashMap<String, Vec<String>>, // type -> types it depends on
    pub internal_depended_by: HashMap<String, Vec<String>>, // type -> types depending on it
    pub external_depends_on: HashMap<String, HashMap<String, Vec<String>>>, // type -> crate -> types
    pub external_depended_by: HashMap<String, HashMap<String, Vec<String>>>, // type -> crate -> types
}
/// Collect definitions from parsed files and count traits.
pub fn collect_defined(files: &[File]) -> (HashMap<String, ClassKind>, usize) {
    fn visit_items(
        items: &[syn::Item],
        defined: &mut HashMap<String, ClassKind>,
        abstract_count: &mut usize,
    ) {
        for item in items {
            match item {
                syn::Item::Struct(item) if !has_test_attr(&item.attrs) => {
                    defined.insert(item.ident.to_string(), ClassKind::Struct);
                }
                syn::Item::Enum(item) if !has_test_attr(&item.attrs) => {
                    defined.insert(item.ident.to_string(), ClassKind::Enum);
                }
                syn::Item::Trait(item) if !has_test_attr(&item.attrs) => {
                    defined.insert(item.ident.to_string(), ClassKind::Trait);
                    *abstract_count += 1;
                }
                syn::Item::Type(item) if !has_test_attr(&item.attrs) => {
                    defined.insert(item.ident.to_string(), ClassKind::TypeAlias);
                }
                syn::Item::Macro(item) if !has_test_attr(&item.attrs) => {
                    if let Some(id) = &item.ident {
                        defined.insert(id.to_string(), ClassKind::Macro);
                    }
                }
                syn::Item::Mod(m) if !has_test_attr(&m.attrs) => {
                    if let Some((_, items)) = &m.content {
                        visit_items(items, defined, abstract_count);
                    }
                }
                _ => {}
            }
        }
    }

    let mut defined = HashMap::new();
    let mut abstract_count = 0usize;

    for file in files {
        if has_test_attr(&file.attrs) {
            continue;
        }
        visit_items(&file.items, &mut defined, &mut abstract_count);
    }
    (defined, abstract_count)
}
/// Map method names to their return types for each impl or trait.
pub fn collect_methods(files: &[File]) -> HashMap<(String, String), String> {
    let mut map = HashMap::new();
    fn ret_ty(output: &syn::ReturnType, self_ty: &str) -> Option<String> {
        fn from_impl_trait(it: &syn::TypeImplTrait) -> Option<String> {
            for b in &it.bounds {
                if let syn::TypeParamBound::Trait(t) = b {
                    if let Some(seg) = t.path.segments.last() {
                        return Some(seg.ident.to_string());
                    }
                }
            }
            None
        }

        fn from_trait_object(obj: &syn::TypeTraitObject) -> Option<String> {
            for b in &obj.bounds {
                if let syn::TypeParamBound::Trait(t) = b {
                    if let Some(seg) = t.path.segments.last() {
                        return Some(seg.ident.to_string());
                    }
                }
            }
            None
        }

        fn from_path(p: &syn::Path, self_ty: &str) -> Option<String> {
            if let Some(last) = p.segments.last() {
                if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(t) = arg {
                            if let Some(name) = from_type(t, self_ty) {
                                return Some(name);
                            }
                        }
                    }
                }
            }
            p.segments.last().map(|s| {
                if s.ident == "Self" {
                    self_ty.to_string()
                } else {
                    s.ident.to_string()
                }
            })
        }

        fn from_type(ty: &syn::Type, self_ty: &str) -> Option<String> {
            match ty {
                syn::Type::Path(p) => from_path(&p.path, self_ty),
                syn::Type::Reference(r) => from_type(&*r.elem, self_ty),
                syn::Type::ImplTrait(it) => from_impl_trait(it),
                syn::Type::TraitObject(obj) => from_trait_object(obj),
                syn::Type::Paren(p) => from_type(&p.elem, self_ty),
                syn::Type::Group(g) => from_type(&g.elem, self_ty),
                syn::Type::Ptr(p) => from_type(&*p.elem, self_ty),
                _ => None,
            }
        }

        match output {
            syn::ReturnType::Type(_, ty) => from_type(ty, self_ty),
            _ => None,
        }
    }

    for file in files {
        if has_test_attr(&file.attrs) {
            continue;
        }
        for item in &file.items {
            match item {
                syn::Item::Impl(imp) if !has_test_attr(&imp.attrs) => {
                    if let syn::Type::Path(tp) = &*imp.self_ty {
                        if let Some(seg) = tp.path.segments.last() {
                            let self_ty = seg.ident.to_string();
                            for item in &imp.items {
                                if let syn::ImplItem::Fn(m) = item {
                                    if has_test_attr(&m.attrs) {
                                        continue;
                                    }
                                    if let Some(ret) = ret_ty(&m.sig.output, &self_ty) {
                                        map.insert((self_ty.clone(), m.sig.ident.to_string()), ret);
                                    }
                                }
                            }
                        }
                    }
                }
                syn::Item::Trait(t) if !has_test_attr(&t.attrs) => {
                    let trait_name = t.ident.to_string();
                    for item in &t.items {
                        if let syn::TraitItem::Fn(m) = item {
                            if has_test_attr(&m.attrs) {
                                continue;
                            }
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
/// Collect trait inheritance information for each trait.
pub fn collect_trait_bounds(files: &[File]) -> HashMap<String, Vec<String>> {
    let mut map = HashMap::new();
    for file in files {
        if has_test_attr(&file.attrs) {
            continue;
        }
        for item in &file.items {
            if let syn::Item::Trait(t) = item {
                if has_test_attr(&t.attrs) {
                    continue;
                }
                let name = t.ident.to_string();
                let mut bounds = Vec::new();
                for b in &t.supertraits {
                    if let syn::TypeParamBound::Trait(tb) = b {
                        if let Some(seg) = tb.path.segments.last() {
                            bounds.push(seg.ident.to_string());
                        }
                    }
                }
                map.insert(name, bounds);
            }
        }
    }
    map
}
/// Parse all Rust source files belonging to the given package.
pub fn parse_package(
    package: &cargo_metadata::Package,
) -> Result<Vec<File>, Box<dyn std::error::Error>> {
    info!("reading crate {}", package.name);
    let manifest_dir = package.manifest_path.parent().unwrap();

    let mut dirs = HashSet::new();
    for target in &package.targets {
        if target.kind.iter().any(|k| k == "lib" || k == "bin") {
            let src_path = std::path::Path::new(&target.src_path);
            if let Some(parent) = src_path.parent() {
                dirs.insert(parent.to_path_buf());
            }
        }
    }
    if dirs.is_empty() {
        dirs.insert(manifest_dir.join("src").into());
    }

    let mut files = Vec::new();
    for dir in dirs {
        for entry in WalkDir::new(dir) {
            let entry = crate::loc_try!(entry);
            if entry.file_type().is_file()
                && entry.path().extension().map(|s| s == "rs").unwrap_or(false)
            {
                // Skip integration tests located under `tests/`. Cargo treats files
                // in this directory as separate crates, so they do not represent
                // types defined by the package itself.
                // https://doc.rust-lang.org/cargo/guide/tests.html#integration-tests
                if entry.path().components().any(|c| c.as_os_str() == "tests") {
                    continue;
                }
                debug!("parsing {}", entry.path().display());
                let content = crate::loc_try!(fs::read_to_string(entry.path()));
                let file = crate::loc_try!(syn::parse_file(&content));
                files.push(file);
            }
        }
    }
    Ok(files)
}
/// Parse and analyze a single package.
pub fn analyze_package(
    package: &cargo_metadata::Package,
    workspace_types: &HashSet<String>,
) -> Result<Metrics, Box<dyn std::error::Error>> {
    let files = crate::loc_try!(parse_package(package));
    Ok(analyze_files(&files, workspace_types))
}
/// Analyze parsed files to produce package metrics.
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
        r,
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
/// Return metrics and dependency graphs for multiple crates.
pub fn analyze_workspace_details(crates: &[(String, Vec<File>)]) -> HashMap<String, CrateDetail> {
    debug!("analysing {} crates", crates.len());

    let mut crate_defined = HashMap::new();
    let mut crate_abstract = HashMap::new();
    let mut method_map: HashMap<(String, String), String> = HashMap::new();
    let mut trait_bounds: HashMap<String, Vec<String>> = HashMap::new();

    for (name, files) in crates {
        let (defined, abstract_count) = collect_defined(files);
        let methods = collect_methods(files);
        let bounds = collect_trait_bounds(files);
        method_map.extend(methods);
        for (k, v) in bounds {
            trait_bounds.insert(k, v);
        }
        crate_defined.insert(name.clone(), defined);
        crate_abstract.insert(name.clone(), abstract_count);
    }

    let mut workspace_crates: HashSet<String> = HashSet::new();
    for (name, _) in crates {
        workspace_crates.insert(name.clone());
    }

    let mut internal_refs: HashMap<String, HashMap<String, HashSet<String>>> = HashMap::new();
    let mut external_refs: HashMap<String, HashMap<String, HashMap<String, HashSet<String>>>> =
        HashMap::new();

    for (name, files) in crates {
        let defined = crate_defined.get(name).unwrap();
        let mut visitor = DetailVisitor {
            current: None,
            defined,
            crate_name: name,
            workspace_crates: &workspace_crates,
            all_defined: &crate_defined,
            imports: HashMap::new(),
            internal: HashMap::new(),
            external: HashMap::new(),
            methods: &method_map,
            trait_bounds: &trait_bounds,
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

        result.insert(name.clone(), {
            let metrics = Metrics {
                r,
                n,
                h,
                ca,
                ce,
                a,
                i,
                d,
                d_prime,
            };
            CrateDetail {
                kind: CrateKind::Workspace,
                metrics: metrics.clone(),
                evaluation: evaluate_metrics(&metrics),
                classes,
                internal_depends_on: to_vec_map(internal_refs.get(name).unwrap_or(&HashMap::new())),
                internal_depended_by: to_vec_map(internal_rev.get(name).unwrap_or(&HashMap::new())),
                external_depends_on: to_vec_nested(
                    external_refs.get(name).unwrap_or(&HashMap::new()),
                ),
                external_depended_by: to_vec_nested(
                    external_rev.get(name).unwrap_or(&HashMap::new()),
                ),
            }
        });
    }

    result
}

/// Determine dependency cycles between crates based on analysis details.
pub fn dependency_cycles(details: &HashMap<String, CrateDetail>) -> Vec<Vec<String>> {
    // Build adjacency list of crate -> crates it depends on
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    for (name, detail) in details {
        let entry = graph.entry(name.clone()).or_default();
        for map in detail.external_depends_on.values() {
            for (krate, _) in map {
                entry.insert(krate.clone());
            }
        }
    }

    let graph: HashMap<String, Vec<String>> = graph
        .into_iter()
        .map(|(k, v)| (k, v.into_iter().collect()))
        .collect();

    fn strongconnect(
        v: &String,
        index: &mut usize,
        stack: &mut Vec<String>,
        indices: &mut HashMap<String, usize>,
        lowlink: &mut HashMap<String, usize>,
        on_stack: &mut HashSet<String>,
        graph: &HashMap<String, Vec<String>>,
        result: &mut Vec<Vec<String>>,
    ) {
        indices.insert(v.clone(), *index);
        lowlink.insert(v.clone(), *index);
        *index += 1;
        stack.push(v.clone());
        on_stack.insert(v.clone());

        if let Some(neigh) = graph.get(v) {
            for w in neigh {
                if !indices.contains_key(w) {
                    strongconnect(w, index, stack, indices, lowlink, on_stack, graph, result);
                    let lw = *lowlink.get(w).unwrap();
                    let lv = *lowlink.get(v).unwrap();
                    if lw < lv {
                        lowlink.insert(v.clone(), lw);
                    }
                } else if on_stack.contains(w) {
                    let iw = *indices.get(w).unwrap();
                    let lv = *lowlink.get(v).unwrap();
                    if iw < lv {
                        lowlink.insert(v.clone(), iw);
                    }
                }
            }
        }

        if indices.get(v) == lowlink.get(v) {
            let mut scc = Vec::new();
            while let Some(w) = stack.pop() {
                on_stack.remove(&w);
                scc.push(w.clone());
                if &w == v {
                    break;
                }
            }
            if scc.len() > 1 {
                scc.reverse();
                result.push(scc);
            }
        }
    }

    let mut index = 0usize;
    let mut stack = Vec::new();
    let mut indices: HashMap<String, usize> = HashMap::new();
    let mut lowlink: HashMap<String, usize> = HashMap::new();
    let mut on_stack: HashSet<String> = HashSet::new();
    let mut result_vec = Vec::new();

    for v in graph.keys() {
        if !indices.contains_key(v) {
            strongconnect(
                v,
                &mut index,
                &mut stack,
                &mut indices,
                &mut lowlink,
                &mut on_stack,
                &graph,
                &mut result_vec,
            );
        }
    }

    result_vec
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
    crate_name: &'a str,
    workspace_crates: &'a HashSet<String>,
    all_defined: &'a HashMap<String, HashMap<String, ClassKind>>,
    imports: HashMap<String, Option<String>>,
    internal: HashMap<String, HashSet<String>>, // from -> to
    external: HashMap<String, HashMap<String, HashSet<String>>>, // from -> crate -> types
    methods: &'a HashMap<(String, String), String>,
    trait_bounds: &'a HashMap<String, Vec<String>>,
}

impl<'ast> Visit<'ast> for DetailVisitor<'_> {
    fn visit_file(&mut self, i: &'ast syn::File) {
        if has_test_attr(&i.attrs) {
            return;
        }
        syn::visit::visit_file(self, i);
    }
    fn visit_item_struct(&mut self, i: &'ast syn::ItemStruct) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_struct(self, i);
        self.current = None;
    }
    fn visit_item_enum(&mut self, i: &'ast syn::ItemEnum) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_enum(self, i);
        self.current = None;
    }
    fn visit_item_trait(&mut self, i: &'ast syn::ItemTrait) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_trait(self, i);
        self.current = None;
    }
    fn visit_item_type(&mut self, i: &'ast syn::ItemType) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_type(self, i);
        self.current = None;
    }
    fn visit_item_impl(&mut self, i: &'ast syn::ItemImpl) {
        if has_test_attr(&i.attrs) {
            return;
        }
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
    fn visit_item_fn(&mut self, i: &'ast syn::ItemFn) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.sig.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_fn(self, i);
        self.current = None;
    }
    fn visit_item_mod(&mut self, i: &'ast syn::ItemMod) {
        if has_test_attr(&i.attrs) {
            return;
        }
        syn::visit::visit_item_mod(self, i);
    }
    fn visit_item_use(&mut self, i: &'ast syn::ItemUse) {
        fn handle(
            tree: &syn::UseTree,
            first: Option<String>,
            ws: &HashSet<String>,
            map: &mut HashMap<String, Option<String>>,
        ) {
            match tree {
                syn::UseTree::Path(p) => {
                    let root = first.clone().unwrap_or_else(|| p.ident.to_string());
                    handle(&p.tree, Some(root), ws, map);
                }
                syn::UseTree::Name(n) => {
                    if let Some(r) = &first {
                        if ws.contains(r) {
                            map.insert(n.ident.to_string(), Some(r.clone()));
                        } else {
                            map.insert(n.ident.to_string(), None);
                        }
                    }
                }
                syn::UseTree::Rename(rn) => {
                    if let Some(r) = &first {
                        if ws.contains(r) {
                            map.insert(rn.rename.to_string(), Some(r.clone()));
                        } else {
                            map.insert(rn.rename.to_string(), None);
                        }
                    }
                }
                syn::UseTree::Group(g) => {
                    for t in &g.items {
                        handle(t, first.clone(), ws, map);
                    }
                }
                _ => {}
            }
        }

        handle(&i.tree, None, self.workspace_crates, &mut self.imports);
        syn::visit::visit_item_use(self, i);
    }
    fn visit_path(&mut self, node: &'ast syn::Path) {
        if let Some(seg) = node.segments.last() {
            let name = seg.ident.to_string();
            let root = self.path_root(node);
            self.record_use(name, root);
        }
        syn::visit::visit_path(self, node);
    }

    fn visit_expr_call(&mut self, node: &'ast syn::ExprCall) {
        if let syn::Expr::Path(p) = &*node.func {
            if p.path.segments.len() >= 2 {
                let func = p.path.segments.last().unwrap().ident.to_string();
                let ty = p.path.segments[p.path.segments.len() - 2].ident.to_string();
                let root = self.path_root(&p.path);
                self.record_use(ty.clone(), root.clone());
                let _ = self.methods.get(&(ty, func));
            } else if let Some(seg) = p.path.segments.last() {
                let name = seg.ident.to_string();
                let root = self.path_root(&p.path);
                self.record_use(name, root);
            }
        }
        syn::visit::visit_expr_call(self, node);
    }

    fn visit_expr_macro(&mut self, node: &'ast syn::ExprMacro) {
        if let Some(seg) = node.mac.path.segments.last() {
            let name = seg.ident.to_string();
            let root = self.path_root(&node.mac.path);
            self.record_use(name, root);
        }
        syn::visit::visit_expr_macro(self, node);
    }

    fn visit_item_macro(&mut self, i: &'ast syn::ItemMacro) {
        if i.ident.is_none() {
            if let Some(seg) = i.mac.path.segments.last() {
                let name = seg.ident.to_string();
                let root = self.path_root(&i.mac.path);
                self.record_use(name, root);
            }
        }
        syn::visit::visit_item_macro(self, i);
    }

    fn visit_expr_method_call(&mut self, node: &'ast syn::ExprMethodCall) {
        if let Some((receiver_ty, root)) = self.infer_expr_type(&node.receiver) {
            self.record_use(receiver_ty, root);
        }
        syn::visit::visit_expr_method_call(self, node);
    }
}

impl<'a> DetailVisitor<'a> {
    fn path_root(&self, path: &syn::Path) -> Option<String> {
        if let Some(first) = path.segments.first() {
            let ident = first.ident.to_string();
            match ident.as_str() {
                "crate" | "self" | "super" => Some(self.crate_name.to_string()),
                _ => {
                    if self.workspace_crates.contains(&ident) {
                        Some(ident)
                    } else {
                        None
                    }
                }
            }
        } else {
            None
        }
    }

    fn record_use(&mut self, name: String, root: Option<String>) {
        if let Some(current) = &self.current {
            if name == *current {
                return;
            }
            match root {
                Some(ref r) if r == self.crate_name => {
                    if self.defined.contains_key(&name) {
                        self.internal
                            .entry(current.clone())
                            .or_default()
                            .insert(name);
                    }
                }
                Some(ref r) => {
                    if self.workspace_crates.contains(r) {
                        if self
                            .all_defined
                            .get(r)
                            .map_or(false, |d| d.contains_key(&name))
                        {
                            self.external
                                .entry(current.clone())
                                .or_default()
                                .entry(r.clone())
                                .or_default()
                                .insert(name);
                        }
                    }
                }
                None => {
                    if self.defined.contains_key(&name) {
                        self.internal
                            .entry(current.clone())
                            .or_default()
                            .insert(name);
                    } else if let Some(import_root) = self.imports.get(&name).cloned().flatten() {
                        if self
                            .all_defined
                            .get(&import_root)
                            .map_or(false, |d| d.contains_key(&name))
                        {
                            self.external
                                .entry(current.clone())
                                .or_default()
                                .entry(import_root.clone())
                                .or_default()
                                .insert(name);
                        }
                    }
                }
            }
        }
    }

    fn infer_expr_type(&self, expr: &syn::Expr) -> Option<(String, Option<String>)> {
        match expr {
            syn::Expr::Call(call) => {
                if let syn::Expr::Path(p) = &*call.func {
                    if p.path.segments.len() >= 2 {
                        let func = p.path.segments.last().unwrap().ident.to_string();
                        let ty = p.path.segments[p.path.segments.len() - 2].ident.to_string();
                        if let Some(ret) = self.methods.get(&(ty.clone(), func.clone())) {
                            let root = self.path_root(&p.path);
                            return Some((ret.clone(), root));
                        }
                    }
                    if let Some(seg) = p.path.segments.last() {
                        let name = seg.ident.to_string();
                        if self.defined.contains_key(&name)
                            || self.all_defined.values().any(|d| d.contains_key(&name))
                        {
                            let root = self.path_root(&p.path);
                            return Some((name, root));
                        }
                    }
                }
                None
            }
            syn::Expr::MethodCall(mc) => {
                if let Some((receiver_ty, root)) = self.infer_expr_type(&mc.receiver) {
                    if let Some(ret) = self
                        .methods
                        .get(&(receiver_ty.clone(), mc.method.to_string()))
                    {
                        return Some((ret.clone(), root));
                    }
                    if let Some(bounds) = self.trait_bounds.get(&receiver_ty) {
                        let mut found = None;
                        for b in bounds {
                            if let Some(ret) = self.methods.get(&(b.clone(), mc.method.to_string()))
                            {
                                if found.is_some() {
                                    return None;
                                }
                                found = Some(ret.clone());
                            }
                        }
                        if let Some(ret) = found {
                            return Some((ret, None));
                        }
                    }
                    return Some((receiver_ty, root));
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
                ret.map(|r| (r, None))
            }
            syn::Expr::Path(p) => {
                if p.path.segments.len() == 1 && p.path.segments[0].ident == "self" {
                    return self
                        .current
                        .as_ref()
                        .map(|c| (c.clone(), Some(self.crate_name.to_string())));
                }
                if let Some(seg) = p.path.segments.last() {
                    let name = seg.ident.to_string();
                    if self.defined.contains_key(&name)
                        || self.all_defined.values().any(|d| d.contains_key(&name))
                    {
                        let root = self.path_root(&p.path);
                        return Some((name, root));
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
        assert!(a_info
            .external_depended_by
            .get("A")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"B".to_string()))
            .unwrap_or(false));
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

        assert!(b_info
            .external_depends_on
            .get("Bar")
            .and_then(|m| m.get("crate_a"))
            .map(|v| v.contains(&"Foo".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("Foo")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Bar".to_string()))
            .unwrap_or(false));
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

        assert!(b_info
            .external_depends_on
            .get("B")
            .and_then(|m| m.get("crate_a"))
            .map(|v| v.len() == 1 && v.contains(&"A".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("A")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.len() == 1 && v.contains(&"B".to_string()))
            .unwrap_or(false));
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

        assert!(b_info
            .external_depends_on
            .get("Use")
            .and_then(|m| m.get("crate_a"))
            .map(|v| v.contains(&"Dao".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("Dao")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Use".to_string()))
            .unwrap_or(false));
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

        assert!(a_info
            .external_depended_by
            .get("Dao")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Use".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("HaveDao")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Use".to_string()))
            .unwrap_or(false));
    }

    #[test]
    fn dyn_trait_return() {
        let src_a = r#"
            pub trait Dao { fn delete(&self); }
            pub trait HaveDao { fn dao(&self) -> Box<dyn Dao>; }
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
        assert_eq!(a_info.metrics.ca, 1);

        let b_deps = b_info
            .external_depends_on
            .get("Use")
            .and_then(|m| m.get("crate_a"))
            .cloned()
            .unwrap_or_default();
        assert!(b_deps.contains(&"Dao".to_string()));
        assert!(b_deps.contains(&"HaveDao".to_string()));

        assert!(a_info
            .external_depended_by
            .get("Dao")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Use".to_string()))
            .unwrap_or(false));
    }

    #[test]
    fn ignore_non_workspace_crate() {
        let src_a = "pub struct Tx;";
        let src_b = "use tx_rs::Tx; pub struct Use { t: Tx }";
        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
        ];

        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert_eq!(b_info.metrics.ce, 0);
        assert_eq!(a_info.metrics.ca, 0);
        assert!(b_info.external_depends_on.is_empty());
        assert!(a_info.external_depended_by.is_empty());
    }

    #[test]
    fn struct_usage_in_trait() {
        let src_a = r#"
            pub struct Paycheck;
            impl Paycheck { pub fn new() -> Self { Paycheck } }
        "#;
        let src_c = "pub struct Paycheck;";
        let src_b = r#"
            use crate_a::Paycheck;
            pub trait Payday {
                fn run(&self) {
                    self.run_tx(|_| {
                        let _ = Paycheck::new();
                    });
                }
                fn run_tx<F>(&self, f: F) where F: FnOnce(i32) {}
            }
        "#;

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();
        let file_c: syn::File = syn::parse_str(src_c).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a.clone()]),
            ("crate_b".to_string(), vec![file_b.clone()]),
            ("crate_c".to_string(), vec![file_c.clone()]),
        ];

        let info = analyze_workspace_details(&crates);
        let a_info = info.get("crate_a").unwrap();
        let b_info = info.get("crate_b").unwrap();

        assert!(b_info
            .external_depends_on
            .get("Payday")
            .and_then(|m| m.get("crate_a"))
            .map(|v| v.contains(&"Paycheck".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("Paycheck")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Payday".to_string()))
            .unwrap_or(false));
    }
    #[test]
    fn r_counts_unique_edges() {
        let src = "pub struct B; pub struct A { b1: B, b2: B }";
        let file: syn::File = syn::parse_str(src).unwrap();
        let crates = vec![("crate_a".to_string(), vec![file.clone()])];
        let info = analyze_workspace_details(&crates);
        let a = info.get("crate_a").unwrap();
        assert_eq!(a.metrics.r, 1);
        let deps = a.internal_depends_on.get("A").cloned().unwrap_or_default();
        assert_eq!(deps.len(), 1);
        assert!(deps.contains(&"B".to_string()));
    }

    #[test]
    fn r_multiple_edges() {
        let src = "pub struct B; pub struct C { b1: B, b2: B } pub struct A { b: B, c: C }";
        let file: syn::File = syn::parse_str(src).unwrap();
        let crates = vec![("crate_a".to_string(), vec![file.clone()])];
        let info = analyze_workspace_details(&crates);
        let a = info.get("crate_a").unwrap();
        assert_eq!(a.metrics.r, 3);
        let a_deps = a.internal_depends_on.get("A").cloned().unwrap_or_default();
        let c_deps = a.internal_depends_on.get("C").cloned().unwrap_or_default();
        assert!(a_deps.contains(&"B".to_string()));
        assert!(c_deps.contains(&"B".to_string()));
    }

    #[test]
    fn r_counts_method_body() {
        let src = "pub struct B; pub struct A; impl A { fn make() -> B { B } }";
        let file: syn::File = syn::parse_str(src).unwrap();
        let crates = vec![("crate_a".to_string(), vec![file.clone()])];
        let info = analyze_workspace_details(&crates);
        let a = info.get("crate_a").unwrap();
        assert_eq!(a.metrics.r, 1);
        let deps = a.internal_depends_on.get("A").cloned().unwrap_or_default();
        assert_eq!(deps.len(), 1);
        assert!(deps.contains(&"B".to_string()));
    }

    #[test]
    fn free_function_dependency() {
        let src_a = "pub struct Helper;";
        let src_b = "use crate_a::Helper; fn main() { let _ = Helper; }";
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

        assert!(b_info
            .external_depends_on
            .get("main")
            .and_then(|m| m.get("crate_a"))
            .map(|v| v.contains(&"Helper".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("Helper")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"main".to_string()))
            .unwrap_or(false));
    }

    #[test]
    fn module_metrics() {
        let root = r#"
            mod foo;
            pub mod bar;

            pub struct Root {
                f: foo::Foo,
                b: bar::Bar,
            }
        "#;
        let foo = "pub struct Foo;";
        let bar = "pub struct Bar;";

        let file_root: syn::File = syn::parse_str(root).unwrap();
        let file_foo: syn::File = syn::parse_str(foo).unwrap();
        let file_bar: syn::File = syn::parse_str(bar).unwrap();

        let defs = collect_defined(&[file_root.clone(), file_foo.clone(), file_bar.clone()]);
        let workspace: HashSet<String> = defs.0.keys().cloned().collect();
        let metrics = analyze_files(&[file_root, file_foo, file_bar], &workspace);

        assert_eq!(metrics.n, 3);
        assert_eq!(metrics.r, 2);
        assert_eq!(metrics.ce, 0);
        assert_eq!(metrics.ca, 0);
    }

    #[test]
    fn inline_module_metrics() {
        let src = r#"
            mod foo {
                pub struct Foo;
            }
            pub mod bar {
                pub struct Bar;
            }

            pub struct Root {
                f: foo::Foo,
                b: bar::Bar,
            }
        "#;

        let file: syn::File = syn::parse_str(src).unwrap();
        let defs = collect_defined(&[file.clone()]);
        let workspace: HashSet<String> = defs.0.keys().cloned().collect();
        let metrics = analyze_files(&[file], &workspace);

        assert_eq!(metrics.n, 3);
        assert_eq!(metrics.r, 2);
        assert_eq!(metrics.ce, 0);
        assert_eq!(metrics.ca, 0);
    }

    #[test]
    fn macro_dependencies() {
        let src_a = r#"
            #[macro_export]
            macro_rules! my_macro {
                () => {};
            }
        "#;
        let src_b = r#"
            pub struct Use;
            impl Use {
                pub fn run() {
                    crate_a::my_macro!();
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

        assert!(b_info
            .external_depends_on
            .get("Use")
            .and_then(|m| m.get("crate_a"))
            .map(|v| v.contains(&"my_macro".to_string()))
            .unwrap_or(false));
        assert!(a_info
            .external_depended_by
            .get("my_macro")
            .and_then(|m| m.get("crate_b"))
            .map(|v| v.contains(&"Use".to_string()))
            .unwrap_or(false));
    }

    #[test]
    fn evaluate_metrics_thresholds() {
        let m = Metrics {
            r: 0,
            n: 0,
            h: 1.1,
            ca: 0,
            ce: 0,
            a: 0.8,
            i: 0.8,
            d: 0.0,
            d_prime: 0.7,
        };
        let eval = evaluate_metrics(&m);
        assert!(matches!(eval.a, AbstractionEval::Abstract));
        assert!(matches!(eval.h, CohesionEval::High));
        assert!(matches!(eval.i, StabilityEval::Unstable));
        assert!(matches!(eval.d_prime, DistanceEval::Useless));

        let m = Metrics {
            r: 0,
            n: 0,
            h: 0.8,
            ca: 0,
            ce: 0,
            a: 0.2,
            i: 0.2,
            d: 0.0,
            d_prime: 0.7,
        };
        let eval = evaluate_metrics(&m);
        assert!(matches!(eval.a, AbstractionEval::Concrete));
        assert!(matches!(eval.h, CohesionEval::Low));
        assert!(matches!(eval.i, StabilityEval::Stable));
        assert!(matches!(eval.d_prime, DistanceEval::Painful));

        let m = Metrics {
            r: 0,
            n: 0,
            h: 1.0,
            ca: 0,
            ce: 0,
            a: 0.5,
            i: 0.5,
            d: 0.0,
            d_prime: 0.5,
        };
        let eval = evaluate_metrics(&m);
        assert!(matches!(eval.a, AbstractionEval::Mixed));
        assert!(matches!(eval.h, CohesionEval::Low));
        assert!(matches!(eval.i, StabilityEval::Moderate));
        assert!(matches!(eval.d_prime, DistanceEval::Balanced));

        let m = Metrics {
            r: 0,
            n: 0,
            h: 1.0,
            ca: 0,
            ce: 0,
            a: 0.5,
            i: 0.5,
            d: 0.0,
            d_prime: 0.3,
        };
        let eval = evaluate_metrics(&m);
        assert!(matches!(eval.d_prime, DistanceEval::Good));
    }

    #[test]
    fn detects_two_crate_cycle() {
        let src_a = "use crate_b::B; pub struct A(B);";
        let src_b = "use crate_a::A; pub struct B(A);";

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a]),
            ("crate_b".to_string(), vec![file_b]),
        ];

        let info = analyze_workspace_details(&crates);
        let cycles = dependency_cycles(&info);
        assert_eq!(cycles.len(), 1);
        let cyc = &cycles[0];
        assert!(cyc.contains(&"crate_a".to_string()));
        assert!(cyc.contains(&"crate_b".to_string()));
    }

    #[test]
    fn detects_three_crate_cycle() {
        let src_a = "use crate_b::B; pub struct A(B);";
        let src_b = "use crate_c::C; pub struct B(C);";
        let src_c = "use crate_a::A; pub struct C(A);";

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();
        let file_c: syn::File = syn::parse_str(src_c).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a]),
            ("crate_b".to_string(), vec![file_b]),
            ("crate_c".to_string(), vec![file_c]),
        ];

        let info = analyze_workspace_details(&crates);
        let cycles = dependency_cycles(&info);
        assert_eq!(cycles.len(), 1);
        let cyc = &cycles[0];
        assert!(cyc.contains(&"crate_a".to_string()));
        assert!(cyc.contains(&"crate_b".to_string()));
        assert!(cyc.contains(&"crate_c".to_string()));
    }

    #[test]
    fn unrelated_crate_not_included() {
        let src_a = "use crate_b::B; pub struct A(B);";
        let src_b = "use crate_a::A; pub struct B(A);";
        let src_c = "pub struct C;";

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();
        let file_c: syn::File = syn::parse_str(src_c).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a]),
            ("crate_b".to_string(), vec![file_b]),
            ("crate_c".to_string(), vec![file_c]),
        ];

        let info = analyze_workspace_details(&crates);
        let cycles = dependency_cycles(&info);
        assert_eq!(cycles.len(), 1);
        let cyc = &cycles[0];
        assert!(cyc.contains(&"crate_a".to_string()));
        assert!(cyc.contains(&"crate_b".to_string()));
        assert!(!cyc.contains(&"crate_c".to_string()));
    }

    #[test]
    fn detects_cycle_via_method_calls() {
        let src_a = r#"
            pub struct A;
            impl A {
                pub fn call_b() {
                    crate_b::B::bar();
                }
                pub fn bar() {}
            }
        "#;
        let src_b = r#"
            pub struct B;
            impl B {
                pub fn bar() {
                    crate_a::A::call_b();
                }
            }
        "#;

        let file_a: syn::File = syn::parse_str(src_a).unwrap();
        let file_b: syn::File = syn::parse_str(src_b).unwrap();

        let crates = vec![
            ("crate_a".to_string(), vec![file_a]),
            ("crate_b".to_string(), vec![file_b]),
        ];

        let info = analyze_workspace_details(&crates);
        let cycles = dependency_cycles(&info);
        assert_eq!(cycles.len(), 1);
        let cyc = &cycles[0];
        assert!(cyc.contains(&"crate_a".to_string()));
        assert!(cyc.contains(&"crate_b".to_string()));
    }
}
