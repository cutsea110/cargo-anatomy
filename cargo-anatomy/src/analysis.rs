use crate::metrics::{
    evaluate_metrics_with, ClassInfo, ClassKind, CrateKind, Evaluation, EvaluationThresholds,
    Metrics,
};
use crate::utils::has_test_attr;
use log::{debug, info};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use syn::{visit::Visit, File};
use walkdir::WalkDir;
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
                syn::Type::Reference(r) => from_type(&r.elem, self_ty),
                syn::Type::ImplTrait(it) => from_impl_trait(it),
                syn::Type::TraitObject(obj) => from_trait_object(obj),
                syn::Type::Paren(p) => from_type(&p.elem, self_ty),
                syn::Type::Group(g) => from_type(&g.elem, self_ty),
                syn::Type::Ptr(p) => from_type(&p.elem, self_ty),
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
fn package_source_dirs(package: &cargo_metadata::Package) -> HashSet<std::path::PathBuf> {
    let manifest_dir = package.manifest_path.parent().unwrap();
    let mut dirs = HashSet::new();
    for target in &package.targets {
        if target.kind.iter().any(|k| {
            matches!(
                k,
                cargo_metadata::TargetKind::Lib | cargo_metadata::TargetKind::Bin
            )
        }) {
            if let Some(parent) = std::path::Path::new(&target.src_path).parent() {
                dirs.insert(parent.to_path_buf());
            }
        }
    }
    if dirs.is_empty() {
        dirs.insert(manifest_dir.join("src").into());
    }
    dirs
}

fn parse_dir(dir: &std::path::Path) -> Result<Vec<File>, Box<dyn std::error::Error>> {
    let mut files = Vec::new();
    for entry in WalkDir::new(dir) {
        let entry = crate::loc_try!(entry);
        if entry.file_type().is_file()
            && entry.path().extension().map(|s| s == "rs").unwrap_or(false)
        {
            if entry.path().components().any(|c| c.as_os_str() == "tests") {
                continue;
            }
            debug!("parsing {}", entry.path().display());
            let content = crate::loc_try!(fs::read_to_string(entry.path()));
            let file = crate::loc_try!(syn::parse_file(&content));
            files.push(file);
        }
    }
    Ok(files)
}

/// Parse all Rust source files belonging to the given package.
///
/// Every library or binary target's source directory is scanned (or `src/` when no targets declare a path) and any `.rs`
/// files found are parsed into `syn::File` structures. Files located under a
/// `tests` directory are skipped since Cargo treats integration tests as
/// separate crates.
pub fn parse_package(
    package: &cargo_metadata::Package,
) -> Result<Vec<File>, Box<dyn std::error::Error>> {
    info!("reading crate {}", package.name);
    let mut files = Vec::new();
    for dir in package_source_dirs(package) {
        files.extend(parse_dir(&dir)?);
    }
    Ok(files)
}
/// Parse a package's source files and compute metrics.
///
/// The provided `workspace_types` should contain class names from all crates
/// in the workspace so that internal/external references are classified correctly.
pub fn analyze_package(
    package: &cargo_metadata::Package,
    workspace_types: &HashSet<String>,
) -> Result<Metrics, Box<dyn std::error::Error>> {
    let files = crate::loc_try!(parse_package(package));
    Ok(analyze_files(&files, workspace_types))
}
/// Analyze parsed files to produce package metrics.
///
/// `workspace_types` should contain all type names defined in the workspace so references can be counted as internal or external.
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
///
/// Each entry is a crate name paired with its parsed source files.
pub fn analyze_workspace(crates: &[(String, Vec<File>)]) -> HashMap<String, Metrics> {
    analyze_workspace_with_thresholds(crates, &EvaluationThresholds::default())
}

/// Analyse multiple crates together so cross-crate dependencies can be counted
/// using custom evaluation thresholds.
pub fn analyze_workspace_with_thresholds(
    crates: &[(String, Vec<File>)],
    t: &EvaluationThresholds,
) -> HashMap<String, Metrics> {
    analyze_workspace_details_with_thresholds(crates, t)
        .into_iter()
        .map(|(k, v)| (k, v.metrics))
        .collect()
}
/// Return metrics and dependency graphs for multiple crates.
///
/// This function performs a deeper analysis than `analyze_workspace` by tracking type-level dependencies between crates.
pub fn analyze_workspace_details(crates: &[(String, Vec<File>)]) -> HashMap<String, CrateDetail> {
    analyze_workspace_details_with_thresholds(crates, &EvaluationThresholds::default())
}

/// Return metrics and dependency graphs for multiple crates using custom thresholds.
pub fn analyze_workspace_details_with_thresholds(
    crates: &[(String, Vec<File>)],
    t: &EvaluationThresholds,
) -> HashMap<String, CrateDetail> {
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
                .filter(|(k, _)| *k != DetailVisitor::ROOT_ITEM)
                .map(|(k, v)| {
                    (
                        k.clone(),
                        v.iter()
                            .filter(|t| *t != DetailVisitor::ROOT_ITEM)
                            .cloned()
                            .collect::<Vec<_>>(),
                    )
                })
                .filter(|(_, v): &(_, Vec<String>)| !v.is_empty())
                .collect::<HashMap<_, _>>()
        };

        let to_vec_nested = |map: &HashMap<String, HashMap<String, HashSet<String>>>| {
            map.iter()
                .filter(|(k, _)| *k != DetailVisitor::ROOT_ITEM)
                .map(|(k, v)| {
                    (
                        k.clone(),
                        v.iter()
                            .filter(|(k2, _)| *k2 != DetailVisitor::ROOT_ITEM)
                            .map(|(k2, set)| {
                                (
                                    k2.clone(),
                                    set.iter()
                                        .filter(|t| *t != DetailVisitor::ROOT_ITEM)
                                        .cloned()
                                        .collect::<Vec<_>>(),
                                )
                            })
                            .filter(|(_, v)| !v.is_empty())
                            .collect::<HashMap<_, _>>(),
                    )
                })
                .filter(|(_, v)| !v.is_empty())
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
                evaluation: evaluate_metrics_with(&metrics, t),
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
///
/// Internally uses Tarjan's strongly connected components algorithm.
pub fn dependency_cycles(details: &HashMap<String, CrateDetail>) -> Vec<Vec<String>> {
    // Build adjacency list of crate -> crates it depends on
    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    for (name, detail) in details {
        let entry = graph.entry(name.clone()).or_default();
        for map in detail.external_depends_on.values() {
            for krate in map.keys() {
                entry.insert(krate.clone());
            }
        }
    }

    let graph: HashMap<String, Vec<String>> = graph
        .into_iter()
        .map(|(k, v)| (k, v.into_iter().collect()))
        .collect();

    #[allow(clippy::too_many_arguments)]
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

pub(crate) struct RefVisitor<'a> {
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

pub(crate) struct DetailVisitor<'a> {
    pub(crate) current: Option<String>,
    pub(crate) defined: &'a HashMap<String, ClassKind>,
    pub(crate) crate_name: &'a str,
    pub(crate) workspace_crates: &'a HashSet<String>,
    pub(crate) all_defined: &'a HashMap<String, HashMap<String, ClassKind>>,
    /// Map of imported identifiers to their originating crate and original name.
    /// The key is the local identifier introduced by a `use` statement. The value
    /// is a tuple of `(root crate, original identifier)` where either element may
    /// be `None` if it cannot be resolved.
    pub(crate) imports: HashMap<String, (Option<String>, Option<String>)>,
    pub(crate) internal: HashMap<String, HashSet<String>>, // from -> to
    pub(crate) external: HashMap<String, HashMap<String, HashSet<String>>>, // from -> crate -> types
    pub(crate) methods: &'a HashMap<(String, String), String>,
    pub(crate) trait_bounds: &'a HashMap<String, Vec<String>>,
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
    fn visit_item_const(&mut self, i: &'ast syn::ItemConst) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_const(self, i);
        self.current = None;
    }
    fn visit_item_static(&mut self, i: &'ast syn::ItemStatic) {
        if has_test_attr(&i.attrs) {
            return;
        }
        let name = i.ident.to_string();
        self.current = Some(name);
        syn::visit::visit_item_static(self, i);
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
            all_def: &HashMap<String, HashMap<String, ClassKind>>,
            map: &mut HashMap<String, (Option<String>, Option<String>)>,
        ) {
            match tree {
                syn::UseTree::Path(p) => {
                    let root = first.clone().unwrap_or_else(|| p.ident.to_string());
                    handle(&p.tree, Some(root), ws, all_def, map);
                }
                syn::UseTree::Name(n) => {
                    // When `first` is `None`, the use statement is importing a
                    // crate without an alias (e.g. `use foo;`). In this case
                    // `path_root` can resolve the crate directly, so we only
                    // track names when they appear as part of a path with a
                    // prefix.
                    if let Some(r) = &first {
                        if ws.contains(r) {
                            map.insert(
                                n.ident.to_string(),
                                (Some(r.clone()), Some(n.ident.to_string())),
                            );
                        } else {
                            map.insert(n.ident.to_string(), (None, None));
                        }
                    }
                }
                syn::UseTree::Rename(rn) => {
                    let root = first
                        .as_ref()
                        .cloned()
                        .unwrap_or_else(|| rn.ident.to_string());
                    if ws.contains(&root) {
                        map.insert(
                            rn.rename.to_string(),
                            (Some(root), Some(rn.ident.to_string())),
                        );
                    } else {
                        map.insert(rn.rename.to_string(), (None, None));
                    }
                }
                syn::UseTree::Group(g) => {
                    for t in &g.items {
                        handle(t, first.clone(), ws, all_def, map);
                    }
                }
                syn::UseTree::Glob(_) => {
                    if let Some(r) = &first {
                        if ws.contains(r) {
                            if let Some(defs) = all_def.get(r) {
                                for name in defs.keys() {
                                    map.insert(name.clone(), (Some(r.clone()), Some(name.clone())));
                                }
                            }
                        }
                    }
                }
            }
        }

        handle(
            &i.tree,
            None,
            self.workspace_crates,
            self.all_defined,
            &mut self.imports,
        );
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
    pub(crate) fn path_root(&self, path: &syn::Path) -> Option<String> {
        if let Some(first) = path.segments.first() {
            let ident = first.ident.to_string();
            match ident.as_str() {
                "crate" | "self" | "super" => Some(self.crate_name.to_string()),
                _ => {
                    if self.workspace_crates.contains(&ident) {
                        Some(ident)
                    } else if let Some((Some(root), _)) = self.imports.get(&ident) {
                        Some(root.clone())
                    } else {
                        None
                    }
                }
            }
        } else {
            None
        }
    }

    const ROOT_ITEM: &'static str = "__crate_root";

    fn record_use(&mut self, name: String, root: Option<String>) {
        let current = self
            .current
            .clone()
            .unwrap_or_else(|| Self::ROOT_ITEM.to_string());

        if name == current {
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
                    let lookup = if let Some((_, Some(orig))) = self.imports.get(&name) {
                        orig
                    } else {
                        &name
                    };
                    if self
                        .all_defined
                        .get(r)
                        .is_some_and(|d| d.contains_key(lookup))
                    {
                        self.external
                            .entry(current.clone())
                            .or_default()
                            .entry(r.clone())
                            .or_default()
                            .insert(lookup.to_string());
                    }
                }
            }
            None => {
                if self.defined.contains_key(&name) {
                    self.internal
                        .entry(current.clone())
                        .or_default()
                        .insert(name);
                } else if let Some((Some(import_root), orig)) = self.imports.get(&name).cloned() {
                    let lookup = orig.unwrap_or(name.clone());
                    if self
                        .all_defined
                        .get(&import_root)
                        .is_some_and(|d| d.contains_key(&lookup))
                    {
                        self.external
                            .entry(current.clone())
                            .or_default()
                            .entry(import_root.clone())
                            .or_default()
                            .insert(lookup);
                    }
                }
            }
        }
    }
    fn infer_from_call(&self, call: &syn::ExprCall) -> Option<(String, Option<String>)> {
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

    fn infer_from_method_call(&self, mc: &syn::ExprMethodCall) -> Option<(String, Option<String>)> {
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
                    if let Some(ret) = self.methods.get(&(b.clone(), mc.method.to_string())) {
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
        let mut ret = None;
        for ((_, name), r) in self.methods.iter() {
            if name == &mc.method.to_string() {
                if ret.is_some() {
                    return None;
                }
                ret = Some(r.clone());
            }
        }
        ret.map(|r| (r, None))
    }

    fn infer_from_path(&self, p: &syn::ExprPath) -> Option<(String, Option<String>)> {
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

    pub(crate) fn infer_expr_type(&self, expr: &syn::Expr) -> Option<(String, Option<String>)> {
        match expr {
            syn::Expr::Call(call) => self.infer_from_call(call),
            syn::Expr::MethodCall(mc) => self.infer_from_method_call(mc),
            syn::Expr::Path(p) => self.infer_from_path(p),
            _ => None,
        }
    }
}
