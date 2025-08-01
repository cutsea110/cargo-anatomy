use crate::{
    analysis::{
        analyze_files, analyze_workspace_details, collect_defined, dependency_cycles,
        parse_package, DetailVisitor,
    },
    metrics::{
        evaluate_metrics, AbstractionEval, ClassKind, CohesionEval, DistanceEval, Metrics,
        StabilityEval,
    },
};
use std::collections::HashSet;

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
fn same_name_internal_external_dependency() {
    let src_a = "pub struct Foo;";
    let src_b = "pub struct Foo; pub struct Bar { ext: crate_a::Foo, int: Foo }";

    let file_a: syn::File = syn::parse_str(src_a).unwrap();
    let file_b: syn::File = syn::parse_str(src_b).unwrap();

    let crates = vec![
        ("crate_a".to_string(), vec![file_a.clone()]),
        ("crate_b".to_string(), vec![file_b.clone()]),
    ];

    let info = analyze_workspace_details(&crates);
    let b_info = info.get("crate_b").unwrap();

    assert!(b_info
        .internal_depends_on
        .get("Bar")
        .map(|v| v.contains(&"Foo".to_string()))
        .unwrap_or(false));
    assert!(b_info
        .external_depends_on
        .get("Bar")
        .and_then(|m| m.get("crate_a"))
        .map(|v| v.contains(&"Foo".to_string()))
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
fn async_function_dependency() {
    let src_a = "pub struct Helper;";
    let src_b = "use crate_a::Helper; async fn run() { let _ = Helper; }";
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
        .get("run")
        .and_then(|m| m.get("crate_a"))
        .map(|v| v.contains(&"Helper".to_string()))
        .unwrap_or(false));
    assert!(a_info
        .external_depended_by
        .get("Helper")
        .and_then(|m| m.get("crate_b"))
        .map(|v| v.contains(&"run".to_string()))
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
}

#[test]
fn glob_import_dependency() {
    let src_a = r#"
        pub struct Foo;
    "#;
    let src_b = r#"
        use crate_a::*;
        pub struct Use(Foo);
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
}

#[test]
fn top_level_macro_invocation() {
    let src_a = r#"
        #[macro_export]
        macro_rules! my_macro {
            () => {};
        }
    "#;
    let src_b = r#"
        use crate_a::*;
        my_macro!();
        fn main() {}
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
}

#[test]
fn const_dependency() {
    let src_a = r#"
        pub enum Buz { X }
    "#;
    let src_b = r#"
        pub const FOO: &crate_a::Buz = &crate_a::Buz::X;
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
}

#[test]
fn import_const_dependency() {
    let src_a = r#"
        pub mod foo {
            pub enum Foo { A }
        }
    "#;
    let src_c = r#"
        use crate_a::foo;
        pub const X: foo::Foo = foo::Foo::A;
    "#;

    let file_a: syn::File = syn::parse_str(src_a).unwrap();
    let file_c: syn::File = syn::parse_str(src_c).unwrap();

    let crates = vec![
        ("crate_a".to_string(), vec![file_a.clone()]),
        ("crate_c".to_string(), vec![file_c.clone()]),
    ];

    let info = analyze_workspace_details(&crates);
    let a_info = info.get("crate_a").unwrap();
    let c_info = info.get("crate_c").unwrap();

    assert_eq!(c_info.metrics.ce, 1);
    assert_eq!(a_info.metrics.ca, 1);
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

#[test]
fn parse_package_ignores_tests_dir() {
    use cargo_metadata::MetadataCommand;
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("pkg/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("pkg/tests")).unwrap();
    std::fs::write(
        dir.path().join("pkg/Cargo.toml"),
        "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("pkg/src/lib.rs"), "pub struct Foo;\n").unwrap();
    std::fs::write(
        dir.path().join("pkg/tests/integration.rs"),
        "pub struct Bar;\n",
    )
    .unwrap();
    let metadata = MetadataCommand::new()
        .no_deps()
        .current_dir(dir.path().join("pkg"))
        .exec()
        .unwrap();
    let package = metadata.packages.first().unwrap();
    let files = parse_package(package).unwrap();
    assert_eq!(files.len(), 1);
}

#[test]
fn lib_and_main_dependency() {
    use cargo_metadata::MetadataCommand;
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("dep/src")).unwrap();
    std::fs::write(
        dir.path().join("dep/Cargo.toml"),
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("dep/src/lib.rs"), "pub struct Dep;\n").unwrap();

    std::fs::create_dir_all(dir.path().join("app/src")).unwrap();
    std::fs::write(
        dir.path().join("app/Cargo.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\ndep = { path = \"../dep\" }\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("app/src/lib.rs"), "pub struct App;\n").unwrap();
    std::fs::write(
        dir.path().join("app/src/main.rs"),
        "use dep::Dep; fn main() { let _ = Dep; }\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"app\", \"dep\"]\n",
    )
    .unwrap();

    let metadata = MetadataCommand::new()
        .no_deps()
        .current_dir(dir.path())
        .exec()
        .unwrap();
    let mut crates = Vec::new();
    for pkg in &metadata.packages {
        crates.push((pkg.name.as_str().to_string(), parse_package(pkg).unwrap()));
    }

    let info = analyze_workspace_details(&crates);
    assert_eq!(info["app"].metrics.ce, 1);
    assert_eq!(info["dep"].metrics.ca, 1);
}

#[test]
fn path_dep_import_dependency() {
    use cargo_metadata::MetadataCommand;
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("a/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("b/c/src")).unwrap();
    std::fs::create_dir_all(dir.path().join("b/src")).unwrap();

    std::fs::write(
        dir.path().join("a/Cargo.toml"),
        "[package]\nname = \"a\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("a/src/lib.rs"), "pub mod foo;\n").unwrap();
    std::fs::write(
        dir.path().join("a/src/foo.rs"),
        "#[derive(Debug)]\npub enum Foo { A }\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("b/Cargo.toml"),
        "[package]\nname = \"b\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\na = { path = \"../a\" }\nc = { path = \"./c\" }\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("b/src/lib.rs"), "pub use c;\n").unwrap();

    std::fs::write(
        dir.path().join("b/c/Cargo.toml"),
        "[package]\nname = \"c\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\na = { path = \"../../a\" }\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("b/c/src/lib.rs"),
        "use a::foo;\npub const X: foo::Foo = foo::Foo::A;\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"a\", \"b\"]\nresolver = \"3\"\n",
    )
    .unwrap();

    let metadata = MetadataCommand::new()
        .no_deps()
        .current_dir(dir.path())
        .exec()
        .unwrap();
    let mut crates = Vec::new();
    for pkg in &metadata.packages {
        crates.push((pkg.name.as_str().to_string(), parse_package(pkg).unwrap()));
    }

    let info = analyze_workspace_details(&crates);
    assert_eq!(info["c"].metrics.ce, 1);
    assert_eq!(info["a"].metrics.ca, 1);
}

#[test]
fn external_crate_alias() {
    use cargo_metadata::MetadataCommand;
    let dir = tempfile::tempdir().unwrap();
    let external = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(external.path().join("src")).unwrap();
    std::fs::write(
        external.path().join("Cargo.toml"),
        "[package]\nname = \"foo_bar\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(external.path().join("src/lib.rs"), "pub struct FooBar;\n").unwrap();

    std::fs::create_dir(dir.path().join("app")).unwrap();
    std::fs::create_dir(dir.path().join("app/src")).unwrap();
    std::fs::write(
        dir.path().join("app/Cargo.toml"),
        format!(
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\nfoo_bar = {{ path = \"{}\" }}\n",
            external.path().display()
        ),
    )
    .unwrap();
    std::fs::write(
        dir.path().join("app/src/lib.rs"),
        "use foo_bar as foo; pub struct App { d: foo::FooBar }\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"app\"]\n",
    )
    .unwrap();

    let metadata_app = MetadataCommand::new()
        .no_deps()
        .current_dir(dir.path().join("app"))
        .exec()
        .unwrap();
    let app_pkg = metadata_app.packages.first().unwrap();
    let app_files = parse_package(app_pkg).unwrap();

    let metadata_dep = MetadataCommand::new()
        .no_deps()
        .current_dir(external.path())
        .exec()
        .unwrap();
    let dep_pkg = metadata_dep.packages.first().unwrap();
    let dep_files = parse_package(dep_pkg).unwrap();

    let crates = vec![
        ("app".to_string(), app_files),
        ("foo_bar".to_string(), dep_files),
    ];

    let info = analyze_workspace_details(&crates);
    let app = info.get("app").unwrap();
    let dep = info.get("foo_bar").unwrap();
    assert_eq!(app.metrics.ce, 1);
    assert_eq!(dep.metrics.ca, 1);
    assert_eq!(dep.metrics.ce, 0);
    assert!(app
        .external_depends_on
        .get("App")
        .and_then(|m| m.get("foo_bar"))
        .map(|v| v.contains(&"FooBar".to_string()))
        .unwrap_or(false));
}

#[test]
fn type_alias_dependency() {
    use cargo_metadata::MetadataCommand;
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("a/src")).unwrap();
    std::fs::write(
        dir.path().join("a/Cargo.toml"),
        "[package]\nname = \"a\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("a/src/lib.rs"), "pub struct Cache;\n").unwrap();

    std::fs::create_dir_all(dir.path().join("b/src")).unwrap();
    std::fs::write(
        dir.path().join("b/Cargo.toml"),
        "[package]\nname = \"b\"\nversion = \"0.1.0\"\n\n[[bin]]\nname = \"b\"\n",
    )
    .unwrap();
    std::fs::write(
        dir.path().join("b/src/main.rs"),
        "use a::Cache as CacheImpl; fn main() { let _ = CacheImpl; }\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"b\", \"a\"]\n",
    )
    .unwrap();

    let metadata = MetadataCommand::new()
        .no_deps()
        .current_dir(dir.path())
        .exec()
        .unwrap();
    let mut crates = Vec::new();
    for pkg in &metadata.packages {
        crates.push((pkg.name.as_str().to_string(), parse_package(pkg).unwrap()));
    }

    let info = analyze_workspace_details(&crates);
    assert_eq!(info["b"].metrics.ce, 1);
    assert_eq!(info["a"].metrics.ca, 1);
}

#[test]
fn bin_target_dependency() {
    use cargo_metadata::MetadataCommand;
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(dir.path().join("dep/src")).unwrap();
    std::fs::write(
        dir.path().join("dep/Cargo.toml"),
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("dep/src/lib.rs"), "pub struct Dep;\n").unwrap();

    std::fs::create_dir_all(dir.path().join("app/src/bin")).unwrap();
    std::fs::write(
        dir.path().join("app/Cargo.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\ndep = { path = \"../dep\" }\n\n[[bin]]\nname = \"cli\"\npath = \"src/bin/cli.rs\"\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("app/src/lib.rs"), "pub struct App;\n").unwrap();
    std::fs::write(
        dir.path().join("app/src/bin/cli.rs"),
        "use dep::Dep; fn main() { let _ = Dep; }\n",
    )
    .unwrap();

    std::fs::write(
        dir.path().join("Cargo.toml"),
        "[workspace]\nmembers = [\"app\", \"dep\"]\n",
    )
    .unwrap();

    let metadata = MetadataCommand::new()
        .no_deps()
        .current_dir(dir.path())
        .exec()
        .unwrap();
    let mut crates = Vec::new();
    for pkg in &metadata.packages {
        crates.push((pkg.name.as_str().to_string(), parse_package(pkg).unwrap()));
    }

    let info = analyze_workspace_details(&crates);
    assert_eq!(info["app"].metrics.ce, 1);
    assert_eq!(info["dep"].metrics.ca, 1);
}

#[test]
fn path_root_resolves_special_paths() {
    let defined = std::collections::HashMap::new();
    let mut ws = std::collections::HashSet::new();
    ws.insert("my_crate".to_string());
    let visitor = DetailVisitor {
        current: None,
        defined: &defined,
        crate_name: "my_crate",
        workspace_crates: &ws,
        all_defined: &std::collections::HashMap::new(),
        imports: std::collections::HashMap::new(),
        internal: std::collections::HashMap::new(),
        external: std::collections::HashMap::new(),
        methods: &std::collections::HashMap::new(),
        trait_bounds: &std::collections::HashMap::new(),
    };
    let p: syn::Path = syn::parse_str("self::Foo").unwrap();
    assert_eq!(visitor.path_root(&p), Some("my_crate".to_string()));
    let p: syn::Path = syn::parse_str("super::bar::Baz").unwrap();
    assert_eq!(visitor.path_root(&p), Some("my_crate".to_string()));
    let p: syn::Path = syn::parse_str("crate::Foo").unwrap();
    assert_eq!(visitor.path_root(&p), Some("my_crate".to_string()));
}

#[test]
fn infer_expr_type_special_paths() {
    use std::collections::{HashMap, HashSet};
    let mut defined = HashMap::new();
    defined.insert("Foo".to_string(), ClassKind::Struct);
    let mut ws = HashSet::new();
    ws.insert("my_crate".to_string());
    let visitor = DetailVisitor {
        current: Some("Current".to_string()),
        defined: &defined,
        crate_name: "my_crate",
        workspace_crates: &ws,
        all_defined: &HashMap::new(),
        imports: HashMap::new(),
        internal: HashMap::new(),
        external: HashMap::new(),
        methods: &HashMap::new(),
        trait_bounds: &HashMap::new(),
    };
    let e: syn::Expr = syn::parse_str("self").unwrap();
    assert_eq!(
        visitor.infer_expr_type(&e),
        Some(("Current".to_string(), Some("my_crate".to_string())))
    );
    let e: syn::Expr = syn::parse_str("self::Foo").unwrap();
    assert_eq!(
        visitor.infer_expr_type(&e),
        Some(("Foo".to_string(), Some("my_crate".to_string())))
    );
    let e: syn::Expr = syn::parse_str("super::Foo").unwrap();
    assert_eq!(
        visitor.infer_expr_type(&e),
        Some(("Foo".to_string(), Some("my_crate".to_string())))
    );
    let e: syn::Expr = syn::parse_str("crate::Foo").unwrap();
    assert_eq!(
        visitor.infer_expr_type(&e),
        Some(("Foo".to_string(), Some("my_crate".to_string())))
    );
}

#[test]
fn detects_multiple_cycles() {
    let src_a = "use crate_b::B; pub struct A(B);";
    let src_b = "use crate_a::A; pub struct B(A);";
    let src_c = "use crate_d::D; pub struct C(D);";
    let src_d = "use crate_c::C; pub struct D(C);";

    let file_a: syn::File = syn::parse_str(src_a).unwrap();
    let file_b: syn::File = syn::parse_str(src_b).unwrap();
    let file_c: syn::File = syn::parse_str(src_c).unwrap();
    let file_d: syn::File = syn::parse_str(src_d).unwrap();

    let crates = vec![
        ("crate_a".to_string(), vec![file_a]),
        ("crate_b".to_string(), vec![file_b]),
        ("crate_c".to_string(), vec![file_c]),
        ("crate_d".to_string(), vec![file_d]),
    ];

    let info = analyze_workspace_details(&crates);
    let mut cycles = dependency_cycles(&info);
    cycles.sort_by(|a, b| a[0].cmp(&b[0]));
    assert_eq!(cycles.len(), 2);
    assert!(cycles[0].contains(&"crate_a".to_string()));
    assert!(cycles[0].contains(&"crate_b".to_string()));
    assert!(cycles[1].contains(&"crate_c".to_string()));
    assert!(cycles[1].contains(&"crate_d".to_string()));
}

#[test]
fn detects_no_cycles() {
    let src_a = "use crate_b::B; pub struct A(B);";
    let src_b = "pub struct B;";
    let src_c = "use crate_b::B; pub struct C(B);";

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
    assert!(cycles.is_empty());
}

#[test]
fn where_clause_dependency() {
    let src_a = "pub trait Foo {}";
    let src_b = r#"
        use crate_a::Foo;
        pub fn bar<T>(_: T)
        where
            T: Foo,
        {}
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
}

#[test]
fn macro_method_generic_dependency() {
    let src_a = "pub struct A<T, E>(T, std::marker::PhantomData<E>);";
    let src_b = r#"
        use crate_a::A;

        pub struct B(A<u8, u8>);

        macro_rules! impl_invoke {
            ($that:expr, $req:expr) => {
                $that.invoke($req)
            };
        }

        impl B {
            fn invoke(&self, _req: ()) -> () {
                ()
            }
        }

        fn get_foo(b: &B, req: ()) -> () {
            impl_invoke!(b, req)
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
}
