# Output Format Schema

This document describes the JSON/YAML output produced by `cargo anatomy`.
When using `-o dot`, the tool outputs a Graphviz DOT graph instead which is not detailed here. The `-o mermaid` option similarly emits a Mermaid diagram.
The command prints a single object with three sections:

```json
{
  "meta": { ... },
  "crates": [ { "crate_name": "pkg", ... } ],
  "warnings": { "dependency_cycles": [] }
}
```

The `crates` array contains an entry for each analyzed crate. Each entry always
includes the crate name, its computed metrics and the evaluation labels. When
the `-a` flag is used a `details` object is also present containing class and
dependency information. The `warnings` object aggregates workspace wide
notices such as detected dependency cycles.

The `meta` object records the version of `cargo-anatomy`, the build target and
the configuration values used for the run.

## Metrics Object

| Field   | Description |
|---------|-------------|
| `n`     | Number of classes (`struct`, `enum`, `trait` and `type` definitions). |
| `r`     | Number of internal class relationships. Each unique reference from one class to another within the same crate counts once. |
| `h`     | Relational cohesion, calculated as `(R + 1) / N`. |
| `ca`    | Afferent coupling: external classes that depend on this crate. |
| `ce`    | Efferent coupling: classes in this crate depending on other workspace crates. |
| `a`     | Abstraction ratio: `abstract_classes / N`. |
| `i`     | Instability: `Ce / (Ce + Ca)`. |
| `d`     | Distance from the main sequence: `|A + I - 1| / sqrt(2)`. |
| `d_prime` | Normalized distance from the main sequence: `|A + I - 1|`. |

## Evaluation Object

The results also include qualitative labels derived from the metrics:

| Field | Description |
|-------|-------------|
| `a` | "abstract", "mixed" or "concrete" depending on the abstraction ratio. |
| `h` | "high" or "low" relational cohesion. |
| `i` | "stable", "moderate" or "unstable" based on instability. |
| `d_prime` | "good", "balanced", "painful" or "useless" depending on the normalized distance. |

## Details Object

When running with `-a` a `details` object is emitted containing:

- `kind` – either `Workspace` or `External`.
- `classes` – list of class information objects.
- `internal_depends_on` – mapping of type name to the list of types it depends on within the same crate.
- `internal_depended_by` – mapping of type name to the list of types that depend on it within the same crate.
- `external_depends_on` – mapping of type name to other crates and the types from those crates it depends on.
- `external_depended_by` – mapping of type name to crates and the types that depend on it from those crates.

Each class object contains:

- `name` – the identifier of the class.
- `kind` – one of `Struct`, `Enum`, `Trait`, `TypeAlias` or `Macro`.

## Example

The following is a shortened example after running `cargo anatomy -a | jq`:

```json
{
  "meta": {
  "cargo-anatomy": { "version": "0.5.2", "target": "linux/x86_64" },
    "config": {
      "evaluation": {
        "abstraction": { "abstract_min": 0.7, "concrete_max": 0.3 },
        "cohesion": { "high_gt": 1.0 },
        "instability": { "unstable_min": 0.7, "stable_max": 0.3 },
        "distance": { "good_max": 0.4, "bad_min": 0.6 }
      }
    }
  },
  "crates": [
    {
      "crate_name": "my_crate",
      "metrics": {
        "n": 3,
        "r": 1,
        "h": 0.67,
        "ca": 0,
        "ce": 1,
        "a": 0.33,
        "i": 1.0,
        "d": 0.24,
        "d_prime": 0.34
      },
      "evaluation": {
        "a": "mixed",
        "h": "low",
        "i": "unstable",
        "d_prime": "good"
      },
      "details": {
        "kind": "Workspace",
        "classes": [
          { "name": "Foo", "kind": "Struct" },
          { "name": "Bar", "kind": "Struct" },
          { "name": "MyTrait", "kind": "Trait" }
        ]
      }
    }
  ],
  "warnings": {
    "dependency_cycles": []
  }
}
```
