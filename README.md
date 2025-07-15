# cargo-anatomy

[![Rust CI](https://github.com/cutsea110/cargo-anatomy/actions/workflows/ci.yml/badge.svg)](https://github.com/cutsea110/cargo-anatomy/actions/workflows/ci.yml)

`cargo-anatomy` analyzes Rust workspaces and calculates metrics inspired by Robert C. Martin's package metrics. Each crate inside the workspace is treated as a package.

## Metrics

- **N** — number of classes in the crate (`struct`, `enum`, `trait` and `type` definitions).
- **R** — number of internal class relationships. Each unique reference from one class to another within the same crate counts once.
- **H** — relational cohesion: `H = (R + 1) / N`.
- **Ca** — afferent coupling: the number of external classes that depend on types from this crate.
- **Ce** — efferent coupling: the number of classes in this crate that depend on types from other crates in the workspace.
- **A** — abstraction: ratio of abstract classes (traits) to total classes, `A = abstract_classes / N`.
- **I** — instability: `I = Ce / (Ce + Ca)`.
- **D** — distance from the main sequence: `D = |A + I - 1| / sqrt(2)`.
- **D'** — normalized distance from the main sequence: `D' = |A + I - 1|`.

`cargo-anatomy` also reports the list of classes discovered in each crate and detailed dependency graphs when invoked with the `-a` flag.

## Usage

```bash
# Run on the current workspace
cargo anatomy

# Show detailed class and dependency information
cargo anatomy -a

# Display help with metric descriptions
cargo anatomy -?

# Show version
cargo anatomy -V

# Output in YAML format
cargo anatomy -o yaml
```

The command outputs metrics for every member crate in JSON format by default. Use `-o yaml` for YAML output. Example output:

```json
{
  "my_crate": {
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
    "classes": [
      { "name": "Foo", "kind": "Struct" },
      { "name": "Bar", "kind": "Struct" },
      { "name": "MyTrait", "kind": "Trait" }
    ]
  }
}
```

Enable `RUST_LOG=info` to see progress logs during analysis.

