# Output Format Schema

This document describes the JSON/YAML output produced by `cargo anatomy`.
The tool emits a list where each element is a two item array:

```
[package_name, result]
```

The first element is the package name as a string.  The second element is
either a **Metrics** object or a **CrateDetail** object when the `-a` flag is
used.

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

## CrateDetail Object

The detailed result includes additional fields:

- `kind` – either `Workspace` or `External`.
- `metrics` – the Metrics object described above.
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
    "evaluation": {
      "a": "mixed",
      "h": "low",
      "i": "unstable",
      "d_prime": "good"
    },
    "classes": [
      { "name": "Foo", "kind": "Struct" },
      { "name": "Bar", "kind": "Struct" },
      { "name": "MyTrait", "kind": "Trait" }
    ]
  }
}
```
