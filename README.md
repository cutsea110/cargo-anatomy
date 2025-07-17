# cargo-anatomy
<p align="center">
  <img src="logo.svg" alt="Cargo Anatomy Logo" width="200" />
</p>

[![Rust CI](https://github.com/cutsea110/cargo-anatomy/actions/workflows/ci.yml/badge.svg)](https://github.com/cutsea110/cargo-anatomy/actions/workflows/ci.yml)
[![Crates.io](https://img.shields.io/crates/v/cargo-anatomy.svg)](https://crates.io/crates/cargo-anatomy)
[![Crates.io Downloads](https://img.shields.io/crates/d/cargo-anatomy.svg?label=Crates.io&logo=rust)](https://crates.io/crates/cargo-anatomy)
[![Docker Cloud Build Status](https://img.shields.io/docker/pulls/cutsea110/cargo-anatomy?label=cargo-anatomy&logo=docker)](https://hub.docker.com/repository/docker/cutsea110/cargo-anatomy/general)

`cargo-anatomy` analyzes Rust workspaces and calculates metrics inspired by Robert C. Martin's package metrics. Each crate inside the workspace is treated as a package.

## Installation

```
cargo install cargo-anatomy
```

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

The command outputs metrics for every member crate in compact JSON format by default. Pipe to `jq` if you want it pretty printed. Use `-o yaml` for YAML output. Example output (`| jq`):

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

## Docker image

Build an image for the current architecture and load it into Docker with

```bash
docker buildx build --platform <arch> -t cargo-anatomy --load .
```

Replace `<arch>` with `linux/amd64` on x86_64 machines or `linux/arm64` on
Arm-based hosts. To publish a multi-platform image, use `--push` instead of
`--load`:

```bash
docker buildx build --platform linux/amd64,linux/arm64 \
    -t <your-registry>/cargo-anatomy:<version> --push .
```
Set `<version>` to the tag for the published image.

Run the container on your project by mounting the workspace into `/work`:

```bash
docker run --rm -v $(pwd):/work <your-registry>/cargo-anatomy:<version> [ARGS...]
```

Any arguments after the image name are forwarded to `cargo-anatomy`. The image
includes the toolchain `cargo` binary and sets the `CARGO` environment variable
to that path, so `cargo metadata` works without `rustup`. The runtime uses a
distroless base for a smaller footprint.

