# Changelog

All notable changes to this project will be documented in this file.

## [0.6.7] - 2025-11-17
### Maintenance
- Routine maintenance release.

## [0.6.6] - 2025-10-29
### Maintenance
- Routine maintenance release.

## [0.6.5] - 2025-10-18
### Fixed
- Corrected external dependency counting when metrics are accessed via re-exports.

### Maintenance
- Routine maintenance release.

## [0.6.4] - 2025-10-09
### Maintenance
- Routine maintenance release.

## [0.6.3] - 2025-09-10
### Maintenance
- Routine maintenance release.

## [0.6.2] - 2025-08-14
### Maintenance
- Routine maintenance release.

## [0.6.1] - 2025-08-13
### Maintenance
- Routine maintenance release.

## [0.7.0-alpha.0] - 2025-08-05
### Changed
- Pre-release version bump.

## [0.6.0] - 2025-07-31
### Added
- Type-level mermaid and Graphviz DOT output with crate filters.
- Function nodes included in Mermaid graphs.
- CLI thresholds configurable via `.anatomy.toml`.
### Changed
- Argument parsing switched to clap.
- Removed crate root nodes from graphs.
- Default configuration uses workspace `.anatomy.toml`.
### Fixed
- Correct handling when invoked as a cargo subcommand.

## [0.5.2] - 2025-07-23
### Fixed
- Improved workspace dependency detection for glob and macro imports.
- Resolved issues with type alias dependencies.

## [0.5.1] - 2025-07-22
### Fixed
- Correctly detect dependencies imported via module paths and aliases.

## [0.5.0] - 2025-07-21
### Added
- Configurable evaluation thresholds.
- `init` subcommand to generate a configuration template.
- Meta information in the JSON output, including the target triple.
### Changed
- Reorganized configuration documentation and clarified usage.
### Fixed
- Mermaid format now correctly inserts newlines.

## [0.4.0] - 2025-07-20
### Added
- Mermaid graph output format.
- Graphviz graph output format.
- CLI categories in the manifest for crates.io.
### Changed
- Graph edges are hidden unless `-a` is specified.
- Improved DOT output and couple counting.
### Fixed
- Documentation updates and internal refactoring.

## [0.3.0] - 2025-07-19
### Added
- Qualitative evaluation labels for metrics.
- Output schema now uses objects with `crate_name`, `metrics` and `evaluation` fields.
- File and line number information on errors.
- Evaluation criteria in help output.
- Expanded Docker usage documentation.

## [0.2.0] - 2025-07-18
### Added
- External dependency metrics.
- Output schema documentation with README link.
- Clarification of `-x` option impact.
- Project logo.

## [0.1.0] - 2025-07-16
### Added
- Initial release with workspace analysis metrics.
