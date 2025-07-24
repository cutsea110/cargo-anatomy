# Changelog

All notable changes to this project will be documented in this file.
## [0.5.3] - 2025-07-24
### Changed
- Removed alpha designation from version.


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
