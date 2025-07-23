## Code Style
- Format Rust code with `cargo fmt --all`
- Enforce linting with `cargo clippy -- -D warnings`
- Disallow glob imports (e.g. `use crate::*;`)

## Testing
- Run `cargo test` on every commit
- Minimum coverage: 90% (e.g. via `cargo tarpaulin`)

## Commit Message
- Follow Conventional Commits
- Prefix hotfixes with `fix:` and feature work with `feat:`

## Build
- Run `cargo build` on every commit

