# syntax=docker/dockerfile:1

# Stage 1: build
# Use the slim Debian image. Buildx will pull the correct variant for the
# requested architecture so multi-arch images still work.
FROM rust:1.87-slim AS builder

WORKDIR /app

# Cache dependencies
COPY Cargo.toml Cargo.lock ./
COPY cargo-anatomy/Cargo.toml cargo-anatomy/Cargo.toml
# Prefetch dependencies so later stages can leverage Docker layer caching
RUN cargo fetch --locked --manifest-path cargo-anatomy/Cargo.toml
COPY . .
RUN cargo install --locked --path cargo-anatomy --root /usr/local \
    && strip /usr/local/bin/cargo-anatomy \
    && cp "$(rustup which cargo)" /usr/local/bin/cargo

# Stage 2: package
FROM gcr.io/distroless/cc AS runtime
COPY --from=builder /usr/local/bin/cargo-anatomy /usr/local/bin/
COPY --from=builder /usr/local/bin/cargo /usr/local/bin/
ENV CARGO=/usr/local/bin/cargo
WORKDIR /work
ENTRYPOINT ["/usr/local/bin/cargo-anatomy"]
