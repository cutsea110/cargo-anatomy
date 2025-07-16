# syntax=docker/dockerfile:1

# Stage 1: build
# Use a recent Rust release to ensure Cargo understands our lock file
FROM --platform=$BUILDPLATFORM rust:1.87-alpine AS builder

# Install build tools
RUN apk add --no-cache build-base

# Set up target triple based on requested architecture
ARG TARGETARCH
RUN case "$TARGETARCH" in \
    amd64) TARGET=x86_64-unknown-linux-musl ;; \
    arm64) TARGET=aarch64-unknown-linux-musl ;; \
    *) echo "Unsupported arch $TARGETARCH" && exit 1 ;; \
    esac && \
    rustup target add $TARGET

WORKDIR /app

# Cache dependencies
COPY Cargo.toml Cargo.lock ./
COPY cargo-anatomy/Cargo.toml cargo-anatomy/Cargo.toml
RUN mkdir -p cargo-anatomy/src && echo 'fn main() {}' > cargo-anatomy/src/main.rs
RUN case "$TARGETARCH" in \
    amd64) TARGET=x86_64-unknown-linux-musl ;; \
    arm64) TARGET=aarch64-unknown-linux-musl ;; \
    esac && \
    cargo build --release --target $TARGET -p cargo-anatomy

RUN rm -rf cargo-anatomy/src
COPY . .
RUN case "$TARGETARCH" in \
    amd64) TARGET=x86_64-unknown-linux-musl ;; \
    arm64) TARGET=aarch64-unknown-linux-musl ;; \
    esac && \
    cargo install --locked --path cargo-anatomy --target $TARGET --root /usr/local && \
    strip /usr/local/bin/cargo-anatomy

# Stage 2: package
FROM scratch AS runtime
COPY --from=builder /usr/local/bin/cargo-anatomy /usr/local/bin/
ENTRYPOINT ["cargo-anatomy"]
CMD ["-h"]
