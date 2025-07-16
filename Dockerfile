# syntax=docker/dockerfile:1

# Stage 1: build
# Build natively for the requested architecture so no cross toolchain is needed
FROM rust:1.87-alpine AS builder

# Install build tools
RUN apk add --no-cache build-base

# Set up musl target based on architecture
ARG TARGETARCH
RUN case "$TARGETARCH" in \
        amd64) TARGET=x86_64-unknown-linux-musl ;; \
        arm64) TARGET=aarch64-unknown-linux-musl ;; \
        *) echo "Unsupported arch $TARGETARCH" && exit 1 ;; \
    esac && \
    rustup target add "$TARGET"

WORKDIR /app

# Cache dependencies
COPY Cargo.toml Cargo.lock ./
COPY cargo-anatomy/Cargo.toml cargo-anatomy/Cargo.toml
# Create a dummy main to build dependencies only
RUN mkdir -p cargo-anatomy/src \
    && echo 'fn main() {}' > cargo-anatomy/src/main.rs
RUN case "$TARGETARCH" in \
        amd64) TARGET=x86_64-unknown-linux-musl ;; \
        arm64) TARGET=aarch64-unknown-linux-musl ;; \
    esac && \
    cargo build --release --target "$TARGET" -p cargo-anatomy

RUN rm -rf cargo-anatomy/src
COPY . .
RUN case "$TARGETARCH" in \
        amd64) TARGET=x86_64-unknown-linux-musl ;; \
        arm64) TARGET=aarch64-unknown-linux-musl ;; \
    esac && \
    cargo install --locked --path cargo-anatomy --target "$TARGET" --root /usr/local \
    && strip /usr/local/bin/cargo-anatomy

# Stage 2: package
FROM scratch AS runtime
COPY --from=builder /usr/local/bin/cargo-anatomy /usr/local/bin/
# Run with help by default
ENTRYPOINT ["cargo-anatomy", "-h"]
