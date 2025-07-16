# syntax=docker/dockerfile:1

# Stage 1: build
# Build natively for the requested architecture so no cross toolchain is needed
FROM --platform=$TARGETPLATFORM rust:1.87-alpine AS builder

# Install build tools
RUN apk add --no-cache build-base

# Set up musl target based on architecture
ARG TARGETARCH
RUN TARGET="${TARGETARCH}-unknown-linux-musl" \
    && rustup target add "$TARGET"

WORKDIR /app

# Cache dependencies
COPY Cargo.toml Cargo.lock ./
COPY cargo-anatomy/Cargo.toml cargo-anatomy/Cargo.toml
RUN mkdir -p cargo-anatomy/src && echo 'fn main() {}' > cargo-anatomy/src/main.rs
RUN TARGET="${TARGETARCH}-unknown-linux-musl" \
    && cargo build --release --target "$TARGET" -p cargo-anatomy

RUN rm -rf cargo-anatomy/src
COPY . .
RUN TARGET="${TARGETARCH}-unknown-linux-musl" \
    && cargo install --locked --path cargo-anatomy --target "$TARGET" --root /usr/local \
    && strip /usr/local/bin/cargo-anatomy

# Stage 2: package
FROM scratch AS runtime
COPY --from=builder /usr/local/bin/cargo-anatomy /usr/local/bin/
ENTRYPOINT ["cargo-anatomy"]
CMD ["-h"]
