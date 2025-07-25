FROM ubuntu:24.04

# Insall general dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    python3 \
    python3-pip \
    qemu-system-riscv64 \
    just \
    && rm -rf /var/lib/apt/lists/*

# Copy the content of this repository
WORKDIR /artifact
COPY . .

# Install the tools required by Miralis
WORKDIR /artifact/miralis
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:$PATH"
ENV RUST_MIN_STACK=16777216
RUN just install-toolchain
RUN cargo clean

# Entry point
WORKDIR /artifact
CMD ["/bin/bash"]
