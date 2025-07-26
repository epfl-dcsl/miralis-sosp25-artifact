# Build container, used to create dependencies
FROM ubuntu:24.04 AS build

RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    device-tree-compiler \
    libboost-regex-dev \
    libboost-system-dev \
    gcc-riscv64-unknown-elf

# Build spike
WORKDIR /artifact
RUN git clone --depth 1 https://github.com/riscv-software-src/riscv-isa-sim.git
RUN mkdir build
RUN cd build && ../riscv-isa-sim/configure CFLAGS=-static CXXFLAGS="-static -static-libgcc -static-libstdc++" LDFLAGS=-static && make -j `nproc` spike && cp spike ../

FROM ubuntu:24.04

# Insall general dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    python3 \
    python3-pip \
    qemu-system-riscv64 \
    device-tree-compiler \
    just \
    && rm -rf /var/lib/apt/lists/*

# Copy binaries from build container
COPY --from=build /artifact/spike /usr/local/bin/spike

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
