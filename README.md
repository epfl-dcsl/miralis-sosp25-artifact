# Miralis - SOSP'25 artifact

This repository holds the artifacts for the SOSP'25 paper "The Design and Implementation of a Virtual Firmware Monitor".

The artifacts are split in three parts:
- Miralis — the implementation of the virtual firmware monitor described in the paper.
- Softcore-rs — a compiler and Rust translation of the official RISC-V executable specification
- The benchmarks scripts and results

We provide a docker image with all required software `TODO!!!!`

The rest of this README describes how to evaluate each of the artifacts.

> Hardware requirements:
> - Required: 20 GB available storage and 16GB RAM to test with QEMU
> - Optional: a compatible RISC-V board to test on hardware, 64GB RAM for full formal verification

## Miralis

> Estimated time: XX human time + XX computer time

The Miralis repository is hosted at [https://github.com/CharlyCst/miralis](https://github.com/CharlyCst/miralis).
Additional documentation is available online on the [Miralis website](https://miralis-firmware.github.io/docs/introduction).
For archival purpose, a copy of the repository at the time of the artifact evaluation is provided in `./miralis`.

### Running Miralis

**Installing dependencies**

- **With docker**: all necessary software is already installed in the docker image.

- **Without docker**:
  To run outside of the docker image, you will need to first install the dependencies and configure the project with the following steps:
    
    1. Install Rust (see the [instructions](https://rust-lang.org/tools/install)).
    2. Install [Just](https://github.com/casey/just) (can be installed with `cargo install just`).
    3. Install `qemu-system-riscv64`. On Ubuntu: `sudo apt install qemu-system-riscv64`.
    
    Then in the `miralis` folder:  
    
    4. Run `just install-toolchain` to install the required Rust components.

> [!WARNING] 
> A subtle bug has been introduced in recent QEMU versions (after 9.2) which causes Miralis to mis-behave. We [reported the bug](https://gitlab.com/qemu-project/qemu/-/issues/3020), which was confirmed by the developer. The patch will be back-ported to all maintained QEMU versions, but some package managers such as Homebrew might provide a `qemu-system-riscv64` version with the bug.
> If the Miralis fails with a panic, we recommend using `qemu-system-riscv64` version 9.2.0, which is the latest good version.

**Running Miralis**

Let's first check that everything is in place by running `just run` in the `miralis` folder.
The output should be similar to:

```
[Info  | miralis] Hello, world!
[Info  | miralis] Platform name: QEMU virt
[Info  | miralis] Hart 0 is up
[Info  | miralis::modules] Installed 1 modules:
[Info  | miralis::modules]   - Offload Policy
[Info  | miralis::virt::emulator] > Hello from default firmware!
[Info  | miralis::virt::emulator] Success!
[Info  | miralis::virt::emulator] Number of exits: 5
[Info  | miralis::debug] Maximal stack usage: 4232 bytes (12.29%)
```

Under the hood, `just run` compiles a small test firmware (the sources are in `miralis/firmware/default`), starts QEMU with Miralis as the boot firmware which will then virtualize the test firmware.
This is how the deployment looks like:

```
        ┌──────────────┐ ┌────────────┐
U-mode  │   User App   │ │  Firmware  │ vM-mode
        ├──────────────┤ └────────────┘
S-mode  │    Kernel    │
        ├──────────────┴──────────────┐
M-mode  │           Miralis           │
        └─────────────────────────────┘
```

To make testing easier, we provide firmware artifacts that are downloaded on demand. For instance, to run Linux with a virtualized OpenSBI firmware, run :

```sh
just run linux
```

The list of artifacts is available in the [documentation](https://miralis-firmware.github.io/docs/artifacts).
Most of them are exercised by the test suite, thus an easy way to test multiple configurations is simply to run the test suite:

```sh
just test
```

It can take up to a couple of minutes to run the full test suite.

> [!NOTE]
> `just test` will skip some tests if [`spike`](https://github.com/riscv-software-src/riscv-isa-sim) is not installed.
> We use Spike in addition to QEMU to test Miralis with different sets of hardware features.
> If `just test` exists with:
>
> ```
> Test done: 32/37
> spike is not available, skipped 5 tests
> error: Recipe `test` failed on line 33 with exit code 1
> ```
>
> It means that all tests succeeded on QEMU.
> Testing with Spike is not necessary for the artifact evaluation, but if desired it can be [installed from source](https://github.com/riscv-software-src/riscv-isa-sim) or by downloading the [x86 binary we use in the CI](https://github.com/epfl-dcsl/spike-ci-artifact/releases/tag/v0.1.3).

At this point we know that Miralis is working as intended.

### Understanding the codebase

The purpose of this section is to give a feel for the Miralis code base.

The source code of Miralis is in the `src` folder at the root of the `miralis` repository, and is organized as follow:
- The `virt` folder contains the virtualization logic, including CSR and instruction emulation, as well as world switches.
- The architecture-specific code is under the `arch` module, which abstracts the hardware interface.
  `arch/metal.rs` contains the RISC-V assembly code, while `arch/userspace` is pure Rust code and emulates hardware interaction using a software RISC-V core that we compiled from the RISC-V specification. This allows running Miralis either on hardware, or in a user-space process for unit-testing and verification purposes.
  We discuss the details in the `softcore-rs` artifact section.
- `module.rs` defines the module abstraction.
  The implementation of the policy modules described in the paper are in the `policy` folder.
  Our benchmarks tools are also implemented as module under `benchmark`.
  The choice of modules is done at compile time, using the [configuration](https://miralis-firmware.github.io/docs/configuration).
- The `platform` folder hosts the SoC-specific code.
  Miralis supports the VisionFive2 and Premier P550 at present, as well as RISC-V boards using similar SoC (jh7110 and EIC7700X).
  Instructions to instalm Miralis supported platforms are available on the [website](https://miralis-firmware.github.io/docs/platforms).

The rest of the codebase, including the tools and tests, are described in the [online documentation](https://miralis-firmware.github.io/docs/overview).

### Testing the isolation policies

To enforce isolation between the firmware and other parts of the system, we can compile Miralis with policy modules.
The paper describes three isolation policies:
- The sandbox policy, which protects the OS from the firmware
- The keystone policy, which can create enclaves protected from both the OS and the firmware.
  Our implementation is a re-write of the [original Keystone security monitor](https://github.com/keystone-enclave/keystone) as a Miralis policy module.
- The ACE policy.
  Our implementation if a port of the [ACE security monitor](https://github.com/IBM/ACE-RISCV), and lives in a [separate repository](https://github.com/epfl-dcsl/miralis-ace) to reduce the maintenance burden.

For the artifact evaluation we propose to test the sandbox and keystone policies.
The ACE policy requires a different setup and more computer time and storage, although we can provide instructions if desired.

**Sandbox policy**

We propose to test the sandbox policy by running an off-the-shelf Ubuntu distribution.
In this setup, the whole OS (kernel and all processes) are isolated from the firmware.
Running the following command will automatically download an Ubuntu image, and run it with Miralis (it might take a few minutes do townload):

```sh
just run opensbi-jump config/ubuntu.toml
```

> [!NOTE]
> The download might fail if the Ubuntu image gets deleted, which can happen when vulnerabilities are fixed in point releases.
> The download URL can be found in `misc/artifacts.toml` and points to the official Ubuntu servers.
> We will provide an update if the link breaks during the artifact evaluation.

The last part of the command (`config/ubuntu.toml`) points to a configuration file.
The interesting part of that file is the following snippet:

```toml
[modules]
modules = ["protect_payload"]
```

It defines the modules to be compiled into Miralis.
Here we see that the `protect_payload` module is enabled, which corresponds to the sandbox policy described in the paper.

By looking at the Miralis logs right after starting QEMU we can confirm the module has been installed:

```
[Info  | miralis::modules] Installed 1 modules:
[Info  | miralis::modules]   - Protect Payload Policy
```

> [!NOTE]
> You might find error logs from the sandbox policy:
> ```
> [Error | miralis::policy::protect_payload] Loaded payload is suspicious
> [Error | miralis::policy::protect_payload] Hashed value: [34, 106, 26, 97, 59, 202, 118, 187, 74, 68, 40, 96, 169, 103, 39, 230, 41, 123, 251, 197, 189, 59, 76, 237, 40, 161, 195, 213, 117, 148, 118, 113]
> [Error | miralis::policy::protect_payload] Expected value: [241, 90, 158, 184, 200, 210, 145, 178, 30, 80, 200, 161, 56, 120, 75, 241, 68, 38, 21, 2, 248, 112, 128, 155, 31, 240, 37, 94, 203, 66, 243, 167]
> [Error | miralis::policy::protect_payload] Protect Payload policy: Invalid hash
> ```
> This is because we do not keep the hashes up-to date, and the policy detects that an unknown payload (i.e. OS) has been loaded.
> This can be safely ignored (or fixed by updating the policy with the latest hash value).

Eventually you will get a login prompt and then a shell.
Everything should feel normal, except the firmware can not access the OS memory.

**Keystone policy**

To evaluate the keystone policy we propose to run the tests from the upstream [Keystone project](https://github.com/keystone-enclave/keystone).
To ease testing, we provide a pre-built disk image containing a Keystone-capable Linux kernel as well as test binaries.
The build scripts and resulting artifacts can be inspected [in this repository](https://github.com/epfl-dcsl/miralis-artifact-keystone).
The disk image is automatically downloaded and launched by running the following command:

```sh
just run keystone config/qemu-keystone.toml
```

Like for the previous command, the last part (`config/qemu-keystone.toml`) points to a configuration file.
By looking at the file, we can see that the Keystone policy is enabled:

```toml
[modules]
modules = ["keystone", "exit_counter"]
```

Which is confirmed by the Miralis logs:

```
[Info  | miralis::modules] Installed 2 modules:
[Info  | miralis::modules]   - Keystone Policy
[Info  | miralis::modules]   - Counter Benchmark
```

Once the Linux shell open, first install the Keystone kernel module:

```sh
modprobe keystone-driver
```

You should see similar logs which confirm the module has been installed:

```
bash-5.2# modprobe keystone-driver
[  132.877929] keystone_driver: loading out-of-tree module taints kernel.
[  132.883585] keystone_enclave: keystone enclave v1.0.0
```

Then go to the `/usr/share/keystone/examples/` directory to find the enclave examples:

```sh
cd /usr/share/keystone/examples/
```

For instance, executing `hello.ke`:

```
bash-5.2# ./hello.ke
Verifying archive integrity... MD5 checksums are OK. All good.
Uncompressing Keystone Enclave Packagedf: /tmp/selfgz62519370: can't find mount point
./hello.ke: line 651: test: Available: integer expression expected

hello, world!
```

A `.ke` executable is an archive containing a user-space and an enclave bundled together, in the above example the enclave prints "hello, world!".
Note that the de-compression and "line 651" error exist with upstream Keystone to in our tests, they are not introduced by Miralis.
Overall, we just verified that Miralis can run Keystone enclaves, while additionally removing the firmware from the trusted computing base.

> [!NOTE]
> The execution time reported by some benchmarks such as dhrystone might not make sense, this is because the performance counters emulated by QEMU do not necessarily match the elapsed time or cycles on the host.
> The performance reported in the paper were measured on real hardware.

## Softcore-rs

> Estimated time: 15m human time + 2h computer time for full verification (optional)

Section 6 of the paper describes the methodology we used to verify the main components of the virtualization sub-system of Miralis.
In a nutshell, we translated the official RISC-V executable specification (written in Sail) to Rust, and used the [Kani model-checker](https://model-checking.github.io/kani/) to ensure emulation is compliant with the specification and hardware configuration produces the desired effects.

Since the original paper submission, we split this part of Miralis into an independent project called `softcore-rs` in a [separate repository](https://github.com/CharlyCst/softcore-rs).
For archival purpose, we include a copy of the project in the `./softcore-rs/` folder.
`softcore-rs` is available on the [Rust package registry](https://crates.io/crates/softcore-rv64) and can now be used by any other Rust project.

### A quick tour of `softcore-rs`

The Sail-to-Rust translation is not the main focus of the paper, therefore we only provide a quick overview of `softcore-rs` for the purpose of the artifact evaluation.
We plan to present `softcore-rs` and its application more in depth in a future work.

At its core, `softcore-rs` is a Sail-to-Rust compiler.
It takes a Sail ISA implementation, and produces equivalent Rust code.
The code of the compiler lives in `softcore-rs/sail_to_rust`.

For the purpose of verifying Miralis, we focus on translating the [official RISC-V model](https://github.com/riscv/sail-riscv).
The Rust translation of the RISC-V (64 bits variant) can be found in `./softcore-rs/rv64/src/raw.rs`, and is available as a [Rust library](https://crates.io/crates/softcore-rv64).
We provide a thin wrapper around the raw translation in `./softcore-rs/rv64/src/lib.rs`, to make the Sail model more ergonomic to use from Rust code.

### Verifying Miralis with `softcore-rs` ([recording](https://asciinema.org/a/UQARO1QlIKXPXTepgyEgV0IZb))

Miralis leverages the RISC-V model to verify some of the core virtualization components.
The verification tasks can be found in `./miralis/model_checking/src/lib.rs` and are the functions marked with `#[cfg_attr(kani, kani::proof)]`.

Verification is handled by the `cargo kani` command, which is already available in the docker and installed as part of the Miralis installation when running outside of docker.
To verify the emulation of the `mret` instruction, it suffices to run the following command in the `miralis` folder:

```sh
cargo kani -p model_checking --output-format terse --harness mret
```

This is expected to succeed in about 2 to 3 minutes.
The last part of the command (`--harness mret`) selects which verification task to run, while running `cargo kani -p model_checking --output-format terse` will run all verification tasks.

As full verification is time and RAM intensive, we provide a [recording](https://asciinema.org/a/UQARO1QlIKXPXTepgyEgV0IZb) showing the successful verification of Miralis at the time of the artifact review.
The recording was speed-up to remove waiting time during compilation and verification, but the verification time are displayed for each verification task on lines starting with `Verification Time:`.

[![asciicast](https://asciinema.org/a/UQARO1QlIKXPXTepgyEgV0IZb.svg)](https://asciinema.org/a/UQARO1QlIKXPXTepgyEgV0IZb)

It is not necessary to re-run all verification tasks for the artifact evaluation, verifying a couple is sufficient.
We provide the full list of verification tasks below, with the expected completion time.
Tasks needed more than 16GB of ram are marked as "high RAM usage".
Execution time might vary from simple to double on different machines.

To run one of the task, execute `cargo kani -p model_checking --output-format terse --harness <task_id>` with the corresponding task ID:

**Instructions decoding:**
- `verify_stores`: ~2m30s
- `verify_compressed_stores`: ~2m30s
- `verify_load`: ~2m30s
- `verify_compressed_loads`: ~2m30s
- `verify_decoder`: ~3m

**Hardware configuration:**
- `pmp_virtualization`: ~20m (high RAM usage)

**Emulation:**
- `exception_virtualization`: ~3m
- `interrupt_virtualization`: ~3m
- `write_csr`: ~30m (high RAM usage)
- `read_csr`: ~15m (high RAM usage)
- `fences`: ~2m
- `wfi`: ~4m
- `sret`: ~2m
- `mret`: ~2m

## Benchmarks

> Estimated time: XX human time + XX computer time


