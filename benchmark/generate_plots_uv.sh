#!/bin/bash

touch overhead.txt
rm overhead.txt
touch overhead.txt

mkdir -p plots/

echo "Plotting CPU Microbenchmark"
uv run coremark.py
 
echo "Plotting Disk Microbenchmark"
uv run iozone.py

echo "Plotting Network Microbenchmark"
uv run cdf.py

echo "Plotting Benchmarks"
uv run throughput.py

echo "Plotting Keystone"
uv run keystone.py

echo "Generating boot"
uv run boot.py
