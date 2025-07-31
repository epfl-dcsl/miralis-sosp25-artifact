#!/bin/bash

touch overhead.txt
rm overhead.txt
touch overhead.txt

mkdir -p plots/

echo "Plotting CPU Microbenchmark"
python3 coremark.py
 
echo "Plotting Disk Microbenchmark"
python3 iozone.py

echo "Plotting Network Microbenchmark"
python3 cdf.py

echo "Plotting Benchmarks"
python3 throughput.py

echo "Plotting Keystone"
python3 keystone.py

echo "Generating boot"
python3 boot.py
