#!/bin/bash
set -e 
set -o pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"

###############
# Coremark pro
###############

WORKLOAD_NAME="coremarkpro"

echo "Running CPU Microbenchmark [Coremarkpro]"

executables=(
    "./coremark-pro/builds/linux64/gcc64/bin/core.exe -i60  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/cjpeg-rose7-preset.exe -i10000  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/linear_alg-mid-100x100-sp.exe -i3000  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/loops-all-mid-10k-sp.exe -i30  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/nnet_test.exe -i50  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/parser-125k.exe -i2000  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/radix2-big-64k.exe -i3000  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/sha-test.exe -i1450  -c4 -v0"
    "./coremark-pro/builds/linux64/gcc64/bin/zip-test.exe -i4200  -c40 -v0"
)

names=(
    "core"
    "jpeg"
    "gaussian"
    "livermore"
    "neuralnetwork"
    "xml"
    "fft"
    "sha"
    "zip"
)

length=${#names[@]} 


for i in {0..4} 
do
    for ((idx=0; idx<length; idx++)); do
        # Clear previous file
        clear_stats_entries "${WORKLOAD_NAME}-${names[idx]}_$1_$i"

        add_miralis_stat_entry "${WORKLOAD_NAME}-${names[idx]}_$1_$i"
        RemoteExec $ADDRESS "${executables[idx]}" > "results/${WORKLOAD_NAME}-${names[idx]}_$1_$i.txt"
        add_miralis_stat_entry "${WORKLOAD_NAME}-${names[idx]}_$1_$i"
    done
done


echo "Done with CPU microbenchmark"

###############
# Iozone
###############

WORKLOAD_NAME="iozone"

echo "Running filesystem microbenchmark [Filesystem]"

for i in {0..4} 
do
    # Clear previous file
    clear_stats_entries "${WORKLOAD_NAME}_$1_$i"

    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
    RemoteExec $ADDRESS "./microbenchmark_fs.sh" > "results/${WORKLOAD_NAME}_$1_$i.txt"
    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
done

echo "Done with disk microbenchmark"

###############
# Netperf
###############

WORKLOAD_NAME="netperf"

echo "Running network microbenchmark [netperf]"

# Start network server
RemoteExec $ADDRESS "./microbenchmark_network.sh $1"

for i in {0..4} 
do
    # Clear previous file
    clear_stats_entries "${WORKLOAD_NAME}_$1_$i"

    # Launch remote benchmarks
    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
    netperf -H $(echo "$ADDRESS" | cut -d'@' -f2-) -t TCP_STREAM -l 30  > "results/${WORKLOAD_NAME}-tcp_$1_$i.txt"
    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
    netperf -H $(echo "$ADDRESS" | cut -d'@' -f2-) -t UDP_STREAM -l 30 > "results/${WORKLOAD_NAME}-udp_$1_$i.txt"
    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
done

echo "Done with network microbenchmark"

