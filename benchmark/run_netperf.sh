#!/bin/bash
set -e 
set -o pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"

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

