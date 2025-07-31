#!/bin/bash
set -e 
set -o pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"

###############
# Idle benchmark
###############

WORKLOAD_NAME="idle-benchmark"

echo "Running idle benchmark"

for i in {0..4} 
do
    # Clear previous file
    clear_stats_entries "${WORKLOAD_NAME}_$1_$i"

    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
    echo "Sleeping 30 seconds"
    sleep 10
    echo "20 left"
    sleep 10
    echo "10 left"
    sleep 10
    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
done


echo "Done with idle benchmark"
