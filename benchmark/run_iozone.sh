#!/bin/bash
set -e 
set -o pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"

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

