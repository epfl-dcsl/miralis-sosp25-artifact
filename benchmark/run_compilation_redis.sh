#!/bin/bash
set -e 
set -o pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"

WORKLOAD_NAME="redis-compilation"

function install_redis() {
    # First delete the repository
    RemoteExec $ADDRESS "rm -rf redis" > /dev/null
    
    # Clone the Redis repository
    RemoteExec $ADDRESS "git clone https://github.com/redis/redis"

    # Navigate to the Redis directory
    (time (RemoteExec $ADDRESS "cd redis; (make -j$(nproc))")) 2>> "results/${WORKLOAD_NAME}_$1_$2.txt"
}



# Currently we run it a single time
for i in {0..4} 
do
    clear_stats_entries "${WORKLOAD_NAME}_$1_$i"

    add_miralis_stat_entry  "${WORKLOAD_NAME}_$1_$i"

    install_redis $1 $i

    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
done

