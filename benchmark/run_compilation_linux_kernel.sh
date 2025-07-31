#!/bin/bash
set -e 
set -o pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null 2>&1 && pwd)"
source $DIR/common.sh

setup "$1"

WORKLOAD_NAME="linux-compilation"

function install_linux() {
    # First delete the repository
    RemoteExec $ADDRESS "rm -rf linux"
    
    # Clone the LInux repository
    RemoteExec $ADDRESS "git clone --depth 1 https://github.com/starfive-tech/linux"
    
    # Navigate to the Linux directory
    (time (RemoteExec $ADDRESS "cd linux && make defconfig && make -j4")) 2>> "results/${WORKLOAD_NAME}_$1_$2.txt"
}



# Currently we run it a single time
for i in {0..4} 
do
    clear_stats_entries "${WORKLOAD_NAME}_$1_$i"

    add_miralis_stat_entry  "${WORKLOAD_NAME}_$1_$i"

    install_linux $1 $i

    add_miralis_stat_entry "${WORKLOAD_NAME}_$1_$i"
done


