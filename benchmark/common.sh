#!/bin/bash

# Todo: Replace the ips here
COMMON_IP="sosp@192.168.1.22"
BOARD_IP=$COMMON_IP
MIRALIS_IP=$COMMON_IP
PROTECT_PAYLOAD_IP=$COMMON_IP
OFFLOAD_IP=$COMMON_IP

function create_folder_if_not_exists() {
    local folder="$1" 
    if [ ! -d "$folder" ]; then
        echo "Folder '$folder' does not exist. Creating..."
        mkdir "$folder"
    fi
}

function RemoteExec() {
    ssh -oStrictHostKeyChecking=no -p 22 "$1" "cd miralis-benchmark;$2";
}

function setup() {
    if [ "$#" -ne 1 ]; then
        echo "Usage: $0 <arg1>"
        echo "Error: Please provide the benchmark type as argument, options are [board|miralis|protect]"
        exit 1
    fi

    # We only allow three kind of benchmark types
    if ! [[ "$1" == "board" || "$1" == "miralis" || "$1" == "protect" || "$1" == "offload" ]]; then
        echo "Error: Invalid argument. Allowed values are 'board', 'miralis', 'offload' or 'protect'."
        exit 1
    fi

    echo "Benchmark type: $1"

    # Determine ADDRESS based on VALUE
    if [[ "$1" == "board" ]]; then
        ADDRESS=$BOARD_IP
    elif [[ "$1" == "miralis" ]]; then
        ADDRESS=$MIRALIS_IP
    elif [[ "$1" == "protect" ]]; then
        ADDRESS=$PROTECT_PAYLOAD_IP
    elif [[ "$1" == "offload" ]]; then
        ADDRESS=$OFFLOAD_IP
    else
        echo "Unknown value: $VALUE"
        exit 1
    fi

    create_folder_if_not_exists "results"
    create_folder_if_not_exists "results/stats"
}

function clear_stats_entries() {
    echo "" > "results/stats/$1_core_1.txt"
    echo "" > "results/stats/$1_core_2.txt"
    echo "" > "results/stats/$1_core_3.txt"
    echo "" > "results/stats/$1_core_4.txt"
}

function add_miralis_stat_entry() {
    RemoteExec $ADDRESS "taskset 1 cat /proc/miralis && dmesg | tail -n 1" >> "results/stats/$1_core_1.txt"
    RemoteExec $ADDRESS "taskset 2 cat /proc/miralis && dmesg | tail -n 1" >> "results/stats/$1_core_2.txt"
    RemoteExec $ADDRESS "taskset 3 cat /proc/miralis && dmesg | tail -n 1" >> "results/stats/$1_core_3.txt"
    RemoteExec $ADDRESS "taskset 4 cat /proc/miralis && dmesg | tail -n 1" >> "results/stats/$1_core_4.txt"
}
