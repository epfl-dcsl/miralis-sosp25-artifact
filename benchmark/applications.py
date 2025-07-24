import re
import numpy as np
from plot import *

def ycsb_tp(filename):
    # Initialize the data to store results
    latencies = []

    # Open the text file for reading
    with open(filename, 'r') as file:
        content = file.readlines()

        # Iterate through each line in the file
        for line in content:
            if line.startswith("[OVERALL]"):
                if "Throughput(ops/sec)" in line:
                    latencies.append(float(line.split(',')[2].strip()))

    return latencies[0]


def sysbench_tp(filename):

    # Read the file
    with open(filename, "r") as file:
        content = file.read()

    # Extract latencies from the output
    average_latency = re.search(r"total number of events:\s+([\d.]+)", content)
    percentile_95_latency = re.search(r"95th percentile:\s+([\d.]+)", content)

    # Convert extracted values to floats
    average_latency = float(average_latency.group(1)) if average_latency else None
    percentile_95_latency = float(percentile_95_latency.group(1)) if percentile_95_latency else None

    # Return the results
    return average_latency / 300

def time_to_seconds(time_str):
    # Split the string at 'm' to separate minutes and seconds
    minutes, seconds = time_str.split('m')

    # Remove the 's' from the seconds part and convert both parts to float
    seconds = float(seconds.replace('s', ''))

    # Convert minutes to seconds and return the total
    return float(minutes) * 60 + seconds

# Function to parse time values from the input text file
def parse_times(filename):
    real_times = []
    user_times = []
    sys_times = []

    # Open and read the file
    with open(filename, 'r') as f:
        real_times = []
        user_times = []
        sys_times = []

        lines = f.readlines()  # Read all lines from the file

        i = 0  # Index for line processing
        while i < len(lines):
            # Process the next 3 non-empty lines
            if i + 2 < len(lines) and lines[i].strip().startswith("real"):
                real_line = lines[i].strip()
                user_line = lines[i+1].strip()
                sys_line = lines[i+2].strip()

                # Extract time values (ignoring 'm' and 's' part)
                real_time = time_to_seconds(real_line.split()[1])  # e.g., 0.002
                user_time = time_to_seconds(user_line.split()[1])  # e.g., 0.000
                sys_time = time_to_seconds(sys_line.split()[1])    # e.g., 0.001

                # Append the times to the respective lists
                real_times.append(real_time)
                user_times.append(user_time)
                sys_times.append(sys_time)

                # Skip the next two lines since we've already processed them
                i += 3
            else:
                # If we can't find a full block (real, user, sys), just move to the next non-empty line
                i += 1

    return float(np.mean(real_times))
    # val = float(np.mean(real_times)) / 60 / 60
    # return 1 / val


if __name__ == "__main__":
    title = 'Relative speedup'

    workloads = ["redis-kv", "memcached-kv", "mysql", "redis-compilation"] #, "linux-compilation"]
    extractor = [ycsb_tp, ycsb_tp, sysbench_tp, parse_times] #, parse_times]
    output = []

    values = []
    for w, ex in zip(workloads, extractor):
        current = extract(w, ex, PATH_VISIONFIVE2)
        current = list(map(lambda x: x[1], current))
        values.append(np.array(current))
    for w, ex in zip(workloads, extractor):
        current = extract(w, ex, PATH_PREMIER)
        current = list(map(lambda x: x[1], current))
        values.append(np.array(current))
    
    values = np.array(values).T

    normal = np.mean(values[0:5], axis=0)
    
    native = list(map(lambda x: "{:.3f}".format(x), normal))

    native[0] = f"{float(native[0]):.0f} op/s"
    native[1] = "{:.0f} op/s".format(float(native[1]))
    native[2] = "{:.1f} op/s".format(float(native[2]))
    native[3] = f"{float(native[3]):.1f}s"
    native[4] = "{:.0f} op/s".format(float(native[4]))
    native[5] = "{:.0f} op/s".format(float(native[5]))
    native[6] = "{:.1f} op/s".format(float(native[6]))
    native[7] = f"{float(native[7]):.1f}s"

    board = np.mean(values[0:5], axis=0) / normal
    offload = np.mean(values[5:10], axis=0) / normal
    protect = np.mean(values[10:15], axis=0) / normal

    print("Workloads : ", workloads)
    print("Offload : ", offload)
    print("Protect : ", protect)

    # Invert build time to ensure higher is better
    offload[3] = 1/offload[3]
    offload[7] = 1/offload[7]
    protect[3] = 1/protect[3]
    protect[7] = 1/protect[7]

    # Patch name of workloads before display
    labels = ["Redis", "Memcached", "MySQL", "GCC"] * 2

    plot_bar(labels, {
        'Board': board,
        'Offload': offload,
        'Protect': protect,
    }, 'applications', native, 1.09, 1.2, figsize=(6.5, 4.2), fontsize=14, legendsize=14, valfontsize=11, tick_rotation=25, val_shift=True, ncols=3, split=True, split_labels=["VisionFive 2", "HiFive Premier P550"], split_label_height=1.202, columnspacing=1.1)
