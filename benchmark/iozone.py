import matplotlib.pyplot as plt
import pandas as pd
import re
import numpy as np
from plot import *


# Function to parse Iozone output
def parse_iozone_output(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()

    # Find the header line and the data section
    header_line = None
    data_lines = []
    for line in lines:
        if "reclen" in line and header_line is None:
            header_line = line
        elif re.match(r"^\s*\d+", line):
            data_lines.append(line.strip())

    # Parse header and data
    if not header_line or not data_lines:
        raise ValueError("Invalid Iozone output format")

    headers = header_line.split()[:6]  # Ignore 'fwrite', 'frewrite', 'fread', 'freread' 
    data = []
    for line in data_lines:
        data.append([float(val) for val in line.split()])

    df = pd.DataFrame(data, columns=headers)

    df = df[df['reclen'] == 128.0][['kB', 'write', 'read']]

    return df


def process_values(arr):
    return np.mean(np.sort(arr, axis=0)[3:22], axis=0).astype(float), np.var(np.sort(arr, axis=0)[3:22], axis=0).astype(float)


if __name__ == "__main__":
    indices = ['write', 'rewrite', 'read', 'reread', 'random read', 'random write', 'bkwd read', 'record rewrite', 'stride read']

    title = 'IOzone throughput in [KB/s]'
    fontsize=14
    ms=12 # Marker size

    values = extract("iozone", parse_iozone_output, PATH_VISIONFIVE2)
    values = list(map(lambda x: x[1], values))

    values_read = np.array(list(map(lambda x: np.array(x['read']), values))) / 1000
    values_write = np.array(list(map(lambda x: np.array(x['write']), values))) / 1000
    values_len = np.array(list(map(lambda x: np.array(x['kB']), values))[0])
    values_len = list(map(lambda x: str(x), values_len))


    # Process values to get both the mean and variance
    values_read_board, var_read_board = process_values(values_read[:25])
    values_read_offload, var_read_offload = process_values(values_read[25:50])
    values_read_protect, var_read_protect = process_values(values_read[50:75])

    values_write_board, var_write_board = process_values(values_write[:25])
    values_write_offload, var_write_offload = process_values(values_write[25:50])
    values_write_protect, var_write_protect = process_values(values_write[50:75])

    print("Overhead read offload :", np.sort(values_read_offload / values_read_board))
    print("Overhead read protect :", np.sort(values_read_protect / values_read_board))

    print("Overhead write offload :", np.sort(values_write_offload / values_write_board))
    print("Overhead write protect :", np.sort(values_write_protect / values_write_board))

    values_len = ['128 K', '256 K', '512 K', '1 M', '2 M', '4 M', '8 M', '16 M', '32 M', '64 M', '128 M']

    fig, axes = plt.subplots(2, 1, num=1, figsize=(6.5, 3.6))  # Create subplots in a single figure
    fig.tight_layout()

    # First subplot (Read performance with variance)
    axes[0].plot(values_len, values_read_board, label=names['Board'], marker=markers['Board'], ms=ms, color=curve_colors['Board'])
    axes[0].plot(values_len, values_read_offload, label=names['Offload'], marker=markers['Offload'], ms=ms, color=curve_colors['Offload'])
    axes[0].plot(values_len, values_read_protect, label=names['Protect'], marker=markers['Protect'], ms=ms, color=curve_colors['Protect'])
    axes[0].fill_between(values_len, values_read_board - np.sqrt(var_read_board), values_read_board + np.sqrt(var_read_board), alpha=0.2, color=curve_colors['Board'])
    axes[0].fill_between(values_len, values_read_offload - np.sqrt(var_read_offload), values_read_offload + np.sqrt(var_read_offload), alpha=0.2, color=curve_colors['Offload'])
    axes[0].fill_between(values_len, values_read_protect - np.sqrt(var_read_protect), values_read_protect + np.sqrt(var_read_protect), alpha=0.2, color=curve_colors['Protect'])
    axes[0].set_ylabel("Read (MiB/s)", fontsize=fontsize)  
    axes[0].set_xticklabels([])
    axes[0].tick_params(labelsize=fontsize - 2.7)
    axes[0].margins(0)
    axes[0].set_ylim(17,21)
    axes[0].set_yticks(np.arange(17, 21+1, 1.0))

    # Second subplot (Write performance with variance)
    axes[1].plot(values_len, values_write_board, label=names['Board'], marker=markers['Board'], ms=ms, color=curve_colors['Board'])
    axes[1].plot(values_len, values_write_offload, label=names['Offload'], marker=markers['Offload'], ms=ms, color=curve_colors['Offload'])
    axes[1].plot(values_len, values_write_protect, label=names['Protect'], marker=markers['Protect'], ms=ms, color=curve_colors['Protect'])
    axes[1].fill_between(values_len, values_write_board - np.sqrt(var_write_board), values_write_board + np.sqrt(var_write_board), alpha=0.2, color=curve_colors['Board'])
    axes[1].fill_between(values_len, values_write_offload - np.sqrt(var_write_offload), values_write_offload + np.sqrt(var_write_offload), alpha=0.2, color=curve_colors['Offload'])
    axes[1].fill_between(values_len, values_write_protect - np.sqrt(var_write_protect), values_write_protect + np.sqrt(var_write_protect), alpha=0.2, color=curve_colors['Protect'])
    axes[1].set_ylabel("Write (MiB/s)", fontsize=fontsize)  
    axes[1].set_xlabel("File size", fontsize=fontsize)
    axes[1].tick_params(labelsize=fontsize - 2.7)
    axes[1].margins(0)

    axes[1].legend(ncols=3, loc="lower center", fontsize=fontsize - 2)
    # axes[1].set_title("Write Performance")
    axes[1].set_xticks(values_len)
    axes[1].set_ylim(13,17)
    axes[1].set_yticks(np.arange(13, 17+1, 1.0))
    
    fig.suptitle(TITLE)

    plt.tight_layout(rect=[0, 0, 1, 1]) 
    plt.savefig(f"plots/iozone.pdf",format="pdf")
