import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import re
import numpy as np
from plot import *

def extract_get(file_path):
    return extract_value(file_path, "GET")

def extract_set(file_path):
    return extract_value(file_path, "SET")

def extract_value(file_path, typ):

    with open(file_path, 'r') as file:
        lines = file.readlines()

    lines = lines[18:][:-2]

    def remove_duplicate_spaces(s):
        return re.sub(r'\s+', ' ', s).strip()

    lines = list(map(remove_duplicate_spaces, lines))
    lines = list(filter(lambda x: not x.startswith('---'), lines))
    lines = list(filter(lambda x: not x.startswith(typ), lines))
    lines = list(map(lambda x: np.array(x.split(' ')[1:]).astype(float), lines))

    return lines

if __name__ == "__main__":
    title = 'Network latency'
    fontsize=14
    ms=12

    fig, axes = plt.subplots(1, 2, num=1, figsize=(6.5, 3.6))  # Create subplots in a single figure

    v = [extract_get, extract_set]
    v2 = ['Get', 'Set']

    idx = 0
    for e in v:
        output = extract(f"memcached-cdf", e, PATH_VISIONFIVE2)

        board_values = list(map(lambda x:float(x[0]), output[0][1]))
        percentile_board = list(map(lambda x:float(x[1]), output[0][1]))

        offload_values = list(map(lambda x:float(x[0]), output[1][1]))
        percentile_offload = list(map(lambda x:float(x[1]), output[1][1]))

        protect_values = list(map(lambda x:float(x[0]), output[2][1]))
        percentile_protect = list(map(lambda x:float(x[1]), output[2][1]))

        # First subplot (Read performance)
        axes[idx].plot(board_values, percentile_board, label=names['Board'], color=curve_colors['Board'])
        axes[idx].plot(offload_values, percentile_offload, label=names['Offload'], color=curve_colors['Offload'])
        axes[idx].plot(protect_values, percentile_protect, label=names['Protect'], color=curve_colors['Protect'])
        axes[idx].set_xlabel(f"{v2[idx]} latency ($ms$)", fontsize=fontsize)  # Label for the y-axis
        axes[idx].xaxis.set_major_formatter(ticker.FormatStrFormatter('%.1f')) 
        # Label for the y-axis
        if idx == 1:
            axes[idx].legend(fontsize=fontsize - 2)  # Show legend
        elif idx == 0:    
            axes[idx].set_ylabel("Percentile", fontsize=fontsize)
        axes[idx].set_xlim(0,1.2)
        axes[idx].set_ylim(10,100)

        idx += 1

    # fig.suptitle(TITLE)

    plt.tight_layout(rect=[0, 0, 1, 0.96])  # Adjust layout to fit title
    plt.savefig(f"plots/latency.pdf", format="pdf")


