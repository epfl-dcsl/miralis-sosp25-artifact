import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

colors = {
    'Board': "#F8CECC",
    'Offload': "#D5E8D4",
    'Protect': "#DAE8FC",
    'Keystone': "#D5E8D4",
}

curve_colors = {
    'Board': "#B85450",
    'Offload': "#82B366",
    'Protect': "#6C8EBF",
}

names = {
    'Board' : 'Native',
    'Protect' : 'Miralis no-offload',
    'Offload' : 'Miralis',
    'Keystone' : 'Keystone enclave on Miralis'
}

hatches = {
    'Board' : 'xx',
    'Protect' : '//',
    'Offload' : '\\\\',
    'Keystone' : '\\\\'
}

markers = {
    'Board' : '1',
    'Protect' : '3',
    'Offload' : '2',
    'Keystone' : '+'
}

WITH_OFFLOAD=True
TITLE=""
PATH_PREMIER="results_premier"
PATH_VISIONFIVE2="results_visionfive2"

def plot_bar(x_ticks, data, path, native_performance, offset_unit, untily, ymin: float = 0, figsize = (8, 6), fontsize=None, valfontsize=10, legendsize=None, tick_rotation=0, val_shift=False, ncols=1, split=False, split_labels=None, split_label_height=1.0, columnspacing=None):
    x = np.arange(len(x_ticks), dtype=np.float64)  # the label locations
    if split:
        x[len(x)//2:] += 0.2
        print(x)
    width = 0.25  # the width of the bars
    multiplier = 0
    
    if len(data) == 2:
        width = 0.33

    fig, ax = plt.subplots(layout='constrained', figsize=figsize)

    for i, (attribute, measurement) in enumerate(data.items()):
        if attribute == "Offload" and not WITH_OFFLOAD:
            continue
        offset = width * multiplier
        rects = ax.bar(x + offset, measurement, width, hatch=hatches[attribute], label=names[attribute], color=colors[attribute], edgecolor='black', linewidth=1.5)
        multiplier += 1

    # Add labels, title, and legend
    ax.set_ylabel('Relative performance', fontsize=legendsize if legendsize is not None else fontsize)
    ax.set_xticks(x + width, x_ticks, fontsize=fontsize, rotation=tick_rotation)
    ax.tick_params(labelsize=fontsize)

    ax.axhline(y=1, color='black', linestyle='--')
    ax.set_ylim(ymin, untily)

    if ncols == 1:
        ax.legend(loc='lower right', fontsize=fontsize, columnspacing=columnspacing)
    else:
        ax.legend(loc='lower center', fontsize=fontsize, ncols=ncols, columnspacing=columnspacing)

    if len(data) == 2:
        width /= 2

    # Add another set of x_ticks on top
    ax.set_xticks(x + width, x_ticks)

    ax.set_xticklabels(x_ticks)  # This will add the regular x_ticks below

    # Annotate "test" for each x-tick on top
    for i, xtick in enumerate(x_ticks):
        shift = 0
        if val_shift:
            shift = 2 * ((i + 1) % 2) - 1 # +/- 1
            shift *= 0.03 # scale shift
        ax.annotate(
            native_performance[i], 
            xy=(x[i] + width, offset_unit + shift),  # Shift the annotation a little to the right
            ha='center', 
            va='bottom', 
            fontsize=valfontsize, 
            color='black'
        )

    if split_labels is not None:
        n = len(x)
        middle = (x[n//2 - 1] + x[n//2 + 1]) / 2
        ax.annotate(
            split_labels[0],
            xy=(middle * 0.5 - width, split_label_height),
            ha='center', 
            va='bottom', 
            fontsize=fontsize, 
            color='black'
        )
        ax.annotate(
            split_labels[1],
            xy=(middle * 1.5 - width, split_label_height),
            ha='center', 
            va='bottom', 
            fontsize=fontsize, 
            color='black'
        )

    plt.suptitle(TITLE)

    plt.savefig(f"plots/{path}.pdf", format="pdf")



def extract_workload(file_path):
    return str(file_path).split('/')[-1].split('_')[1]

def extract_iteration(file_path):
    return int(str(file_path).split('/')[-1].split('_')[2].split('.')[0])

def is_workload(file_path, name):
    return str(file_path).split('/')[-1].startswith(f"{name}_") and "stats" not in str(file_path)

def extract(key, extractor, path: str):
    folder_path = Path(path)

    output = []

    for file_path in sorted(folder_path.rglob('*')):
        # Recursively search all files
        if is_workload(file_path, key):
            output.append((extract_workload(file_path),extractor(file_path)))

    return output
