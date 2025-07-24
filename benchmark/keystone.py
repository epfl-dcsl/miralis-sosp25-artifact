from pathlib import Path
import numpy as np
from plot import *


def extract_keystone(path: str):
    folder_path = Path(path)

    output = []

    for file_path in sorted(folder_path.rglob('*')):
        output.append(extract_keystone_value(file_path))

    return np.array(output)

def extract_keystone_value(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    lines = list(filter(lambda x: "iruntime" in x, lines))
    lines = list(map(lambda x: int(x.split(" ")[1]), lines))
    lines = np.array(lines)

    return np.mean(lines)

if __name__ == "__main__":
    title = 'Coremarkpro - [iterations/s] - multicore'

    workloads = ["aes", "dhrystone", "miniz", "norx", "prime", "sha", "sort"]
    
    output_board = extract_keystone(f"{PATH_VISIONFIVE2}/keystone_board")
    output_keystone = extract_keystone(f"{PATH_VISIONFIVE2}/keystone_miralis")

    native = output_board.copy() / 1000 / 1000
    native = list(map(lambda x: "{} s".format(round(x,2)), native))

    output_keystone = output_board / output_keystone
    output_board /= output_board

    print("Output keystone performance :", np.sort(output_keystone))
    print("Mean value : ", np.mean(output_keystone))

    plot_bar(workloads, {
        'Board': output_board,
        'Keystone': output_keystone,
    }, 'keystone', native, 1.004, 1.025, ymin=0.8, figsize=(6.5, 2.7), fontsize=14, valfontsize=11)

