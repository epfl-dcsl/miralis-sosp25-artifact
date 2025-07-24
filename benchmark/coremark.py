import numpy as np
from plot import *

DATA_PATH = PATH_VISIONFIVE2

def extract_iterations_per_second(file_path):
    extensions = ["core", "fft", "gaussian", "jpeg", "livermore", "neuralnetwork", "sha", "xml", "zip"]

    with open(file_path, 'r') as file:
        lines = file.readlines()
    return [float(lines[7].split('=')[1])]

def extract_iterations_per_second_jpeg(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    return [float(lines[7+21].split('=')[1])]

if __name__ == "__main__":
    title = 'Coremarkpro - [iterations/s] - multicore'

    workloads = ["xml", "gaussian","zip", "livermore", "neuralnetwork", "sha", "core", "jpeg"]
    output = []
    for w in workloads:
        if w == "jpeg":
            output.append(extract(f"coremarkpro-{w}", extract_iterations_per_second_jpeg, DATA_PATH))
        else:
            output.append(extract(f"coremarkpro-{w}", extract_iterations_per_second, DATA_PATH))

    values = []
    for i in range(len(workloads)):
        current = list(map(lambda x: x[1][0], output[i]))
        values.append(np.array(current))

    values = np.array(values).T

    normal = np.mean(values[0:5], axis=0)

    native = list(map(lambda x: "{:.1f}it/s".format(x) if x > 10 else "{:.2f}it/s".format(x) , normal))

    board = np.mean(values[0:5], axis=0) / normal
    offload = np.mean(values[5:10], axis=0) / normal
    protect = np.mean(values[10:15], axis=0) / normal

    workloads[4] = 'nn'

    print("Offload: ", np.sort(offload))
    print("Protect: ", np.sort(protect))

    print((1 - np.mean(protect)) * 100)

    plot_bar(workloads, {
        'Board': board,
        'Offload': offload,
        'Protect': protect,
    }, 'coremark', native, 1.002, 1.02, ymin=0.8, figsize=(6.5, 2.7), fontsize=14, valfontsize=11)
