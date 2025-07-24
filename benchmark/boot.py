import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes, mark_inset
from plot import *


# Read CSV
csv = pd.read_csv(f"{PATH_VISIONFIVE2}/boot.csv")

# Remove 'Firmware exit' column
csv = csv.drop('Firmware exit', axis=1)

# Move other column at the end
csv["IPI"] = csv.pop("IPI")
csv["Other"] = csv.pop("Other")
print(csv)

print("Firmware traps/s: ", csv["Other"][140:].mean())
print("Total traps/s:    ", csv[140:].mean().sum())
print("Total traps/s:    ", csv[140:].mean())

# Normalize each row to sum to 1
csv = csv.apply(lambda row: row / row.sum() if row.sum() > 0 else row, axis=1)

# Prepare data
data = {}
for col in csv.columns:
    data[col] = np.array(csv[col])

unit_of_time = np.arange(0, len(csv), 1)

fig, ax = plt.subplots()
fig.set_figheight(3.2)
fig.set_figwidth(5.4)

# Convert unit_of_time to labels (every 2 minutes)
unit_of_time_labels = list(map(lambda x: f"{x // 2}s", unit_of_time))

colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
blue = colors[0]
colors = colors[1:]
colors[5] = blue
print(blue)
print(colors)

# Compute cumulative sums for stacking
stacked_values = np.zeros_like(unit_of_time, dtype=float)
for i, (label, values) in enumerate(data.items()):
    ax.fill_between(unit_of_time, stacked_values, stacked_values + values, 
                    step="post", label=label, alpha=0.8, color = colors[i], edgecolor=None)
    stacked_values += values  # Stack values

# Set x-axis ticks
ax.set_xticks(unit_of_time[::50])
ax.set_xticklabels(unit_of_time_labels[::50], fontsize=11.2)

# Labels and formatting
ax.set_ylabel('Proportion of traps', fontsize=11.2)
ax.set_ylim(0, 1)
ax.set_xlim(0, 180) # max is 256

# Minor ticks for y-axis
ax.yaxis.set_minor_locator(mticker.MultipleLocator(.2))


# Define zoom area (e.g., first 20 CPU cycles)
x1, x2 = 18, 30
y1, y2 = 0.96, 1.00  # Zoom sur toute la hauteur

# Create inset
axins = zoomed_inset_axes(ax, zoom=10.5, loc="lower right")  # Facteur de zoom ajustable

# Replot same data in inset
stacked_values_zoom = np.zeros_like(unit_of_time[:x2], dtype=float)
i = 0
for label, values in data.items():
    axins.fill_between(unit_of_time[:x2], stacked_values_zoom, stacked_values_zoom + values[:x2], step="post", alpha=0.8, color = colors[i], edgecolor=None)
    stacked_values_zoom += values[:x2]
    i += 1

# Format inset
axins.set_xlim(x1, x2-4)
axins.set_ylim(y1, y2)
axins.set_xticks([])  # Enlever les ticks pour plus de lisibilité
axins.set_yticks([])

# Draw rectangle on main plot
mark_inset(ax, axins, loc1=1, loc2=3, fc="none", ec="black", linestyle="-")


# Place legend outside the plot
ax.legend(loc="lower center", bbox_to_anchor=(0.45, -0.32), fancybox=False, ncol=3, labelspacing=-0.06, columnspacing=0.6, frameon=False, fontsize=10.7)

# Adjust layout for better spacing
plt.tight_layout()


# Save as PDF
plt.savefig(f"plots/boot.pdf", format="pdf")
