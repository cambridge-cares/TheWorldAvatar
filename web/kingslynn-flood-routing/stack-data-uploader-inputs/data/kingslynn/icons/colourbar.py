import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap

def draw_colourbar(colors, positions, tick_labels, filename):

    # Normalize the positions to the range [0, 1]
    norm_positions = [(pos - min(positions)) / (max(positions) - min(positions)) for pos in positions]

    # Create a custom colormap
    cmap = LinearSegmentedColormap.from_list("custom_cmap", list(zip(norm_positions, colors)))

    # Create a figure and a colorbar
    fig, ax = plt.subplots(figsize=(4, 1))
    fig.subplots_adjust(bottom=0.5)

    # Create a colorbar with the custom colormap
    cbar = fig.colorbar(plt.cm.ScalarMappable(cmap=cmap), cax=ax, orientation='horizontal')

    # Customize the ticks and labels
    cbar.set_ticks([0.0, 1.0])
    cbar.set_ticklabels(tick_labels)

    plt.savefig(filename)
    plt.close()

# Change in usage frequency of road segments

draw_colourbar(['#FF0000', '#C0C0C0', '#C0C0C0', '#00AA00'], [-0.5, -0.001, 0.001, 0.5], ['Decrease', 'Increase'], 'cb-tebc.png')

# Unused roads

draw_colourbar(['#C0C0C0','#FF0000'], [0, 0.02], ['Non-critical', 'Critical'], 'cb-unused.png')
