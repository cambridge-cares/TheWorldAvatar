# This module creates colorbar legend for DTVF

from pathlib import Path
import os

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap

# Specify filepath to store colorbar legend
f = 'colorbar.png'
fp = os.path.join(Path(__file__).parent, 'visualisation','data', f)

# Specify colors for colorbar (must match settings in data.json)
colors = ["#F7BA56", "#AD0709"]
values = [100000, 400000]
ticks = range(values[0], values[1]+1, 100000)
labels = [f'£ {t:,}' for t in ticks]

# Create figure and colorbar location
fig = plt.figure( figsize=(1,4) )
ax = fig.add_axes([0.05, 0.05, 0.25, 0.9])

# Create colorbar values
norm = mpl.colors.Normalize(vmin=min(values), vmax=max(values))  
normed_vals = norm(values)
cmap = LinearSegmentedColormap.from_list("mypalette", list(zip(normed_vals, colors)), N=1000)  
cb = mpl.colorbar.ColorbarBase(ax, cmap=cmap, norm=norm, orientation='vertical')

# Add tick locations and desired ticklabels
cb.set_ticks(ticks)
cb.ax.set_yticklabels(labels)

plt.savefig(fp, dpi=300, bbox_inches='tight')
