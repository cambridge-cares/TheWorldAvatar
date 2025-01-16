# This module creates colorbar legend for DTVF

import os
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap


#
#  1) Define min and max values for colorbar (£)
# 
#NOTE: Align values and colors with data.json
cb_min = 50000
cb_max = 1200000

#
# 2) Create colorbar legend
# 
# Specify filepath to store colorbar legend
f = 'value_colorbar.png'
fp = os.path.join(Path(__file__).parent.parent.parent, 'StackDeployment', 'inputs', 
                  'stack-manager', 'inputs', 'data', 'visualisation', 'data', 'icons', f)

# Specify colors for colorbar (must match settings in data.json)
colors = ["#0769AD", "#AD0202"]
values = [cb_min, cb_max]
ticks = range(values[0], values[1]+1, 50000)
labels = [f'£ {t:,}' for t in ticks]
labels = [labels[i] if i%2==1 else None for i in range(len(labels))]

# Create figure and colorbar location
fig = plt.figure( figsize=(1,4) )
ax = fig.add_axes([0.05, 0.05, 0.25, 0.9])

# Create colorbar values
norm = mpl.colors.Normalize(vmin=min(values), vmax=max(values))  
normed_vals = norm(values)
cmap = LinearSegmentedColormap.from_list("mypalette", list(zip(normed_vals, colors)), N=100)  
cb = mpl.colorbar.ColorbarBase(ax, cmap=cmap, norm=norm, orientation='vertical')

# Add tick locations and desired ticklabels
cb.set_ticks(ticks)
cb.ax.set_yticklabels(labels)

#plt.show
plt.savefig(fp, dpi=300, bbox_inches='tight')