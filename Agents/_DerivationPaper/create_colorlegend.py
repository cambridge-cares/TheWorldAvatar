# This module creates colorbar legend for DTVF

import os
import json
import pandas as pd
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap


#
#  1) Extract min and max values for colorbar
# 
# Quantile cutoff value for values to be included in colorbar
q = 0.05
files = ['affected_buildings_first_round_adj.geojson',
         'affected_buildings_second_round_adj.geojson']
cb_min = []
cb_max = []
for f in files:
    fpath = os.path.join(Path(__file__).parent, 'visualisation','data', f)
    # To ensure correct formatting of GBP symbol suppress ascii encoding and use UTF-8
    with open(fpath, encoding='utf-8') as fp:
        data = json.load(fp)
    # Extract values from geojson
    values = [d['properties']['Property market value (£)'] for d in data['features']]
    v = pd.Series(values)
    cb_min.append(v.quantile(q))
    cb_max.append(v.quantile(1-q))

# Extract min and max values and round to nearest 10k value
cb_min = int(round(min(cb_min)/10000,0)*10000)
cb_max = int(round(max(cb_max)/10000,0)*10000)

#
# 2) Create colorbar legend
# 
# Specify filepath to store colorbar legend
f = 'colorbar.png'
fp = os.path.join(Path(__file__).parent, 'visualisation','data', f)

# Specify colors for colorbar (must match settings in data.json)
colors = ["#0275C0", "#AD0709"]
values = [cb_min, cb_max]
ticks = range(values[0], values[1]+1, 20000)
labels = [f'£ {t:,}' for t in ticks]
labels = [labels[i] if i%2==0 else None for i in range(len(labels))]

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

#plt.show
plt.savefig(fp, dpi=300, bbox_inches='tight')
