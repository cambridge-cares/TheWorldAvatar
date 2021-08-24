####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Last Update Date: 20 August 2021                 #
####################################################

"""This script developed functuions for creating the Legend for Choropleth Maps via Matplotlib."""

import matplotlib.pyplot as plt
import matplotlib as mpl

fig, ax = plt.subplots(figsize=(0.5, 12))
fig.subplots_adjust(bottom=0.5)

cmap = mpl.colors.ListedColormap(['#006837', '#1a9850', '#66bd63', '#a6d96a', '#d9ef8b', '#ffffbf', '#fee08b', '#fdae61', '#f46d43', '#d73027', '#a50026'])
# cmap.set_over('0.25')
# cmap.set_under('0.75')

bounds = [0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200]
norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
cb2 = mpl.colorbar.ColorbarBase(ax, 
                                cmap = cmap,
                                norm = norm,
                                boundaries = bounds,
                                extend='max',
                                ticks = bounds,
                                spacing = 'proportional',
                                orientation = 'vertical')
cb2.set_label('Electricity Consumptions (GWh)')
fig.show()

fig.set_size_inches(0.5, 12)
plt.savefig('legend-electricity_consumption.svg', dpi=200, bbox_inches = "tight", transparent = True)

    
