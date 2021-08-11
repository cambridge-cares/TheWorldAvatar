import numpy as np 
import matplotlib.pyplot as plt 


l1 = 400
l2 = 600
FP_OG = np.linspace(0,30,l1)
DC_OG = np.linspace(-60,-200,l2)
FP,DC = np.meshgrid(FP_OG,DC_OG)

fp = ((FP_OG - min(FP_OG))/max(FP_OG)-min(FP_OG))
dc = (2*(DC_OG - min(DC_OG))/((max(DC_OG)-min(DC_OG))))-1


GRID = np.zeros((l1,l2))
for i in range(l1):
    for j in range(l2):
        GRID[i,j] = fp[i] * dc[j]

GRID = GRID.T

import matplotlib.pylab as pylab
params = {'legend.fontsize': 'large',
          'figure.figsize': (15, 5),
         'axes.labelsize': 'large',
         'axes.titlesize':'large',
         'xtick.labelsize':'large',
         'ytick.labelsize':'large'}
pylab.rcParams.update(params)

fig,ax = plt.subplots(figsize=(6,5))
plt.subplots_adjust(right=0.95,left=0.15)
plt.contourf(FP,DC,GRID,512,cmap='coolwarm',antialiased=False)
ax.set_xlabel('Fuel Poverty (%)')
ax.set_ylabel('Change in Cost (£/year/household)')
cax = plt.colorbar(ticks=[-1,-0.5, 0, 0.5,1],label='Inequality Index (-)')

plt.savefig('figure_output/inequality_metric.png')
plt.savefig('figure_output/inequality_metric.pdf')


