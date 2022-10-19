from matplotlib.mlab import csd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure

# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
#
def start():
    data = pd.read_csv('SO2_1HR_FLAT_CONC.DAT', delim_whitespace=True, skiprows=range(0,8), header=None, names=['X','Y','AVERAGE CONC', 'ZELEV', 'ZHILL','ZFLAG','AVE','GRP','RANK','NET ID','DATE(CONC)'])
    x_all = data['X']
    y_all = data['Y']

    x_set = sorted(set(x_all))
    y_set = sorted(set(y_all))

    x_matrix = np.empty((len(x_set), len(y_set)))
    y_matrix = np.empty((len(x_set), len(y_set)))

    for i in range(len(x_set)):
        for j in range (len(y_set)):
            x_index = x_set.index(x_set[i])
            y_index = y_set.index(y_set[j])
            x_matrix[x_index,y_index] = x_set[i]
            y_matrix[x_index,y_index] = y_set[j]

    conc_list = data['AVERAGE CONC']
    conc_matrix = np.empty((len(x_set), len(y_set)))

    for i in range(len(x_all)):
        x_index = x_set.index(x_all[i])
        y_index = y_set.index(y_all[i])
        conc_matrix[x_index,y_index] = conc_list[i]

    contour_level = 30
    fig, ax = plt.subplots()

    cs = ax.contourf(x_matrix, y_matrix, conc_matrix, levels=contour_level,cmap=plt.cm.jet)
    fig.colorbar(cs,ax=ax)
    plt.savefig('AERTEST.png', dpi=600)
    plt.show()

if __name__ == '__main__':
    start()