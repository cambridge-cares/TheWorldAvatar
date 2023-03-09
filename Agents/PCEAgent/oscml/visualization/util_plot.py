import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import seaborn as sns

def plot_heatmap(matrix):
    f, ax = plt.subplots(figsize=(10, 6))
    hm = sns.heatmap(round(matrix,2), annot=True, ax=ax, cmap="coolwarm",fmt='.2f',
                linewidths=.05)
    f.subplots_adjust(top=0.93)
    t= f.suptitle('Correlation Heatmap', fontsize=14)
    
def plot_scatter(df, column_x, column_y):
    plt.figure(figsize=(4, 4))
    left, width = 0.1, 0.65
    bottom, height = 0.1, 0.65
    rect_scatter = [left, bottom, width, height]
    #ax_scatter = plt.axes(rect_scatter)
    #ax_scatter.set_xlim((-2, 14))
    #ax_scatter.set_ylim((-2, 14))

    plt.scatter(df[column_x], df[column_y], s = 1, c="b", alpha=0.5)
    plt.xlabel(column_x)
    plt.ylabel(column_y)
    plt.legend(loc='upper left')
    plt.show()
    
def plot(y, y_pred, column_target=''):
    plt.figure(figsize=(8, 8))
    left, width = 0.1, 0.65
    bottom, height = 0.1, 0.65
    rect_scatter = [left, bottom, width, height]
    ax_scatter = plt.axes(rect_scatter)
    ax_scatter.set_xlim((-2, 14))
    ax_scatter.set_ylim((-2, 14))

    plt.scatter(y, y_pred, s = 1, c="b", alpha=0.5)
    plt.xlabel(column_target + " true")
    plt.ylabel(column_target + " predicted")
    plt.legend(loc='upper left')
    plt.show()
    
def plot_3d(x, y, z, xlabel='x', ylabel='y'):
    fig = plt.figure()
    ax = fig.gca(projection='3d')
    surf=ax.plot_trisurf(x, y, z, cmap=plt.cm.viridis, linewidth=0.2)
    # Other palette
    #surf = ax.plot_trisurf(x[:,0], x[:,1], y, cmap=plt.cm.jet, linewidth=0.01)
    fig.colorbar( surf, shrink=0.5, aspect=5)
    #rotate
    #ax.view_init(30, 100)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.show()