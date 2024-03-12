import matplotlib.pyplot as plt
import matplotlib as mpl
from mycolorpy import colorlist as mcp

def create_colors():
    bounds = range(0, 31, 5)

    fig, ax = plt.subplots(figsize=(6, 1))
    fig.subplots_adjust(bottom=0.5)
    color1 = mcp.gen_color(cmap="cool", n=len(bounds))
    print(color1)

    cmap = mpl.colors.ListedColormap(color1)

    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)

    cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                    norm=norm,
                                    ticks=bounds,
                                    spacing='proportional',
                                    orientation='horizontal')
    cb2.set_label('Heat emissions (MW)')
    fig.show()
    x = 2

if __name__ == "__main__":
    create_colors()
