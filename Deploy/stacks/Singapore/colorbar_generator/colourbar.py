import matplotlib.pyplot as plt
import matplotlib as mpl
from mycolorpy import colorlist as mcp


def create_colors():
    # Heat emissions
    bounds = range(0, 31, 5)

    fig, ax = plt.subplots(figsize=(6, 1))
    fig.subplots_adjust(bottom=0.5)
    color1 = mcp.gen_color(cmap="cool", n=len(bounds))

    colour_dict = {}
    for i in range(len(bounds)):
        colour_dict[bounds[i]] = color1[i]

    f = open("heat_emissions_colours.txt", "w")
    f.write(str(colour_dict))
    f.close()

    cmap = mpl.colors.ListedColormap(color1)

    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)

    cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                    norm=norm,
                                    ticks=bounds,
                                    spacing='proportional',
                                    orientation='horizontal')
    cb2.set_label('Heat emissions (MW)')
    plt.savefig("heat_emissions_colorbar.png", bbox_inches='tight',
                transparent=True, dpi=300)

    # Population
    bounds = range(0, 51000, 5000)
    fig2, ax = plt.subplots(figsize=(6, 1))
    fig2.subplots_adjust(bottom=0.5)
    color1 = mcp.gen_color(cmap="Reds", n=len(bounds))

    colour_dict = {}
    for i in range(len(bounds)):
        colour_dict[bounds[i]] = color1[i]

    f = open("population_density_colours.txt", "w")
    f.write(str(colour_dict))
    f.close()

    cmap = mpl.colors.ListedColormap(color1)

    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)

    cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                    norm=norm,
                                    spacing='proportional',
                                    orientation='horizontal')
    cb2.set_label('Population density (#/km$^2$)')
    plt.savefig("population_density.png", bbox_inches='tight',
                transparent=True, dpi=300)


if __name__ == "__main__":
    create_colors()
