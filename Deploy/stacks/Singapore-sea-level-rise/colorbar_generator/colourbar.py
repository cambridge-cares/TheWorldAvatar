import matplotlib.pyplot as plt
import matplotlib as mpl
from mycolorpy import colorlist as mcp
import numpy as np
import json


def create_colors():
    start = 0
    end = 1.1 * 10**10
    num_points = 12

    array_for_mapbox_expression = np.linspace(start, end, num_points)
    array_for_colorbar = np.linspace(
        start, end / 10**6, num_points)  # convert to million

    fig, ax = plt.subplots(figsize=(6, 1))
    fig.subplots_adjust(bottom=0.5)
    color1 = mcp.gen_color(cmap="summer", n=len(
        array_for_colorbar), reverse=True)

    colour_dict = {}
    for i in range(len(array_for_mapbox_expression) - 1):
        colour_dict[array_for_mapbox_expression[i + 1]] = color1[i]

    cmap = mpl.colors.ListedColormap(color1)

    norm = mpl.colors.BoundaryNorm(array_for_colorbar, cmap.N)

    cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                    norm=norm,
                                    orientation='horizontal')
    cb2.set_label('Estimated construction cost (million SGD)')
    plt.savefig("cost.png", bbox_inches='tight',
                transparent=True, dpi=300)

    return colour_dict


def generate_mapbox_expressions(input_dict):
    expression = ["case"]
    for key, value in sorted(input_dict.items()):
        expression.extend([
            ["<=", ["get", "cost"], key],
            value
        ])
    expression.append("#8D99AE")  # default colours
    f = open("mapbox_expression.txt", "w")
    f.write(json.dumps(expression))
    f.close()


if __name__ == "__main__":
    colour_dict = create_colors()
    generate_mapbox_expressions(colour_dict)
