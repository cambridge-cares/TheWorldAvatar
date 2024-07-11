import matplotlib.pyplot as plt
import matplotlib as mpl
from mycolorpy import colorlist as mcp
import numpy as np
import json


def create_colors():
    start = 1 * 10 ** 6
    end = 6 * 10 ** 9
    num_points = 12
    ticks = [1, 10, 100, 1000]

    array_for_mapbox_expression = np.logspace(
        np.log10(start), np.log10(end), num_points).tolist()
    array_for_colorbar = [x / (10.0 ** 6) for x in array_for_mapbox_expression]

    fig, ax = plt.subplots(figsize=(6, 1))
    fig.subplots_adjust(bottom=0.5)
    color1 = mcp.gen_color(cmap="summer", n=len(
        array_for_colorbar), reverse=True)

    colour_dict = {}
    for i in range(len(array_for_mapbox_expression) - 1):
        colour_dict[array_for_mapbox_expression[i + 1]] = color1[i]

    cmap = mpl.colors.ListedColormap(color1)

    norm = mpl.colors.LogNorm(vmin=start / 10 ** 6, vmax=end / 10 ** 6)

    cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
                                    norm=norm, ticks=ticks,
                                    orientation='horizontal')
    cb2.set_label('Estimated construction cost (million SGD)')
    cb2.set_ticklabels(['<1', '10', '100', '1000'])
    plt.savefig("cost.png", bbox_inches='tight',
                transparent=True, dpi=300)

    return colour_dict


def generate_mapbox_expressions(input_dict):
    expression = ["case"]
    sorted_dict = sorted(input_dict.items())
    for key, value in sorted_dict:
        expression.extend([
            ["<=", ["get", "cost"], key],
            value
        ])
    expression.append(sorted_dict[len(sorted_dict)-1][1])  # default colour
    f = open("mapbox_expression_cost.txt", "w")
    f.write(json.dumps(expression))
    f.close()


if __name__ == "__main__":
    colour_dict = create_colors()
    generate_mapbox_expressions(colour_dict)
