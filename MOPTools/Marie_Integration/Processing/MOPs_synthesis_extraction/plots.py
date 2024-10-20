import matplotlib.pyplot as plt
import numpy as np

def barplot_grouped(labels, data, title="Bar Plot", xlabel="X-axis", ylabel="Y-axis", color=None):
    """
    Generates a grouped bar plot with three bars per x-axis label.

    Parameters:
        labels (list or array-like): The labels for each x-axis group.
        data (list of lists): A 2D list where each sublist contains the heights of the bars for each group.
                              Should contain three sublists, one for each set of bars per x-axis label.
        title (str): The title of the plot. Default is "Bar Plot".
        xlabel (str): The label for the x-axis. Default is "X-axis".
        ylabel (str): The label for the y-axis. Default is "Y-axis".
        color (list): A list of colors for each group. Should contain three colors. Default is None.
    """
    num_groups              = len(labels)
    num_bars                = 3  # As specified, there should be three bars for each x-axis instance

    # Default color scheme if no color is provided
    if color is None:
        color               = ['blue', 'green', 'red']

    # Check that data and color lists match the required dimensions
    assert len(data) == num_bars, "Data should contain exactly three sub-lists."
    assert len(color) == num_bars, "Color should contain exactly three colors."

    # Set the width of each bar and the positions
    bar_width               = 0.25
    x = np.arange(num_groups)  # X-axis label locations

    plt.figure(figsize=(10, 6))
    plt.bar(x + 0 * bar_width, data[0], width=bar_width, color=color[0], label=f'All procedures')
    plt.bar(x + 1 * bar_width, data[1], width=bar_width, color=color[1], label=f'Linked to MOP')
    plt.bar(x + 2 * bar_width, data[2], width=bar_width, color=color[2], label=f'Not linked to MOP')
    
    font_size                = 16
    plt.title(title, fontsize=font_size + 4)   # Increase title font size
    plt.xlabel(xlabel, weight='bold', fontsize=font_size)
    plt.ylabel(ylabel, weight='bold', fontsize=font_size)
    plt.xticks(x + bar_width, labels, fontsize=font_size)  # Increase font size for x-axis labels
    plt.yticks(fontsize=font_size)  # Increase font size for y-axis labels
    plt.legend(fontsize=font_size)  # Increase font size for the legend
    plt.show()

def barplot(labels, data, title="Bar Plot", xlabel="X-axis", ylabel="Y-axis", color="blue"):
    """
    Generates a bar plot.

    Parameters:
        data (list or array-like): The heights of the bars.
        labels (list or array-like): The labels for each bar.
        title (str): The title of the plot. Default is "Bar Plot".
        xlabel (str): The label for the x-axis. Default is "X-axis".
        ylabel (str): The label for the y-axis. Default is "Y-axis".
        color (str or list): The color of the bars. Can be a single color or a list of colors. Default is "blue".
    """
    print("data: ", data)
    plt.figure(figsize=(10, 6))
    plt.bar(labels, data, color=color)
    plt.title(title)
    plt.xlabel(xlabel, weight='bold')
    plt.ylabel(ylabel, weight='bold')
    plt.show()
    return 

def main():

    data                                = [[203, 79, 124], [31, 18, 13], [3, 1, 2], [3, 2,1], [1,1,0]]
    data                                = [[203,31,3,3,1], [79,18,1,2,1], [124, 13, 2, 1,0]]
    labels                              = ["1", "2", "3", "4", "5"]
    barplot_grouped(labels, data, "Extracted Synthesis", "Number of procedures for single product", "Number of occurences" )
    input_path                                              = f"../Data/fift10_chemicals1/10.1021_acsami.8b02015.json"



if __name__ == "__main__":
    main()