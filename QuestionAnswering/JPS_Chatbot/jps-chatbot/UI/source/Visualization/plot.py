import time

import matplotlib.pyplot as plt
import matplotlib.text
import matplotlib.text as text
import matplotlib.pyplot as plt
import matplotlib.text as text
import hashlib


def plot_simple_graph(x, y, x_label, y_label):
    fig, ax = plt.subplots()
    plt.plot(x, y)
    plt.legend(('Model length'),
               loc='upper center', shadow=True)
    plt.grid(False)
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title('Minimum Message Length')

    # match on arbitrary function
    def myfunc(x):
        return hasattr(x, 'set_color') and not hasattr(x, 'set_facecolor')

    for o in fig.findobj(myfunc):
        if type(o) == matplotlib.text.Text:
            o.set_color('black')
        else:
            o.set_color('blue')

    # match on class instances
    for o in fig.findobj(text.Text):
        o.set_fontstyle('italic')

    plt.show()
    plt.savefig()


def generate_hash(x, y_list, labels):
    tmp = str(y_list + x + labels)
    hash_object = hashlib.md5(tmp.encode('utf-8'))
    hash_string = hash_object.hexdigest()
    return hash_string


def plot_multiple_simple_graph(x, y_matrix, y_labels):
    # fig, ax = plt.subplots()

    file_name = generate_hash(x, y_list, y_labels)
    for y in y_matrix:
        print('y_label', y_label)
        plt.plot(x, y)
    plt.legend(y_labels, loc='upper center', shadow=True)
    plt.xlabel('Temperature (K)')
    # plt.show()
    plt.savefig(file_name)
    return file_name


class Plotter:
    def __init__(self):
        pass


if __name__ == '__main__':
    plotter = Plotter()
    x_array = [298.15, 300, 400, 500, 600, 700, 800, 900, 1000, 1200, 1500, 1700, 2000, 2500, 3000, 3500, 4000, 4500,
               5000]
    y_array = [40.12919928553181, 40.19786011080941, 44.122427745582854, 48.40745396254705, 52.98341513544712,
               57.799040782079246, 62.81429008207218, 67.99640193002644, 73.31813390564186, 84.29375431554108,
               101.35240203484366, 112.99644772707006, 130.73564669650025, 160.773742500908, 191.16940755451378,
               221.77926965357764, 252.52683172725426, 283.36787763540224, 314.27517912104935]
    z_array = [y + 10 for y in y_array]

    y_list = [y_array, z_array]
    x_label = 'Temperature (K)'
    y_label = 'Enthalpy (KJ/mol)'

    y_labels = ['Enthalpy (KJ/mol)', 'Heat capacity (J/mol/K)']
    # plotter.plot_simple_graph(x_array=x_array, y_array=y_array, x_label= x_label, y_label=y_label)
    # plotter.plot_simple_graph(x_array, y_array, x_label, y_label)
    plot_multiple_simple_graph(x_array, y_list, y_labels)
