'''
Extracts the inital data file (all level 1) and the descriptions texts from the Excel model for the webapp.

'''
from utils.plot_formatter import translateMetas,readControlMetas
from utils.config import INITIALDATAJSONPATH,LEVERDESCRIPTIONPATH
if __name__ == '__main__':
    plotdata = translateMetas()
    with open(INITIALDATAJSONPATH, "w") as file1:
        file1.write(plotdata)
    descriptions = readControlMetas()
    with open(LEVERDESCRIPTIONPATH, "w") as file2:
        file2.write(descriptions)