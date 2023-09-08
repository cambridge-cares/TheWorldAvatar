##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 Sept 2023         #
##########################################

"""This class defines the properties of PopulationDensity data"""
from pathlib import Path

class PopulationDensityDataProperty:
    
    def __init__(self, version, pointDistance):
        self.VERSION = version
    
        """ File path """
        self.DataPath = str(Path(__file__).resolve().parent.parent) + "/Data files/PopulationDensity/"
        self.PopulationDensityDataPath = self.DataPath + str(self.VERSION) + '/reduced_file_grid_size_%skm.csv'%str(pointDistance)

        """Header"""
        self.headerPopulationDensityData = ["Lat", "Lon", "Population\n"]
