##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 06 Sept 2023         #
##########################################

"""This class defines the properties of DUKES data"""
from pathlib import Path

class DUKESData:
    
    def __init__(self, version = 2019):
        self.VERSION = version
    
        """ File path """
        self.DataPath = str(Path(__file__).resolve().parent.parent) + "/Data files/DUKES/"
        self.PlantName = self.DataPath + str(self.VERSION) + '/plantname.csv'
        self.PlantType = self.DataPath + str(self.VERSION) + '/planttype.csv'
        self.EnergyGen = self.DataPath + str(self.VERSION) + '/energyGen.csv'
        self.GenTech = self.DataPath + str(self.VERSION) + '/genTech.csv'
        self.Owner = self.DataPath + str(self.VERSION) + '/owner.csv'
        self.PrimaryFuel = self.DataPath + str(self.VERSION) + '/primaryFuel.csv'
        self.DesignCapacity = self.DataPath + str(self.VERSION) + '/designcapacity.csv'
        self.BuiltYear =  self.DataPath + str(self.VERSION) + '/builtyear.csv'
        self.GPSLocation = self.DataPath + str(self.VERSION) + '/gpslocation.csv'
        self.Region = self.DataPath + str(self.VERSION) + '/regionaladdress.csv'
        
        """Source Data"""
        self.__DUKES = self.DataPath + str(self.VERSION) + "/DUKES2019.xls"

if __name__ == '__main__':  
    test = DUKESData(2019)       
    print(test.PlantName)
       