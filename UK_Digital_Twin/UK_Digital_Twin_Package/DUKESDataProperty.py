##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 20 April 2021        #
##########################################

"""This class defines the properties of DUKES data"""

class DUKESData:
    
    """Data Version"""
    VERSION = 2019
    
    """ File path """
    DataPath = '../Data files/DUKES/'
    PlantName = DataPath + str(VERSION) + '/plantname.csv'
    PlantType = DataPath + str(VERSION) + '/planttype.csv'
    EnergyGen = DataPath + str(VERSION) + '/energyGen.csv'
    GenTech = DataPath + str(VERSION) + '/genTech.csv'
    Owner = DataPath + str(VERSION) + '/owner.csv'
    PrimaryFuel = DataPath + str(VERSION) + '/primaryFuel.csv'
    DesignCapacity = DataPath + str(VERSION) + '/designcapacity.csv'
    BuiltYear =  DataPath + str(VERSION) + '/builtyear.csv'
    GPSLocation = DataPath + str(VERSION) + '/gpslocation.csv'
    Region = DataPath + str(VERSION) + '/regionaladdress.csv'
    
    """Source Data"""
    __DUKES = DataPath + str(VERSION) + "DUKES2019.xls"
    
   