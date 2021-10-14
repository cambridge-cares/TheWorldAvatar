##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 14 Oct 2021          #
##########################################

"""This module declare the properties of generating UK power grid topology A-boxes"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

class UKPowerGridTopology:
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLabel.UKGridTopologylKG
    
    """Bus Node keys"""
    EquipmentConnection_EBusKey = "EquipmentConnection_EBus-"
    PowerFlow_ELineKey = "PowerFlow_ELine-"
    PowerGeneration_EGenKey = "PowerGeneration_EGen-"
    
    CoordinateSystemKey = "CoordinateSystem_"
    
    LongitudeKey = "x_coordinate_" 
    LantitudeKey = "y_coordinate_" 
    
    valueKey = "value_"
    NumberOfKey = "Number_of_"

    def __init__(self, numOfBus = 10, Location = 'http://dbpedia.org/resource/United_Kingdom'):
            self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid_Topology\\UK_Power_Grid_Topology_" + str(numOfBus) + "_Bus_KG\\"
            self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
            self.numOfBus = numOfBus
            self.location = Location