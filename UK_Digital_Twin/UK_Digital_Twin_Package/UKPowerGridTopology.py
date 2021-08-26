##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module declare the properties of generating UK power grid topology A-boxes"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLable

class UKPowerGridTopology:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid_Topology\\UK_Power_Grid_Topology_KG\\"
    
    """Default path of SleepycatStoragePath"""
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLable.UKGridTopologylKG
    
    """Bus Node keys"""
    EquipmentConnection_EBusKey = "EquipmentConnection_EBus-"
    PowerFlow_ELineKey = "PowerFlow_ELine-"
    PowerGeneration_EGenKey = "PowerGeneration_EGen-"
    
    CoordinateSystemKey = "CoordinateSystem_"
    
    LongitudeKey = "x_coordinate_" 
    LantitudeKey = "y_coordinate_" 
    
    valueKey = "value_"
    NumberOfKey = "Number_of_"

