##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 May 2021          #
##########################################

"""This module declare the properties of generating UK power grid topology A-boxes"""

class UKPowerGridTopology:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid_Topology\\"
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec"
    
    """Bus Node keys"""
    EquipmentConnection_EBusKey = "EquipmentConnection_EBus-"
    PowerFlow_ELineKey = "PowerFlow_ELine-"
    
    CoordinateSystemKey = "CoordinateSystem_"
    
    LongitudeKey = "y_coordinate_" 
    LantitudeKey = "x_coordinate_" 
    
    valueKey = "value_"
    NumberOfKey = "Number_of_"

