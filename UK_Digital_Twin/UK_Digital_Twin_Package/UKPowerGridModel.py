##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 08 May 2021          #
##########################################

"""This module declare the properties of generating UK power grid model A-boxes"""

valueKey = "value_"

class UKEbusModel:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\EBus"
    
    """EBus Node keys"""
    EBusKey = "EBus-"
    ModelEBusKey = "Model_EBus-"

class UKElineModel:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\ELine"
    
    """ELine Node keys"""
    ELineKey = "ELine-"
    ModelELineKey = "Model_ELine-"
    ShapeKey = "Shape_"
    LengthKey = "Length_"
    OHL400kVKey = "OHL_400kV_" # Over head line (OHL) with 400kV level
    OHL275kVKey = "OHL_275kV_" # Over head line (OHL) with 275kV level

class UKEGenModel:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\EGen"
    
    """EGen Node keys"""
    EGenKey = "EGen-"
    ModelEGenKey = "Model_EGen-"
    