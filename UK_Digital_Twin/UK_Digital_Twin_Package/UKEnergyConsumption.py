##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 Jan 2022          #
##########################################

"""This module declare the properties of generating UK Energy Consumption A-boxes"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

class UKEnergyConsumption:
    
    """Default path of SleepycatStoragePath"""
    SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    
    """Default remote endpoint"""
    endpoint = EndPointConfigAndBlazegraphRepoLabel.UKEnergyConsumptionKG
    
    """Node keys"""
    TotalConsumptionKey = "TotalElectricityConsumption_"
    DomesticConsumptionKey = "DomesticElectricityConsumption_"
    IndustrialAndCommercialConsumptionKey = "Non-DomesticElectricityConsumption_"
    TimePeriodKey = "TimePeriod_"
    StartTimeKey = "CoordinateValue_"
    
    valueKey = "ScalarValue_"
    
    def __init__(self, VERSION = 2017):
        self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\UK_Energy_Consumption_KG_" + str(VERSION) + "\\"       
        self.DataVersion = VERSION
        