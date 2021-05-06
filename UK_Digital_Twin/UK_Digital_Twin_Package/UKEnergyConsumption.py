##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 April 2021        #
##########################################

"""This module declare the properties of generating UK Energy Consumption A-boxes"""

class UKEnergyConsumption:
    
    """Default path of storing owl file """
    StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\"
    
    """Node keys"""
    TotalConsumptionKey = "Total_Electricity_Consumption_of_"
    DomesticConsumptionKey = "Domestic_Electricity_Consumption_of_"
    IndustrialAndCommercialConsumptionKey = "Industrial_and_Commercial_Electricity_Consumption_of_"
    
    valueKey = "value_"