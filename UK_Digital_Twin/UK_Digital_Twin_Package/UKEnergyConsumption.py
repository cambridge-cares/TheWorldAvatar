##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 Jan 2022          #
##########################################

"""This module declare the properties of generating UK Energy Consumption A-boxes"""

from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel

class UKEnergyConsumption:
    """Node keys"""
    TotalConsumptionKey = "TotalElectricityConsumption_"
    DomesticConsumptionKey = "DomesticElectricityConsumption_"
    IndustrialAndCommercialConsumptionKey = "Non-DomesticElectricityConsumption_"
    TimePeriodKey = "TimePeriod_"
    StartTimeKey = "CoordinateValue_"
    AdministrativeDivisionKey = "AdministrativeDivision_"
    
    valueKey = "ScalarValue_"
    
    def __init__(self, VERSION = 2017):
        self.DataVersion = VERSION
        