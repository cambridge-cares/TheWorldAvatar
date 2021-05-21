##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 April 2021        #
##########################################

"""This class defines the properties of UK Energy Consumption data"""

class EnergyConsumptionData:
    
    """Data Version"""
    VERSION = 2017
    SECTION = [ "COAL", "MANUFACTURED_FUELS", "PETROLEUM_PRODUCTS", "GAS", "ELECTRICITY", "BIOENERGY_AND_WASTE"]
    
    """ File path """
    DataPath = '../Data files/EnergyConsumption/'
    RegionandAreas = DataPath + str(VERSION) + '/' + SECTION[4] + '/official_regions.csv'
    ElectricityConsumptionData = DataPath + str(VERSION) + '/' + SECTION[4] + '/electricityConsumptionData.csv'
    
    """Data file header"""
    headerElectricityConsumption = ["RegionAndArea", "LACode", "IndustrialAndCommercial", "Domestic", "Total\n"]
    
    """Official regions"""
    GovernmentOfficeRegions = ["Wales", "Scotland", "North_East_England", "North_West_England", "Yorkshire_and_the_Humber", "East_Midlands",\
                               "West_Midlands_(county)", "East_of_England", "London", "South_East_England", "South_West_England"]
    
    """Source Data"""
    __ENCONSUMPT = DataPath + str(VERSION) + "Sub-national-total-final-energy-consumption-statistics_2005-2017.xlsx"