##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 15 Sept 2021         #
##########################################

"""This class defines the properties of UK Energy Consumption data"""

class EnergyConsumptionData:
    
    def __init__(self, version = 2017):
        SECTION = [ "COAL", "MANUFACTURED_FUELS", "PETROLEUM_PRODUCTS", "GAS", "ELECTRICITY", "BIOENERGY_AND_WASTE"]
        self.VERSION = version
        """ File path """
        self.DataPath = '../Data files/EnergyConsumption/'
        self.RegionandAreas = self.DataPath + str(self.VERSION) + '/' + SECTION[4] + '/official_regions.csv'
        self.ElectricityConsumptionData = self.DataPath + str(self.VERSION) + '/' + SECTION[4] + '/electricityConsumptionData.csv'
        
        """Data file header"""
        self.headerElectricityConsumption = ["RegionAndArea", "LACode", "IndustrialAndCommercial", "Domestic", "Total\n"]
        
        """Official regions"""
        self.GovernmentOfficeRegions = ["Wales", "Scotland", "North_East_England", "North_West_England", "Yorkshire_and_the_Humber", "East_Midlands",\
                               "West_Midlands_(county)", "East_of_England", "London", "South_East_England", "South_West_England"]
       
        """Start Time"""
        self.startTime_NHH = str(self.VERSION) + '-01-31' # Non-Half Hourly
        self.startTime_HH = str(self.VERSION)+ '-01-01' # Half Hourly
        
        """Source Data"""
        self.__ENCONSUMPT_2017 = self.DataPath + str(self.VERSION) + "Sub-national-total-final-energy-consumption-statistics_2005-2017.xlsx"
        self.__ENCONSUMPT_2019 = self.DataPath + str(self.VERSION) + "Sub_national_electricity_consumption_statistics_2005-2019.xlsx"