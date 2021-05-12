##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 11 May 2021          #
##########################################

"""This class defines the properties of Model Factors"""

class ModelFactor:
    
    """ File path """
    DataPath = '../Data files/CO2FactorAndCostFactor/'
    CO2EmissionFactor = DataPath + 'CO2EmissionFactorAndCostFactor.csv' 
    
    """Source Data"""
    __ModelFactor = DataPath + "-Node-24h-Tax-Auto - Template.xlsx"
   
    Renewable = ['Solar', 'Hydro', 'PumpHydro', 'Wind']
    Bio = ['Waste_anaerobicdigestion', 'Waste_municipalsolidwaste', 'Waste']
    CCGT = ['CombinedCycleGasTurbine']
    OCGT = ['OpenCycleGasTurbine']
    
    """Data file header"""
    headerModelFactor = ["FuelType", "FixedOperationandMaintenanceCost", "VariableOperationandMaintenanceCost", "FuelCost", "CO2EmissionFactor\n"]