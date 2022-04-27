##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 12 April 2022        #
##########################################

"""This class defines the properties of Model Factors"""
from pathlib import Path

class ModelFactor:
    
    """ File path """
    # DataPath = "C:/Users/wx243/Documents/TheWorldAvatar/UK_Digital_Twin/Data files/CO2FactorAndCostFactor/"
    
    DataPath = str(Path(__file__).resolve().parent.parent) + "\Data files\CO2FactorAndCostFactor\\"
    CO2EmissionFactorAndCostFactor = DataPath + 'CO2EmissionFactorAndCostFactor.csv' 
    
    """Source Data"""
    __ModelFactor = DataPath + "-Node-24h-Tax-Auto - Template.xlsx"
   
    Renewable = ['Solar', 'Hydro', 'PumpHydro', 'Wind']
    Bio = ['Waste_anaerobicdigestion', 'Waste_municipalsolidwaste', 'Waste']
    CCGT = ['CombinedCycleGasTurbine']
    OCGT = ['OpenCycleGasTurbine']
    
    FixMaintenanceCostsKey = "FixMaintenanceCosts_"
    OperationalExpenditureCostsKey = "OperationalExpenditureCosts_"
    FuelCostsKey = "FuelCosts_"
    CO2EmissionFactorkey = "CO2EmissionFactor_"
    
    
    """Data file header"""
    headerModelFactor = ["FuelType", "FixedOperationandMaintenanceCost", "VariableOperationandMaintenanceCost", "FuelCost", "CO2EmissionFactor\n"]