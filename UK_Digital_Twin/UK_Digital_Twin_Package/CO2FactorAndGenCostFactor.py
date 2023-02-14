##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 June 2022         #
##########################################

"""This class defines the properties of Model Factors"""
from pathlib import Path

class ModelFactor:
    
    """ File path """
    # DataPath = "C:/Users/wx243/Documents/TheWorldAvatar/UK_Digital_Twin/Data files/CO2FactorAndCostFactor/"
    
    DataPath = str(Path(__file__).resolve().parent.parent) + "/Data files/CO2FactorAndCostFactor/"
    CO2EmissionFactorAndCostFactor = DataPath + 'CO2EmissionFactorAndCostFactor.csv' 
    
    """Source Data"""
    __ModelFactor = DataPath + "-Node-24h-Tax-Auto - Template.xlsx"
   
    # Renewable = ['Solar', 'Hydro', 'PumpHydro', 'Wind']

    SMR = ['SMR', 'http://www.theworldavatar.com/kb/ontoeip/SmallModularReactor']
    Nuclear = ['Nuclear']
    Coal = ['Coal']
    Solar = ['Solar']
    Hydro = ['Hydro']
    PumpHydro = ['PumpHydro']
    Wind = ['Wind']
    WindOnshore = ['http://www.theworldavatar.com/kb/ontoeip/WindOnshore']
    WindOffshore = ['http://www.theworldavatar.com/kb/ontoeip/WindOffshore']
    Waste = ['Waste_anaerobicdigestion', 'Waste_municipalsolidwaste', 'Waste']
    Bio = ['Biomass']
    CCGT = ['http://www.theworldavatar.com/kb/ontoeip/CombinedCycleGasTurbine']
    OCGT = ['http://www.theworldavatar.com/kb/ontoeip/OpenCycleGasTurbine']

    Renewable = ['Solar','Hydro', 'PumpHydro', 'Wind']
    Natural = ['NaturalGas']
    Oil = ['Oil']
    
    FixMaintenanceCostsKey = "FixMaintenanceCosts_"
    OperationalExpenditureCostsKey = "OperationalExpenditureCosts_"
    FuelCostsKey = "FuelCosts_"
    CO2EmissionFactorkey = "CO2EmissionFactor_"
    
    
    """Data file header"""
    headerModelFactor = ["FuelType", "FixedOperationandMaintenanceCost", "VariableOperationandMaintenanceCost", "FuelCost", "CO2EmissionFactor\n"]