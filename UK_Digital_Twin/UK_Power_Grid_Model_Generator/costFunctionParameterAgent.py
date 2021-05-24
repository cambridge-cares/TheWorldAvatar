##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 20 May 2021          #
##########################################

import os
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package.UKPowerGridModel import UKEGenModel_CostFunc
import SPARQLQueryUsedInModel as query_model

# CarbonTax = input('Carbon Tax (GBP/tCO2) is: ')
    # while int (CarbonTax) < 0:
    #     print('Carbon Tax rangs: >= 0, please enter again')
    #     CarbonTax = input('Carbon Tax (GBP/tCO2) is: ')
    # TypeOfCostFunc = input('TypeOfCostFunc is (1 for piecewise, 2 for polynomial): ')

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

def costFuncPara(uk_egen_costFunc, egen):    
    if isinstance (uk_egen_costFunc, UKEGenModel_CostFunc):
        pass
    else:
        print('The first argument should be an instence of UKEGenModel_CostFunc')
        return None
    
    # Query the location of the digital twin    
    location = (query_model.queryDigitalTwinLocation(dt.SleepycatStoragePath))[0][0]
    
    if str(location) == uk_egen_costFunc.location:
        if uk_egen_costFunc.version == 2019:
            uk_egen_costFunc.a = 0
            # Parameter b is related with carbon tax which should be the trigger of the parallel world
            uk_egen_costFunc.b = egen[3] + egen[4] + egen[5] * uk_egen_costFunc.CarbonTax 
            uk_egen_costFunc.c = egen[2] * egen[7]            
    return uk_egen_costFunc      