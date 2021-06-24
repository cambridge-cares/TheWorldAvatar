##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 June 2021         #
##########################################

import os
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package.UKPowerGridModel import UKEGenModel_CostFunc

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

def costFuncPara(uk_egen_costFunc, egen, located_country, localQuery):    
    if isinstance (uk_egen_costFunc, UKEGenModel_CostFunc):
        pass
    else:
        print('The first argument should be an instence of UKEGenModel_CostFunc')
        return None
    
    if str(located_country) == uk_egen_costFunc.location:
        if uk_egen_costFunc.version == 2019:
            uk_egen_costFunc.a = 0
            # Parameter b is related with carbon tax which should be the trigger of the parallel world
            uk_egen_costFunc.b = egen[3] + egen[4] + egen[5] * uk_egen_costFunc.CarbonTax 
            uk_egen_costFunc.c = egen[2] * egen[7]            
    return uk_egen_costFunc      
