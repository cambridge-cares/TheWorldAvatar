##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 June 2022         #
##########################################

import os
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package.UKPowerGridModel import UKEGenModel_CostFunc

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

def costFuncPara(uk_egen_costFunc, egen):    
    if not isinstance (uk_egen_costFunc, UKEGenModel_CostFunc):      
        raise Exception('The first argument should be an instence of UKEGenModel_CostFunc')
    if uk_egen_costFunc.MODEL == 2 and uk_egen_costFunc.NCOST == 2:
            # uk_egen_costFunc.a = 0
            # Parameter b is related with carbon tax which should be the trigger of the parallel world
            # egen[1]: fixed O&M; egen[2]: var O&M; egen[3]: Fuel cost; egen[4]: emission factor; egen[6]: capacity
            uk_egen_costFunc.a = round((egen[2] + egen[3] + float(egen[4]) * uk_egen_costFunc.CarbonTax), 3) #  first order, c1 
            uk_egen_costFunc.b = round((egen[1] * egen[6]), 3)   # zero order, c0   
            uk_egen_costFunc.COST.append(uk_egen_costFunc.a) 
            uk_egen_costFunc.COST.append(uk_egen_costFunc.b) 
            uk_egen_costFunc.CO2EmissionFactor = float(egen[4])   
    return uk_egen_costFunc        
