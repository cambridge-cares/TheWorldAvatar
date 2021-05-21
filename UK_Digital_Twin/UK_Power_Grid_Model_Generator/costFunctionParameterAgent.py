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

def costFuncPara(uk_egen_costFunc, defaultPath_Sleepycat, egen, powerPlant_Sleepycat):    
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
            modelFactors = list(query_model.queryCostFactors(defaultPath_Sleepycat, egen[0]))
            if len(modelFactors) == 0:
                print ('Cannot query the cost function parameter, please check the iri of the EGen node')
                return None
            uk_egen_costFunc.b = float(modelFactors[0][1]) + float(modelFactors[0][2]) + float(modelFactors[0][3]) * uk_egen_costFunc.CarbonTax
            capacity = query_model.queryCapacity(defaultPath_Sleepycat, powerPlant_Sleepycat, egen[0])
            uk_egen_costFunc.c = float(modelFactors[0][0]) * capacity            
    return uk_egen_costFunc      
            
# if __name__ == '__main__': 
#     uk_egen_costFunc = UKEGenModel_CostFunc(version = 2019)
#     defaultPath_Sleepycat = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
#     powerPlant_Sleepycat = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"
#     egen = 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen-451.owl#EGen-451'
    
#     res = costFuncPara(uk_egen_costFunc, defaultPath_Sleepycat, egen, powerPlant_Sleepycat)
    
#     print(res.a, res.b, res.c)
    