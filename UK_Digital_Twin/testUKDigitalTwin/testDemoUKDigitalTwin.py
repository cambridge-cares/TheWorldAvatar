##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 10 June 2021         #
##########################################

"""The first time generating UK power plant and energy consumption KG"""

import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Power_Plant_Generator import powerPlantABoxGeneration as ppGenerator
from UK_Energy_Consumption_Generator import energyConsumptionABoxGeneration as enconGenerator

ppGenerator.addUKPowerPlantTriples('default', 2019, False)
enconGenerator.addRegionalAndLocalNodes('default', 2017, False)


print('terminated')