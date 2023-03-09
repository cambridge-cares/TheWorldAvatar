import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import interpolate
from scipy.interpolate import LinearNDInterpolator
import json
import sys

from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger
from costEmissionCalculation import cost_emission_calculation

# Preconditions:
# 1. external libraries must be installed in system
# 2. "data" folder must be placed in workingdir of JPS_POWERPLANTEMISSION
# 3. costEmissionCalculation.py file placed in the same folder i.e. caresjpspowerplantemission

# Postconditions:
# Takes in data from csv files and constructs a surrogate model for each of the csv files
# which represents a specific technology


# define a function here that can interpolate the emission and cost surface, and calculate the corresponding power plant emission
# m is the power plant database, n is the emission inventory

if __name__ == "__main__":
    pythonLogger = PythonLogger('PlantEmissionCalculationTest.py')
    pythonLogger.postInfoToLogServer('start of PlantEmissionCalculationTest.py')
    
    try:
        dirPath = sys.argv[1]
        
        # load the powerplant database    
        df = pd.read_csv(dirPath + '/data/input/powerplant_database.csv', header='infer', sep=',')
             
        # choose ultrasupercritical PC from the database
        df_01 = df[df.generation_technology == 'ultrasupercritical']
        # load the emission inventory
        df_1 = pd.read_csv(dirPath +  '/data/input/baseplant/base_ultrasupercritical_PC_coal.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_m = df[df.generation_technology == 'supercritical']
        df_02 = df_m[df_m.primary_fuel == 'anthracite']
        # load the emission table
        df_2 = pd.read_csv(dirPath + '/data/input/baseplant/base_supercritical_PC_anthracite.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_03 = df_m[(df_m.primary_fuel == 'bituminous') | (df_m.primary_fuel == 'coal')]
        # load the emission table
        df_3 = pd.read_csv(dirPath + '/data/input/baseplant/base_supercritical_PC_bituminous.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_04 = df_m[df_m.primary_fuel == 'subbituminous']
        # load the emission table
        df_4 = pd.read_csv(dirPath + '/data/input/baseplant/base_supercritical_PC_subbituminous.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_05 = df_m[df_m.primary_fuel == 'lignite']
        # load the emission table
        df_5 = pd.read_csv(dirPath + '/data/input/baseplant/base_supercritical_PC_lignite.csv', header='infer', sep=',')
         
        # choose anthracite and subcritical PC from the database
        df_n = df[df.generation_technology == 'subcritical']
        df_06 = df_n[df_n.primary_fuel == 'anthracite']
        # load the emission table
        df_6 = pd.read_csv(dirPath + '/data/input/baseplant/base_subcritical_PC_anthracite.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_coal = df[df.fuel_used == 'coal']
        df_07 = df_coal[(df_coal.primary_fuel == 'bituminous') | (df_coal.primary_fuel == 'coal') | (df_coal.generation_technology == 'cogeneration')]
        # load the emission table
        df_7 = pd.read_csv(dirPath + '/data/input/baseplant/base_subcritical_PC_bituminous.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_08 = df_n[df_n.primary_fuel == 'subbituminous']
        # load the emission table
        df_8 = pd.read_csv(dirPath + '/data/input/baseplant/base_subcritical_PC_subbituminous.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_09 = df_n[df_n.primary_fuel == 'lignite']
        # load the emission table
        df_9 = pd.read_csv(dirPath + '/data/input/baseplant/base_subcritical_PC_lignite.csv', header='infer', sep=',')
         
        # choose anthracite and supercritical PC from the database
        df_010 = df_n[df_n.primary_fuel == 'coal_biomass']
        # load the emission table
        df_10 = pd.read_csv(dirPath + '/data/input/baseplant/base_subcritical_PC_coal_biomass.csv', header='infer', sep=',')
         
        # choose natural gas plant from the database
        df_011 = df[df.primary_fuel == 'natural_gas']
        # load the emission table
        df_11 = pd.read_csv(dirPath + '/data/input/baseplant/base_NGCC.csv', header='infer', sep=',')
        df_11 = df_11.sort_values(by=['age', 'capacity_MW'], ascending=[True, True])
         
        # choose oil plant from the database
        df_012 = df[df.primary_fuel == 'oil']
        # load the emission table
        df_12 = pd.read_csv(dirPath + '/data/input/baseplant/base_NGCC_oil.csv', header='infer', sep=',')
        df_12 = df_12.sort_values(by=['age', 'capacity_MW'], ascending=[True, True])
         
        # loop over different generation types df01-df012 to calculate the 2015 emission
        plant_list = [df_01, df_02, df_03, df_04, df_05, df_06, df_07, df_08, df_09, df_010, df_011, df_012]
        emission_list = [df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12]
           
        # return final list of all technologies' emissions
        list_total = []
         
        for i in range(len(plant_list)):
            list_technology = []
         
            column_headers = cost_emission_calculation(plant_list[i], emission_list[i]).columns.values.tolist()
            entries = cost_emission_calculation(plant_list[i], emission_list[i]).values.tolist()
         
            list_technology.append(column_headers)
            list_technology.extend(entries)
            list_total.append(list_technology)
         
        print(json.dumps(list_total))
    except Exception as e:
        returnExceptionToJava(e)
        pythonLogger.postInfoToLogServer('end of PlantEmissionCalculationTest.py')