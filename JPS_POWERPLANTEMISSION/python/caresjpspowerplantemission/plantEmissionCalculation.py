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

# define a function here that can interpolate the emission and cost surface, and calculate the corresponding power plant emission
# m is the power plant database, n is the emission inventory

def cost_emission_calculation (m, n): 
     
  if m.shape[0] == 0:
    # if there is empty dataframe input, set all values to zero then
    pass
    
  else:  
    # read age and capacity information from ultrasupercritical PC database
    x_1 = m.loc[:,('age')].values
    y_1 = m.loc[:,('capacity_MW')].values
    y_11 = m.loc[:,('capacity_MW')].values
    
    # use cubic spline interpolation to construct surrogate model
    x1 = n.loc[:,('age')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[:,0]
    y1 = n.loc[:,('capacity_MW')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[0,:]
    x1x1, y1y1 = np.meshgrid(x1, y1, indexing = 'ij')
    x1y1=np.column_stack([x1x1.ravel(),y1y1.ravel()])
    # x1y1.shape
    y11= n.loc[:,('capacity_MW')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[0,:]
    # y11.shape
    z1 = n.loc[:,('marginal_cost_MWh')].values
    # z1.shape
    z11 = n.loc[:,('annual_emission_ton')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[0,:]
    # z11.shape
    
    # f1 is the functional relationship between electricity cost and age, capacity
    f1 = LinearNDInterpolator(x1y1, z1)
    # f1(30,1865)
    
    # f11 is the functional relationship between annual emission and capacity
    f11 = interpolate.interp1d(y11, z11, kind='slinear')
    # f11(1865)
    # vectorized function
    f_11 = np.vectorize(f11)

    # maximum x_1 and y_1 are 30 and 2500 repsectively
    x_new_1 = np.clip(x_1, 1, 30)
    y_new_1 = np.clip(y_1, 100, 2500)
    xnew1ynew1 = np.column_stack((x_new_1,y_new_1))
    
    # cost estimiation by age and capacity, clip at 30 and 2500
    cost = f1(xnew1ynew1)
    
    # emission estimation by capacity
    emission = f_11(np.clip(y_11%2500, 100, 2500)) + f_11(2500)*(y_11//2500) 
    
    m = m.assign(cost = cost)
    m = m.assign(emission = emission)
    
    m.rename(columns={'cost': 'electricity_cost_MWh', 'emission': 'annual_emission_ton'}, inplace=True)
    k = m.loc[:,:]
    
    text_file = open("log_file.txt", "a")
    text_file.write("###################### \n")
    text_file.write("generation technology: %s, primary_fuel: %s \n" %(n.generation_technology.unique(),n.primary_fuel.unique()))
    text_file.write("cost range: max %s, min %s \n" %( np.max(cost), np.min(cost)))
    text_file.write("emission range: max %s, min %s \n" %( np.max(emission), np.min(emission)))
    text_file.write("###################### \n")
    text_file.write('\n')
    text_file.close()

    return k


if __name__ == "__main__":
    pythonLogger = PythonLogger('PlantEmissionCalculationTest.py')
    pythonLogger.postInfoToLogServer('start of PlantEmissionCalculationTest.py')
    
    try:
        dirPath = sys.argv[1]
#         print(json.dumps(dirPath + '/data/input/powerplant_database.csv'))
        
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