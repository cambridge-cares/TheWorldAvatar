import sys
import json
import numpy as np
import pandas as pd
from scipy import interpolate
from scipy.interpolate import LinearNDInterpolator

# define a function that can interpolate the emission and cost surface, and calculate the corresponding powerplant emission

def cost_emission_calculation (m, n):

    if m.shape[0] == 0:
        # if there is empty dataframe input, set all values to zero then
        pass

    else:
        # read age and capacity information from ultrasubcritical PC database
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

        # maximum x_1 and y_1 are 30 and 2500 respectively
        x_new_1 = np.clip(x_1, 1, 30)
        y_new_1 = np.clip(y_1, 100, 2500)
        xnew1ynew1 = np.column_stack((x_new_1,y_new_1))

        # cost estimation by age and capacity, clip at 30 and 2500
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
    plantInfoDict = json.loads(sys.argv[1])
    workingDir = str(sys.argv[2]) + '/'
    generation_technology = plantInfoDict['generation_technology']
    primary_fuel = plantInfoDict['primary_fuel']
    plant_df = pd.DataFrame(plantInfoDict, index=[0])
    plant_df.drop(['emission_rate'], axis=1, inplace=True)
      
    if generation_technology == 'ultrasupercritical':
        emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_ultrasupercritical_PC_coal.csv', header='infer', sep=',')
          
    if generation_technology == 'supercritical':
        if primary_fuel == 'anthracite':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_supercritical_PC_anthracite.csv', header='infer', sep=',')
        elif primary_fuel == 'subbituminous':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_supercritical_PC_subbituminous.csv', header='infer', sep=',')
        elif primary_fuel == 'lignite':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_supercritical_PC_lignite.csv', header='infer', sep=',')
        elif primary_fuel == 'bituminous' or primary_fuel == 'coal':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_supercritical_PC_subbituminous.csv', header='infer', sep=',')
      
    if generation_technology == 'subcritical' or generation_technology == 'cogeneration':
        if primary_fuel == 'anthracite':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_subcritical_PC_anthracite.csv', header='infer', sep=',')
        elif primary_fuel == 'subbituminous':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_subcritical_PC_subbituminous.csv', header='infer', sep=',')
        elif primary_fuel == 'lignite':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_subcritical_PC_lignite.csv', header='infer', sep=',')
        elif primary_fuel == 'coal_biomass':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_subcritical_PC_coal_biomass.csv', header='infer', sep=',')     
        elif primary_fuel == 'bituminous' or primary_fuel == 'coal':
            emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_subcritical_PC_subbituminous.csv', header='infer', sep=',')
      
    if primary_fuel == 'natural_gas':
        emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_NGCC.csv', header='infer', sep=',')
        emission_df = emission_df.sort_values(by=['age','capacity_MW'], ascending=[True,True])
          
    if primary_fuel == 'oil':
        emission_df = pd.read_csv(workingDir + 'data/input/baseplant/base_NGCC_oil.csv', header='infer', sep=',')
        emission_df = emission_df.sort_values(by=['age','capacity_MW'], ascending=[True,True])
          
    result_dict = cost_emission_calculation(plant_df, emission_df).iloc[0].to_dict()
    print(result_dict['annual_emission_ton'])