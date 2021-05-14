# ## PART 2 - read the power plant database, select different type power plant, and reference plant emission for both ccs and baseline

import pandas as pd
import sys
import json
# import time

from screen_ccs import ccs_screen
from emission_ccs import emission_aggregation_ccs
from post_process_ccs import bau_ccs_post, ccs_results

# rootPath = 'C:/Users/WE/WebstormProjects/JPS_EMISSIONS/'
rootPath = json.loads(sys.argv[1])

# load the powerplant database

# df = pd.read_csv(rootPath + 'data/input/powerplant_database.csv', header='infer', sep=',')
# df = pd.read_csv(rootPath + 'data/output/result.csv', header='infer', sep=',')

from world_powerplants_sparql import WorldPowerPlantsSPARQL
from powerplant_sparql_sync import PowerplantSPARQLSync


wPSPARQL = WorldPowerPlantsSPARQL()
powerplants = wPSPARQL.getPowerplants()

# start_time = time.time()

listDict = []
for powerplant in powerplants:
    pSPARQL = PowerplantSPARQLSync(powerplant)
    powerplantInfo = pSPARQL.getPowerplantInfo()
    listDict.append(powerplantInfo)

df = pd.DataFrame(listDict,
                    columns=['country', 'capacity_MW', 'primary_fuel',
                            'generation_technology', 'age', 'output_MWh', 'fuel_used'])

# print(df.dtypes)
# df.to_csv(rootPath + 'data/output/result.csv', index=False)
# print("{} seconds".format(time.time() - start_time))

# with pd.option_context('display.max_rows', None, 'display.max_columns', None):
#     print(df)

### 2.1 coal

### ultrasupercritical

# choose ultrasupercritical PC from the database
df_01 = df[df.generation_technology == 'ultrasupercritical']

# load the emission inventory table for baseline scenario
dfb_1 = pd.read_csv(rootPath + 'data/input/baseplant/base_ultrasupercritical_PC_coal.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_1 = pd.read_csv(rootPath + 'data/input/ccs/capture_ultrasupercritical_PC_coal.csv', header='infer', sep=',')

# add age column to the dataframe
df_tem = pd.read_csv(rootPath + 'data/input/ccs/ages.csv', header='infer', sep=',')
ages = df_tem.loc[:,('age')].values
df_1['age'] = ages


# ### supercritical anthracite

# choose anthracite and supercritical PC from the database
df_m = df[df.generation_technology == 'supercritical']
df_02 = df_m[df_m.primary_fuel == 'anthracite']

# load the emission inventory table for baseline scenario
dfb_2 = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_anthracite.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_2 = pd.read_csv(rootPath + 'data/input/ccs/capture_supercritical_PC_anthracite.csv', header='infer', sep=',')

# add age column to the dataframe
df_2['age'] = ages


# ### supercritical bituminous and coal

# choose anthracite and supercritical PC from the database
df_03 = df_m[(df_m.primary_fuel == 'bituminous') | (df_m.primary_fuel == 'coal')]

# load the emission inventory table for baseline scenario
dfb_3 = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_bituminous.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_3 = pd.read_csv(rootPath + 'data/input/ccs/capture_supercritical_PC_bituminous.csv', header='infer', sep=',')

# add age column to the dataframe
df_3['age'] = ages


# ### supercritical subbituminous
# choose anthracite and supercritical PC from the database
df_04 = df_m[df_m.primary_fuel == 'subbituminous']

# load the emission inventory table for baseline scenario
dfb_4 = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_subbituminous.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_4 = pd.read_csv(rootPath + 'data/input/ccs/capture_supercritical_PC_subbituminous.csv', header='infer', sep=',')

# add age column to the dataframe
df_4['age'] = ages


# ### supercritical lignite

# choose anthracite and supercritical PC from the database
df_05 = df_m[df_m.primary_fuel == 'lignite']

# load the emission table
dfb_5 = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_lignite.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_5 = pd.read_csv(rootPath + 'data/input/ccs/capture_supercritical_PC_lignite.csv', header='infer', sep=',')

# add age column to the dataframe
df_5['age'] = ages


# ### subcritical anthracite

# choose anthracite and subcritical PC from the database
df_n = df[df.generation_technology == 'subcritical']
df_06 = df_n[df_n.primary_fuel == 'anthracite']

# load the emission table
dfb_6 = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_anthracite.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_6 = pd.read_csv(rootPath + 'data/input/ccs/capture_subcritical_PC_anthracite.csv', header='infer', sep=',')

# add age column to the dataframe
df_6['age'] = ages


# ### subcritical bituminous and coal

# choose anthracite and supercritical PC from the database
df_coal = df[df.fuel_used == 'coal']
df_07 = df_coal[(df_coal.primary_fuel == 'bituminous') | (df_coal.primary_fuel == 'coal') | (df_coal.generation_technology == 'cogeneration')]

# load the emission table
dfb_7 = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_bituminous.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_7 = pd.read_csv(rootPath + 'data/input/ccs/capture_subcritical_PC_bituminous.csv', header='infer', sep=',')

# add age column to the dataframe
df_7['age'] = ages


# ### subcritical subbituminous

# choose anthracite and supercritical PC from the database
df_08 = df_n[df_n.primary_fuel == 'subbituminous']

# load the emission table
dfb_8 = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_subbituminous.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_8 = pd.read_csv(rootPath + 'data/input/ccs/capture_subcritical_PC_subbituminous.csv', header='infer', sep=',')

# add age column to the dataframe
df_8['age'] = ages


# ### subcritical lignite

# choose anthracite and supercritical PC from the database
df_09 = df_n[df_n.primary_fuel == 'lignite']

# load the emission table
dfb_9 = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_lignite.csv', header='infer', sep=',')


# load the emission inventory table for ccs scenario
df_9 = pd.read_csv(rootPath + 'data/input/ccs/capture_subcritical_PC_lignite.csv', header='infer', sep=',')

# add age column to the dataframe
df_9['age'] = ages


# ### subcritical coal_biomass

# choose anthracite and supercritical PC from the database
df_010 = df_n[df_n.primary_fuel == 'coal_biomass']

# load the emission table
dfb_10 = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_coal_biomass.csv', header='infer', sep=',')

# load the emission inventory table for ccs scenario
df_10 = pd.read_csv(rootPath + 'data/input/ccs/capture_subcritical_PC_coal_biomass.csv', header='infer', sep=',')

# add age column to the dataframe
df_10['age'] = ages


# ### 2.2 natural gas plant

# choose natural gas plant from the database
df_011 = df[df.primary_fuel == 'natural_gas']

# load the emission table
dfb_11 = pd.read_csv(rootPath + 'data/input/baseplant/base_NGCC.csv', header='infer', sep=',')
dfb_11 = dfb_11.sort_values(by=['age','capacity_MW'], ascending=[True,True])

# load the emission inventory table for ccs scenario
df_11 = pd.read_csv(rootPath + 'data/input/ccs/NGCC_capture.csv', header='infer', sep=',')
df_11 = df_11.sort_values(by=['age','capacity_MW'], ascending=[True,True])


# ### 2.3 oil

# choose oil plant from the database
df_012 = df[df.primary_fuel == 'oil']

# load the emission table
dfb_12 = pd.read_csv(rootPath + 'data/input/baseplant/base_NGCC_oil.csv', header='infer', sep=',')
dfb_12 = dfb_12.sort_values(by=['age','capacity_MW'], ascending=[True,True])


# load the emission inventory table for ccs scenario
df_12 = pd.read_csv(rootPath + 'data/input/ccs/NGCC_capture_oil.csv', header='infer', sep=',')
df_12 = df_12.sort_values(by=['age','capacity_MW'], ascending=[True,True])


# ## PART 3 - emission and economics analysis

# ###  3.1 get important parameter input from user interface (e.g.  co2 tax and technology learning rate)

# get carbonprice from user interface, choose from 0, 10, 20, 50, 100
# carbonprice = 0
carbonprice = int(json.loads(sys.argv[2]))

# get technology learning rate from user interface, choose from high, middle, low
# coalccslearningrate = 'high'
# gasccslearningrate = 'high'
coalccslearningrate = sys.argv[3] # not needed for string
gasccslearningrate = sys.argv[3]


# ### 3.2 assume the newly added capacity share same decomposition as plant fleet between age 0 and 5

# get the plant list of age between 0 and 5

df_01 = df_01[(df_01.age >0) & (df_01.age <= 5)]
df_02 = df_02[(df_02.age >0) & (df_02.age <= 5)]
df_03 = df_03[(df_03.age >0) & (df_03.age <= 5)]
df_04 = df_04[(df_04.age >0) & (df_04.age <= 5)]
df_05 = df_05[(df_05.age >0) & (df_05.age <= 5)]
df_06 = df_06[(df_06.age >0) & (df_06.age <= 5)]
df_07 = df_07[(df_07.age >0) & (df_07.age <= 5)]
df_08 = df_08[(df_08.age >0) & (df_08.age <= 5)]
df_09 = df_09[(df_09.age >0) & (df_09.age <= 5)]
df_010 = df_010[(df_010.age >0) & (df_010.age <= 5)]
df_011 = df_011[(df_011.age >0) & (df_011.age <= 5)]
df_012 = df_012[(df_012.age >0) & (df_012.age <= 5)]


plant_list = [df_01, df_02, df_03, df_04, df_05, df_06, df_07, df_08, df_09, df_010, df_011, df_012]
emission_list_b = [dfb_1, dfb_2, dfb_3, dfb_4, dfb_5, dfb_6, dfb_7, dfb_8, dfb_9, dfb_10, dfb_11, dfb_12]
emission_list_ccs = [df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12]


# ### 3.3 screen the newly added plant whether ccs is possible or not

# load the CCS learning rate for coal and gas

# ues the test learning rate at constant 0.8
# ccs_learning_coal = pd.read_csv(rootPath + 'data/input/ccs/ccs_technology_learning_test.csv', header='infer', sep=',')

ccs_learning_coal = pd.read_csv(rootPath + 'data/input/ccs/coal_ccs_technology_learning.csv', header='infer', sep=',')
ccs_learning_gas = pd.read_csv(rootPath + 'data/input/ccs/gas_ccs_technology_learning.csv', header='infer', sep=',')


# set the learning rate to high
ccs_learning_coal = ccs_learning_coal[ccs_learning_coal.learning_rate == coalccslearningrate]
ccs_learning_gas = ccs_learning_gas[ccs_learning_gas.learning_rate == gasccslearningrate]


# select year and plant LCOE decrease
ccs_learning_coal = ccs_learning_coal[['year','plant_LCOE_decrease']]
ccs_learning_coal = ccs_learning_coal.reset_index(drop=True)
ccs_learning_gas = ccs_learning_gas[['year','plant_LCOE_decrease']]
ccs_learning_gas = ccs_learning_gas.reset_index(drop=True)


# set the key parameters

# set the refernce cost for coal, gas, and oil plant without carbon price respectively
coal_reference_cost_base = 30
gas_reference_cost_base = 60
oil_reference_cost_base = 60

# set the carbon price
carbon_price = carbonprice

# set the capacity factor
capacity_factor = 0.75

# load the CCS table
ccs = pd.read_csv(rootPath + 'data/output/BAU_CCS/input_bau_ccs_learning_high.csv', header='infer', sep=',')

# set the refernce cost for coal, gas, and oil plant with carbon price respectively
coal_reference_cost = coal_reference_cost_base + (carbon_price * 1144) / (1500 * capacity_factor)
gas_reference_cost = gas_reference_cost_base + (carbon_price * 214) / (600 * capacity_factor)
oil_reference_cost = oil_reference_cost_base + (carbon_price * 214) / (600 * capacity_factor)

# calculate the vintage change of powerplant database during 35 years: from 2015 to 2050

import copy

w = {}   # emission inventory of existing plant type j at year i

k = {}   # ccs possible plant from newly retired plant type j at year i

plant = {} # newly built plant database for type j

plant_m = {} # exisitng plant database for type j
plant_n = {} # ccs plant database for type j

coal_emission_w = {} # overall coal emission inventory at year i
gas_emission_w = {} # overall gas emission inventory at year i
oil_emission_w = {} # overall oil emission inventory at year i

coal_emission_k = {} # overall coal emission inventory at year i
gas_emission_k = {} # overall gas emission inventory at year i
oil_emission_k = {} # overall oil emission inventory at year i

em_li_ccs = {} #emission list

# load the CCS table
ccs = pd.read_csv(rootPath + 'data/output/BAU_CCS/input_bau_ccs_learning_high.csv', header='infer', sep=',')

# open('bau_ccs_learn.txt', 'w').close()

for i in range(36):

    w[i] = {}
    k[i] = {}
    #emission_list_ccs[i] = emission_list_ccs
    em_li_ccs[i] = {}
    M = copy.deepcopy(emission_list_ccs)

    for j in range(12):

        # get the newly built plant database
        plant[j] = plant_list[j]

        # set the reference cost to screen ccs possible plants based on fuel type
        if j >= 0 and j <= 9:
            reference_cost = coal_reference_cost
            learning_rate = ccs_learning_coal.at[i,'plant_LCOE_decrease']
        elif j == 10:
            reference_cost = gas_reference_cost
            learning_rate = ccs_learning_gas.at[i,'plant_LCOE_decrease']
        else:
            reference_cost = oil_reference_cost
            learning_rate = ccs_learning_gas.at[i,'plant_LCOE_decrease']

        em_li_ccs[i][j] = M[j]

        # set the carbon price and capacity factor
        em_li_ccs[i][j]['carbon_price_ton'] = carbon_price
        em_li_ccs[i][j]['capacity_factor'] = capacity_factor

        # set the capture plant LCOE at year i according to learning rate
        em_li_ccs[i][j]['capture_plant_LCOE_MWh'] = em_li_ccs[i][j]['capture_plant_LCOE_MWh'] * (1-learning_rate)

        em_li_ccs[i][j]['carbon_LCOE_MWh'] = (emission_list_ccs[j]['emission_rate_ton_h'] *                                                   em_li_ccs[i][j]['carbon_price_ton']) /                                                    (emission_list_ccs[j]['capacity_MW'] *em_li_ccs[i][j]['capacity_factor'])
        em_li_ccs[i][j]['capture_carbon_LCOE_MWh'] = (emission_list_ccs[j]['capture_emission_rate_ton_h'] *                                                           em_li_ccs[i][j]['carbon_price_ton']) /                                                  (emission_list_ccs[j]['capacity_MW'] *em_li_ccs[i][j]['capacity_factor'])

        em_li_ccs[i][j]['LCOE_MWh'] =em_li_ccs[i][j]['carbon_LCOE_MWh'] +em_li_ccs[i][j]['plant_LCOE_MWh']
        em_li_ccs[i][j]['capture_LCOE_MWh'] =em_li_ccs[i][j]['capture_carbon_LCOE_MWh'] +                                                     em_li_ccs[i][j]['capture_plant_LCOE_MWh']

        # screen ccs possible plant from newly built plant
        w[i][j] =  ccs_screen (plant[j], em_li_ccs[i][j], reference_cost, i)[0]
        k[i][j] =  ccs_screen (plant[j], em_li_ccs[i][j], reference_cost, i)[1]

        # print("plant df: %s" %(j+1))

        # text_file = open("bau_ccs_learn.txt", "a")
        # text_file.write("plant type: %s, " %(j+1))
        # text_file.write("newly built plant number: %s, capacity: %s " %(plant[j].shape[0], plant[j]['capacity_MW'].sum()))
        # text_file.write("newly built ccs possible plant number: %s, capacity: %s " %(w[i][j].shape[0], w[i][j]['capacity_MW'].sum()))
        # text_file.write('\n')

    # aggregated ccs possible new coal power plant
    coal_emission_w[i+2015] = pd.concat([w[i][0],w[i][1],w[i][2],w[i][3],w[i][4],w[i][5],w[i][6],w[i][7],w[i][8],w[i][9]],
                                         ignore_index=True, sort=False)
    coal_emission_w[i+2015]['fuel_used'] = 'coal'
    # aggregated ccs not possible new coal power plant
    coal_emission_k[i+2015] = pd.concat([k[i][0],k[i][1],k[i][2],k[i][3],k[i][4],k[i][5],k[i][6],k[i][7],k[i][8],k[i][9]],
                                         ignore_index=True, sort=False)
    coal_emission_k[i+2015]['fuel_used'] = 'coal'

    # aggregated ccs possible new gas power plant
    gas_emission_w[i+2015] = w[i][10]
    gas_emission_w[i+2015]['fuel_used'] = 'gas'
    # aggregated ccs not possible new gas power plant
    gas_emission_k[i+2015] = k[i][10]
    gas_emission_k[i+2015]['fuel_used'] = 'gas'


    # aggregated ccs possible new oil power plant
    oil_emission_w[i+2015] = w[i][11]
    oil_emission_w[i+2015]['fuel_used'] = 'oil'
    # aggregated ccs not possible new gas power plant
    oil_emission_k[i+2015] = k[i][11]
    oil_emission_k[i+2015]['fuel_used'] = 'oil'

    # aggregate the emission of year i for different plant types
    ccs = emission_aggregation_ccs(coal_emission_w[i+2015], coal_emission_k[i+2015], ccs, i)
    ccs = emission_aggregation_ccs(gas_emission_w[i+2015], gas_emission_k[i+2015], ccs, i)
    ccs = emission_aggregation_ccs(oil_emission_w[i+2015], oil_emission_k[i+2015], ccs, i)

    # print (i+2015)

    # text_file.write("year: %s" %(i+2015))
    # text_file.write("###################### \n")

# post process the ccs table to calculate overall emission and select useful columns

ccs = bau_ccs_post(ccs)
ccs = ccs_results(ccs)
ccs.to_csv(rootPath + 'data/output/BAU_CCS/results_bau_ccs_learning_high.csv', index=False)


# ## 4 visualize the results

import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

x = ccs.loc[:,('year')].values
y = ccs.loc[:,['coal_power_annual_emission_existing','gas_power_annual_emission_existing','oil_power_annual_emission_existing']].values
y = np.transpose(y)

sns.set_style("white")
sns.set_context("paper",font_scale=1)

plt.clf()

f, ax = plt.subplots(1, 1, figsize=(3,2.5))


# create your palette using html codes
pal = ["#F44027", "#CAF91E", "#2B8EF1", "#CAF91E"]
plt.stackplot(x,y, labels=['Coal','Natural Gas','Oil'], colors=pal)

box = ax.get_position()
ax.set_position([box.x0, box.y0, box.width, box.height*0.85])
ax.legend(loc='center left', bbox_to_anchor=(-0.03,1.05), ncol=3)
ax.set(xlabel='Year', ylabel='CO2 annual emission (Gt/year)')


ax.set_xlim([2015, 2050])
xticks = [2015,2020,2030,2040,2050]
yticks = np.arange(0, 22, 2.5)
ax.set_xticks(xticks)
ax.set_yticks(yticks)

plt.savefig(rootPath + 'public/images/annual_baseline.png', bbox_inches='tight', dpi=500)

print(json.dumps("COMPLETE"))