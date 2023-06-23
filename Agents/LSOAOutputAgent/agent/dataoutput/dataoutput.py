################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data output to generate figures
# in various format
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier

import agentlogging
from agent.errorhandling.exceptions import *
from agent.dataretrieval.dataretrival import *
from agent.datacalculation.datacalculation import *
from agent.datamodel.iris import OM_DEGREE_C, ONS_ID
from agent.datamodel.functionality import *
# T_from_COP ,call_pickle, data_treatment, \
#                     normalization, create_color_bar,save_figures,convert_to_tensor, \
#                     select_column, get_median, compare_tensor, tensor_to_df, convert_df

import matplotlib.pyplot as plt
import geopandas as gpd
from mpl_toolkits.axes_grid1 import make_axes_locatable
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from mpl_toolkits.axes_grid1.inset_locator import mark_inset
import matplotlib.patches
import seaborn as sb 
import pandas as pd
import numpy as np
import copy
import scipy.stats as st
import matplotlib.colors as cl 
import csv
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from tqdm import tqdm


# Initialise logger
logger = agentlogging.get_logger("prod")

# ------------------------- Calculation ------------------------------------ #
def monthly_disaggregation(df_in: pd.DataFrame, monthly_ref: list, annual: bool = False):
    '''
    To calculate the monthly distribution, based on the whole year data from df, and reference monthly distribution
    from monthly_ref
    Note: In many cases, monthly disaggregation can be done before or after a variable is calculated, 
    such as cost, emission, you can calculate a annual one and disaggregate into monthly data
    that won't affect the result
    Arguments:
    df: two-column data frame which MUST have the data to disaggregate placed at the second column
    (i.e. at position [1])
    monthly_ref: reference monthly distribution.
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    global months
    df = copy.deepcopy(df_in)
    months = ["2020-01-01T12:00:00.000Z",
              "2020-02-01T12:00:00.000Z",
              "2020-03-01T12:00:00.000Z",
              "2020-04-01T12:00:00.000Z",
              "2020-05-01T12:00:00.000Z",
              "2020-06-01T12:00:00.000Z",
              "2020-07-01T12:00:00.000Z",
              "2020-08-01T12:00:00.000Z",
              "2020-09-01T12:00:00.000Z",
              "2020-10-01T12:00:00.000Z",
              "2020-11-01T12:00:00.000Z",
              "2020-12-01T12:00:00.000Z"]
    total = sum(monthly_ref)
    for i in range(12):
        df[f'{months[i]}'] = df[df.columns[1]] * monthly_ref[i] / total
    if annual == False:
        df = drop_column(df,[1])
    return df

def fuel_cost(df_elec_in:pd.DataFrame, df_gas_in:pd.DataFrame, price_elec: float, price_gas:float, monthly_ref_elec:list, monthly_ref_gas:list, annual: bool = False):
    '''
    To calculate the fuel cost per LSOA, normally on per household basis
    Returns three dataframe which have first column as LSOA code, and the following 12
    columns for each month of df_cost_total, df_cost_elec, df_cost_gas
    Arguments:
    df_elec: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    df_gas: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    price_elec: price of electricity
    price_gas: price of gas
    monthly_ref_elec: monthly consumption of electricity consumption, to be used in monthly_disaggregation 
    monthly_ref_gas: monthly consumption of gas consumption, to be used in monthly_disaggregation 
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    df_elec = copy.deepcopy(df_elec_in)
    df_gas = copy.deepcopy(df_gas_in)
    # Replace the first column from consumption into cost
    df_elec[df_elec.columns[1]] *= price_elec
    df_elec = df_elec.rename(columns={df_elec.columns[1]: 'Annual cost'}, inplace=False)
    df_cost_elec = df_elec.copy()
    
    df_gas[df_gas.columns[1]] *= price_gas
    df_gas = df_gas.rename(columns={df_gas.columns[1]: 'Annual cost'}, inplace=False)
    df_cost_gas = df_gas.copy()

    # Disaggrate into monthly dataframe
    df_cost_elec = monthly_disaggregation(df_cost_elec, monthly_ref_elec,annual)
    df_cost_gas = monthly_disaggregation(df_cost_gas,monthly_ref_gas,annual)
    
    # Merge to total cost
    df_cost_total = df_cost_elec.merge(df_cost_gas, left_on=df_cost_elec.columns[0], right_on=df_cost_gas.columns[0],how='inner')
    # Iterate through the columns of the merged dataframe and add the values
    for col in df_cost_gas.columns[1:]:
        df_cost_total[col] = df_cost_total[col + '_x'] + df_cost_total[col + '_y']

    # Drop the original columns from the merged dataframe
    df_cost_total.drop(columns=[col + '_x' for col in df_cost_elec.columns[1:]], inplace=True)
    df_cost_total.drop(columns=[col + '_y' for col in df_cost_gas.columns[1:]], inplace=True)
    
    return df_cost_total, df_cost_elec, df_cost_gas

def fuel_emission(df_elec_in:pd.DataFrame, df_gas_in:pd.DataFrame, carbon_intensity_elec: float, carbon_intensity_gas:float, monthly_ref_elec:list, monthly_ref_gas:list, annual: bool = False):
    '''
    To calculate the fuel emission per LSOA, normally on per household basis
    Returns three dataframe which have first column as LSOA code, and the following 12
    columns for each month of df_emission_total, df_emission_elec, df_emission_gas
    Arguments:
    df_elec: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    df_gas: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    carbon_intensity_elec: carbon intensity of electricity
    carbon_intensity_gas: carbon intensity of gas
    monthly_ref_elec: monthly consumption of electricity consumption, to be used in monthly_disaggregation 
    monthly_ref_gas: monthly consumption of gas consumption, to be used in monthly_disaggregation 
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    df_elec = copy.deepcopy(df_elec_in)
    df_gas = copy.deepcopy(df_gas_in)
    df_elec[df_elec.columns[1]] *= carbon_intensity_elec
    df_elec = df_elec.rename(columns={df_elec.columns[1]: 'Annual emission'}, inplace=False)
    df_emission_elec = df_elec.copy()
    
    df_gas[df_gas.columns[1]] *= carbon_intensity_gas
    df_gas = df_gas.rename(columns={df_gas.columns[1]: 'Annual emission'}, inplace=False)
    df_emission_gas = df_gas.copy()

    # Disaggrate into monthly dataframe
    df_emission_elec = monthly_disaggregation(df_emission_elec, monthly_ref_elec,annual)
    df_emission_gas = monthly_disaggregation(df_emission_gas,monthly_ref_gas,annual)
    
    # Merge to total cost
    df_emission_total = df_emission_elec.merge(df_emission_gas, left_on=df_emission_elec.columns[0], right_on=df_emission_gas.columns[0],how='inner')
    # Iterate through the columns of the merged dataframe and add the values
    for col in df_emission_gas.columns[1:]:
        df_emission_total[col] = df_emission_total[col + '_x'] + df_emission_total[col + '_y']

    # Drop the original columns from the merged dataframe
    df_emission_total.drop(columns=[col + '_x' for col in df_emission_elec.columns[1:]], inplace=True)
    df_emission_total.drop(columns=[col + '_y' for col in df_emission_gas.columns[1:]], inplace=True)
    
    return df_emission_total, df_emission_elec, df_emission_gas

def COP(temp, hp_efficiency:float = 0.35, T_H: float = 45 +273.15):
    '''
    Based on a given temperature to calculate the COP
    Note: COP = hp_efficiency * T_H / (T_H - T_C), where the input temperature is represented as T_C
    T_H, hp_efficiency are hypothesd as constant, which have default value as 318.15 and 0.35
    respectfully. I suggest to check if that default value is up to date or if that hypothesis is 
    valid in your case
    '''
    COP = hp_efficiency * T_H / (T_H -273.15 - temp)
    
    COP = np.round(COP,3)
    
    return COP

def T_from_COP(COP):
    '''
    Return temperature based on a given COP
    '''
    T = 45 - ((45+273.15)/(COP/0.35))

    return T

def delta_gas(uptake: float, total_gas_consumption, propotion_heating: float = 0.9):
    '''
    Based on a given uptake, and gas consumption, calculate how many gas will be converted to electricity, which is delta_gas
    Note: the definition of uptake is (delta_gas / gas_for_heating) = (delta_gas / (Total_gas_consumption * propotion_heating))
        Therefore, delta_gas = uptake * Total_gas_consumption * propotion_heating
        propotion_heating is hypothesd as constant, which have default value as 0.9 I suggest to check if that default value is 
        up to date or if that hypothesis is valid in your case
    '''
    delta_gas = uptake * total_gas_consumption * propotion_heating

    return delta_gas

def delta_elec(delta_gas, COP, boiler_efficiency: float = 0.8):
    '''
    Based on given COP, delta_gas to calculate how much electricity has been converted based on delta_gas
    Note: delta_elec = boiler_efficiency * delta_gas / COP. where boiler_efficiency is hypothesd as constant, 
    which have default value as 0.8. I suggest to check if that default value 
    is up to date or if that hypothesis is valid in your case
    '''
    delta_elec = boiler_efficiency * delta_gas / COP

    return delta_elec

def calculate_inequality_index(poverty_values, change_values, min_fp, max_fp, min_dc, max_dc):
    
    
    a = ((poverty_values-min_fp)/(max_fp-min_fp))

    b = ((2*(change_values-min_dc))/(max_dc-min_dc))-1
    
    inequality_index = a*b
    
    return np.around(inequality_index,decimals=3)

def calculate_inequality_df(df_change_of_cost_in: pd.DataFrame, df_fuel_poverty_in: pd.DataFrame, min_deltaC_nth_percentile:float = 1, \
                                 max_deltaC_nth_percentile:float = 99, min_fuel_poverty:float = 0, max_fuel_poverty:float = 0.2 ):
    '''
    '''
    # make copy of df_in
    df_change_of_cost = copy.deepcopy(df_change_of_cost_in)
    df_fuel_poverty = copy.deepcopy(df_fuel_poverty_in)
    
    # Merge the data into one df
    df_fuel_poverty.rename(columns={df_fuel_poverty.columns[0]: 'LSOA_code'}, inplace=True)
    df_change_of_cost.rename(columns={df_change_of_cost.columns[0]: 'LSOA_code'}, inplace=True)
    df_all = df_change_of_cost.merge(df_fuel_poverty, left_on=df_change_of_cost.columns[0], right_on=df_fuel_poverty.columns[0], how='inner')
    
    change_values = df_all[df_all.columns[1]].values
    min_fp = min_fuel_poverty * 100
    max_fp = max_fuel_poverty * 100
    min_dc = np.nanpercentile(change_values,min_deltaC_nth_percentile)
    max_dc = np.nanpercentile(change_values,max_deltaC_nth_percentile)
    df_all['inequality_index'] = df_all.apply(calculate_inequality_index, axis=1, args=(min_fp, max_fp, min_dc, max_dc))
    df_all.drop(columns=[df_all.columns[1], df_all.columns[2]], inplace=True)

    return df_all

def calculate_change_of_cost(year, ratio, uptake,df_cop, elec_increase = 0, gas_increase = 0 ):
    df_elec = pd.read_csv(f'./Data/properties_csv/{year}/Elec_Consump_per_house.csv')
    df_gas = pd.read_csv(f'./Data/properties_csv/{year}/Gas_Consump_per_house.csv')
    # df_cost = pd.read_csv(f"./Data/properties_csv/{year}/Total_Cost.csv")
    # df_cost = df_cost[['LSOA_code','Annual cost']]
    if year in ['2020','2021','2022']:
        df_fp = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
        df_fp = df_fp[df_fp['Proportion of households fuel poor (%)'] > 0.001]
        df_fp_ref = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
    else:
        df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_fp_ref = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")

    df_elec, df_gas, df_cop, df_fp, _ = sort_muiltiple_df(df_elec, df_gas, df_cop, df_fp, df_fp_ref)

    cop = df_cop.iloc[:, 1:13].values
    elec_consump = df_elec.iloc[:, 1:13].values
    gas_consump = df_gas.iloc[:, 1:13].values
    fp = df_fp.iloc[:, 1].values
    fp = fp*100

    delta_gas_array = delta_gas(uptake, gas_consump)
    delta_elect_array = delta_elec(delta_gas_array, cop) 

    # resulted_elec_consump = elec_consump + delta_elect_array
    # resulted_gas_consump = gas_consump - delta_gas_array

    #cost_elec = read_from_web_price_elec(year=year)
    cost_gas =  read_from_web_price_gas(year=year) * ratio
    #cost_gas =  cost_elec / ratio
    cost_elec =  cost_gas * 4

    # final_cost = cost_elec * resulted_elec_consump + cost_gas * resulted_gas_consump
    # final_cost = np.sum(final_cost, axis=1)

    # original_cost = df_cost.iloc[:, 1:13].values
    # original_cost = original_cost.reshape(final_cost.shape)

    delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
    delta_cost = np.sum(delta_cost, axis=1)
    
    delta_cost_copy = delta_cost.copy()
    delta_cost_copy = delta_cost_copy[~np.isnan(delta_cost_copy)]
    delta_cost_medium = np.median(delta_cost_copy)

    return delta_cost, delta_cost_medium

# ------------------------ GeoSpatial Plot --------------------------------- #
def plot_geodistribution(label: str, title:str, df_in: pd.DataFrame, cb_scale: float = 0):
    '''
    This module is aim to plot the input variable as geospatial scale (and do NOT have specifiy city view)
    Note: As the LSOA code is the unique identifier, this module accept the DataFrame which have one 
        column of LSOA code, and another column for the variable to plot

    Arguments:
    label: the legend label for colorbar
    title: title of the figure (be stored as file name as well)
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
    df: df should be two column pd.DataFrame which have one column of LSOA code, and another column for the variable to plot
                in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    def basic_settings(df_geo):
        # Set the plot
        color_theme = 'coolwarm'
        fig = plt.figure(figsize=(3.5,4.5))
        plt.subplots_adjust(left=0.032,right=0.799)
        plt.tight_layout()
        axs = plt.axes()

        # Get the UK shape
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        UK_gdf.boundary.plot(ax=axs,color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        
        # y-axis limits of the axis are set to the minimum and maximum latitude values
        axs.set_ylim([boundary[1],boundary[3]])
        # Specify the color bar
        divider = make_axes_locatable(axs)
        cax1    = divider.append_axes("right", size="5%", pad=0.05)

        return axs, cax1, color_theme

    # Get Geospatial shapes:
    df_geo = call_pickle('./Data/pickles/df_geo in function retrieve_ONS_shape_from_KG')

    print(f'Beginning plot for geodistribution of {title}...')
    # Revising the data
    df_geo, val_values =data_treatment(df_geo, title, df_in)

    # Initilize the graph
    axs, cax1, color_theme = basic_settings(df_geo)


    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    divnorm = normalization(val_values, cb_scale)

    tl = df_geo.plot(column=f"{title}",
                cmap=color_theme,
                antialiased=False,
                ax=axs,
                legend=True,
                norm=divnorm,
                cax=cax1)

    # Create a colorbar for the plot
    create_color_bar(color_theme, divnorm, label, axs, cax1, df_geo[f"{title}"])

    axs.set_xticks([])
    axs.set_yticks([])
    axs.spines["top"].set_visible(False)
    axs.spines["right"].set_visible(False)
    axs.spines["left"].set_visible(False)
    axs.spines["bottom"].set_visible(False)
    axs.set_title(f"{title}",y=1.08)
    cax1.ticklabel_format(axis="y", style="sci", scilimits=(0,0))  
    
    # Store the figures
    save_figures(title)

def plot_geodistribution_with_cities(label: str, title:str, df_in: pd.DataFrame, cb_scale: float = 0, pdf = True):
    '''
    This module is aim to plot the input variable as geospatial scale (and DO have specifiy city view)
    Note: As the LSOA code is the unique identifier, this module accept the DataFrame which have one 
        column of LSOA code, and another column for the variable to plot

    Arguments:
    label: the legend label for colorbar
    title: title of the figure (be stored as file name as well)
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
    df: df should be two column pd.DataFrame which have one column of LSOA code, and another column for the variable to plot
                in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    def basic_settings(df_geo):
        # Set the plot
        color_theme = 'coolwarm'
        mosaic = '''
        A
        A
        '''
        fig = plt.figure(figsize=(11,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        UK_gdf.boundary.plot(ax=axs['A'],color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        axs['A'].set_ylim([boundary[1]-5E4,boundary[3]+20E4])
        axs['A'].set_xlim(([boundary[0]-5E4,boundary[2]+1E4]))
        #plt.subplots_adjust(left=0)
        cax = fig.add_axes([0.9, 0.1, 0.02, 0.8])

        return axs, cax, color_theme, UK_gdf

    # Get Geospatial shapes:
    df_geo = call_pickle('./Data/pickles/df_geo in function retrieve_ONS_shape_from_KG')

    # Make a copy of df_in
    df = copy.deepcopy(df_in)
    
    # Revising the data
    df_geo, val_values =data_treatment(df_geo, title, df)
    
    # Initilize the graph
    axs, cax, color_theme, UK_gdf = basic_settings(df_geo)
    
    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    divnorm = normalization(val_values, cb_scale)

    axs_xbounds = [np.array([-2.815E5,-2E5]),np.array([-2.838E5,-1.05E5]),np.array([-3.35E4,9.4E3]),np.array([-6.5E5,-1.957E5])]
    axs_ybounds = [np.array([7.007E6,7.0652E6]),np.array([7.206E6,7.41E6]),np.array([6.656E6,6.6969E6]),np.array([6.39E6,6.78E6])]
    
    tl = df_geo.plot(column=f"{title}",
                cmap=color_theme,
                antialiased=False,
                ax=axs['A'],
                legend=True,
                norm=divnorm,
                cax=cax,
                legend_kwds={'label':'Inequality Index (-)','ticks':[-1,-0.5,0,0.5,1]})
    
    cax.ticklabel_format(axis="y", style="sci", scilimits=(0,0))   
    cax.set_yticklabels(['< -1','-0.5','0','0.5','> 1'])
    # Create a colorbar for the plot
    create_color_bar(color_theme, divnorm, label, axs, cax, df_geo[f"{title}"])

    axs['A'].set_xticks([])
    axs['A'].set_yticks([])
    axs['A'].spines["top"].set_visible(False)
    axs['A'].spines["right"].set_visible(False)
    axs['A'].spines["left"].set_visible(False)
    axs['A'].spines["bottom"].set_visible(False)

    order = [4,3,2,1]
    loc1 = [1,2,2,1]
    loc2 = [3,3,3,4]
    names = ['Greater Manchester','North East','London','South West']

    for f in range(4):
        if f == 0 or f == 1:
            axins2 = inset_axes(axs['A'], width=4, height=2.5,
                    bbox_to_anchor=(0.5, 0.3),
                    bbox_transform=axs['A'].transAxes, loc=order[f], borderpad=6)
        else:
            axins2 = inset_axes(axs['A'], width=4, height=2.5,
                    bbox_to_anchor=(0.5, 0.5),
                    bbox_transform=axs['A'].transAxes, loc=order[f], borderpad=6)
        plt.subplots_adjust(bottom = 0.225,left=0.07)
        plt.setp(axins2.get_xticklabels(), visible=False)
        plt.setp(axins2.get_yticklabels(), visible=False)
        UK_gdf.boundary.plot(ax=axins2,color='k',linewidth=0.5)
        axins2.set_xticks([])
        axins2.set_title(str(names[f]))
        axins2.set_yticks([])
        axins2.set_ylim(axs_ybounds[f])
        axins2.set_xlim(axs_xbounds[f])
        df_geo.plot(column=f"{title}",cmap=color_theme,\
                antialiased=False,\
                ax = axins2,\
                norm = divnorm)
        mark_inset(axs['A'],axins2,loc1=loc1[f],loc2=loc2[f],fc='none',ec='0')
    
    # Store the figures
    save_figures(title, pdf)

def plot_multiple_geodistribution(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0):
    '''
    This module is aim to produce multiple geospatial scale figures in one plot
    Note: As the LSOA code is the unique identifier, this module accept the DataFrame which have one 
        column of LSOA code, and other columns for the variable to plot

    Arguments:
    label: the legend label for colorbar
    title: title of the figure (be stored as file name as well)
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
                in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    df: df should be  pd.DataFrame which have one column of LSOA code, and another column for the variable to plot
    The df should looks like this 
                LSOA_code  var1  var2  var3 ....
            0
            1
            2
            ...
    Note that except the first column (which is LSOA code using for identifier) How many columns are in the df
    how many figures will be generated in the plot. The column name (var1, var2, var3 in this example) will be used
    as the label for this figure

    '''
    # Get Geospatial shapes:
    #df_geo = retrieve_ONS_shape_from_KG()
    ################### TBD
    df_geo = call_pickle('./Data/pickles/df_geo in function retrieve_ONS_shape_from_KG')
    df = copy.deepcopy(df_in)
    ###########################
    print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
    
    # Revising the data
    df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)

    # Count how many figures need to be plotted based on df
    num_plot = df.shape[1] - 1

    # Initialize the plot
    mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
    color_theme = 'coolwarm'
    fig = plt.figure(figsize=(11,5))
    axs = fig.subplot_mosaic(mosaic)    
    UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
    UK_gdf = UK_gdf.to_crs("EPSG:3395")
    plot_names = mosaic 
    
    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    divnorm = normalization(val_values, cb_scale)
    
    for it in range(num_plot):  # iterate over the number of subplots
        axs[plot_names[it]].set_title(df.columns[it + 1], loc='left')
        UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
        axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
        plt.subplots_adjust(left=0.075,right=0.836)
        
        # for the last plot adding a colorbar
        if plot_names[it] == mosaic[-1]:
            cax = fig.add_axes([0.9, 0.1, 0.02, 0.8])
            tl = df_geo.plot(column=df_geo.iloc[:, -1],
                    cmap=color_theme,
                    antialiased=False,
                    ax=axs[plot_names[it]],
                    legend=True,
                    norm=divnorm,
                    cax=cax)
            # Create a colorbar for the plot
            create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
            cax.ticklabel_format(axis="y", style="sci", scilimits=(0,0))  
        else:
            tl  = df_geo.plot(df_geo.iloc[:, it + 2],\
                cmap=color_theme,\
                antialiased=False,\
                ax = axs[plot_names[it]],\
                norm = divnorm)

        axs[plot_names[it]].set_xticks([])
        axs[plot_names[it]].set_yticks([])
        axs[plot_names[it]].spines["top"].set_visible(False)
        axs[plot_names[it]].spines["right"].set_visible(False)
        axs[plot_names[it]].spines["left"].set_visible(False)
        axs[plot_names[it]].spines["bottom"].set_visible(False)
    
    save_figures(title)

def plot_temperature_versus_var(label: str, title:str, df_dict: pd.DataFrame, month: int, df_temproal: pd.DataFrame = None, cb_scale: float = 0):
    '''
This function aims to produce geospatial distribution for a month for max, mean and min temperature with reference for a var that is 
directly relating to temperature, for example, COP = f(temperature)

If the second df_temproal is provided, the function will also produce two line plots refering to specific LSOA's monthly distribution of var, 
for t_max, mean and min senarios

    Arguments:
    label: the legend label for colorbar
    title: title of the figure (be stored as file name as well)
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
                in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    df_dict: df_dict should be pd.DataFrame which have one column of LSOA code, and another column for the variable to plot
            The df should looks like this 
                        LSOA_code  temp  var
                    0
                    1
                    2
                    ...
            df_dict represent the value with respect to one month only, therefore:
            the value in temp and var columns should be dict using {tas: , tmax: ,tmin: } 
            or {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':, 
                'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':,
                'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':}
            Note that the name of column 'var' will be used as label of corresponding colorbar and line figures
            and sequence DO matter, first column is LSOA code, second is temp, third is var
    month: a int to represent which month df_fict is refering to, 0 is Jan, 1 is Feb, etc etc...
    df_temproal: should contain two different row labels only, with 12 columns refering to monthly data for var
            for the same row label, the row data will be plotted at the same figure
            df_temproal: should looks like:
                        Jan  Feb  Mar  Apr ....   Dec
                LSOA_1
                LSOA_1
                LSOA_1
                LSOA_2
                LSOA_2
                LSOA_2
            Note that the row label will be used as label of the line plot. so for how many same row labels were in this df, 
            how many line will be plotted. noted that the column names don't matter as it default the sequence as Jan to Dec.

            If in some case you may want fill_between feature to represent a range, please provide several rows with same 
            [row_label], for those rows have same [row_label], the function will automatically fill between the row have
            max average value and the row have min average value, while for rows in the middle will still be plotted.
            i.e., sequence don't matter as long as they have the same [row_label]
    '''
    # Get Geospatial shapes:
    #df_geo = retrieve_ONS_shape_from_KG()
    ################### TBD
    df_geo = call_pickle('./Data/pickles/df_geo in function retrieve_ONS_shape_from_KG')
    ###########################
    print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
    
    # Revising the data
    df = copy.deepcopy(df_dict)
    df_t = copy.deepcopy(df_temproal)
    extended_df = pd.DataFrame()
    # Extend the original dataframe
    extended_df['LSOA_code'] = df['LSOA_code']
    for i in ['tasmin','tas','tasmax']:
        extended_df[f'temp_{i}'] = df.apply(lambda x: x.iloc[1][i] if i in x.iloc[1] else np.nan, axis=1)
        extended_df[f'var_{i}'] = df.apply(lambda x: x.iloc[2][i] if i in x.iloc[2] else np.nan, axis=1)
    
    df_geo, val_value = data_treatment(df_geo, arg_name=False, arg_value_in = extended_df)
    divnorm = normalization(val_value, cb_scale)
    
    # Initialize the plot
    color_theme = 'coolwarm'
    mosaic = """
    EEEAAAA
    EEEAAAA
    EEEAAAA
    GGGAAAA
    FFFBBCC
    FFFBBCC
    FFFBBCC
    """
    fig = plt.figure(figsize=(7.5,7))
    axs = fig.subplot_mosaic(mosaic)    
    #plt.subplots_adjust(left=0)
    cax = fig.add_axes([0.75, 0.1, 0.03, 0.8])
    cax2 = fig.add_axes([0.87, 0.1, 0.03, 0.8])
    plt.subplots_adjust(right=0.736,left=0.098)
    keys = ['B','A','C']

    # Plotting the geospatial distribution
    for i in range(3):
        tl  = df_geo.plot(df_geo.iloc[:, 2* i + 3],\
                cmap=color_theme,\
                antialiased=False,\
                ax = axs[keys[i]],\
                norm = divnorm)
        if i ==0:
            # color bar for var
            create_color_bar(color_theme,divnorm,label,axs['A'],cax,val_value)
            # color bar for temperature
            create_color_bar(color_theme,divnorm,'Air Temperature (°C)',axs['A'],cax2,val_value)
        
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        UK_gdf.boundary.plot(ax=axs[keys[i]],color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        axs[keys[i]].set_ylim([boundary[1],boundary[3]])
        axs[keys[i]].set_xticks([])
        axs[keys[i]].set_yticks([])
        axs[keys[i]].spines["top"].set_visible(False)
        axs[keys[i]].spines["right"].set_visible(False)
        axs[keys[i]].spines["left"].set_visible(False)
        axs[keys[i]].spines["bottom"].set_visible(False)
        axs[keys[i]].set_title(['Minimum','Mean','Maximum'][i])
    ticks = cax2.get_yticks()
    temp = np.round(np.array(T_from_COP(ticks)),1).astype(str)
    cax2.set_yticklabels(temp)
    axs['G'].set_xticks([])
    axs['G'].set_yticks([])
    axs['G'].spines["top"].set_visible(False)
    axs['G'].spines["right"].set_visible(False)
    axs['G'].spines["left"].set_visible(False)
    axs['G'].spines["bottom"].set_visible(False)
    
    # Plotting the line figure
    months_letter = ['J','F','M','A','M','J','J','A','S','O','N','D']
    axs['E'].set_xlabel('Month')
    axs['E'].set_ylabel(df_dict.columns[2]+' (-)')
    axs['F'].set_xlabel('Month')
    axs['F'].set_ylabel(df_dict.columns[2]+' (-)')

    groups = df_t.groupby(df_t.index)
    i =0 
    month_avg = 0
    for index_name, group in groups:
        
        group_max = group.max().max()
        if len(group) == 1:
            # Plot the line for the single-data row
            axs[['E','F'][i]].plot(x=group.columns, y=group.values[0], label=index_name, color='black', linewidth=2, linestyle='solid')
            axs[['E','F'][i]].scatter([month],[group.iloc[month]],c='r')
            axs[['E','F'][i]].text(0.5,1.25,f'Avg {df_dict.columns[2]}: {str(np.round(group.mean(),2))}')
            axs[['E','F'][i]].set_title('LSOA: '+ index_name.split('/')[-1])
            axs[['E','F'][i]].set_ylim(1,group_max)
            month_avg = group.iloc[month]
            i+=1

        else:
            row_means = group.mean(axis=1)

            # Reset the index of the group
            group = group.reset_index(drop=True)
            group.index = row_means
            
            # Sort the group by the index
            group = group.sort_index()
            
            # Get the minimum and maximum rows
            y1 = group.iloc[0]
            y2 = group.iloc[-1]
            # Plot the fill between the minimum and maximum rows
            axs[['F','E'][i]].fill_between(x=df_t.columns, y1=y1, y2=y2, color='black',linestyle='solid',alpha=0.1)
            axs[['F','E'][i]].scatter([month, month],[y1.iloc[month],y2.iloc[month]],c='r')
            axs[['F','E'][i]].vlines(month,y1.iloc[month],y2.iloc[month],color='r',alpha=0.4)
            axs[['F','E'][i]].set_title('LSOA: '+ index_name.split('/')[-1])
            axs[['F','E'][i]].set_ylim(1,group_max)
            # Plot the remaining rows
            for _, row in group.iloc[1:-1].iterrows():
                axs[['E','F'][i]].plot(df_t.columns, row, label=index_name, color='black', linewidth=2, linestyle='solid')
                axs[['E','F'][i]].scatter([month],[row.iloc[month]],c='r')
                axs[['E','F'][i]].text(0.5,1.25,f'Avg {df_dict.columns[2]}: {str(np.round(row.mean(),2))}')
                month_avg = row.iloc[month]
            i +=1
    
    plt.sca(axs['E'])
    plt.xticks(range(len(months_letter)), [], color='k')
    plt.sca(axs['F'])
    plt.xticks(range(len(months_letter)), months_letter, color='k')
    figtr = fig.transFigure.inverted() # Display -> Figure
    ax0tr = axs['E'].transData # Axis 0 -> Display
    ax1tr = axs['A'].transData
    ptB = figtr.transform(ax0tr.transform((month,month_avg)))
    ptE = figtr.transform(ax1tr.transform((-1.45E5,7.021E6)))
    arrow = matplotlib.patches.FancyArrowPatch(ptB,ptE,transform=fig.transFigure,\
        connectionstyle="arc3,rad=0.2",arrowstyle='->')
    fig.patches.append(arrow)
    ax0tr = axs['F'].transData # Axis 0 -> Display
    ax1tr = axs['A'].transData
    ptB = figtr.transform(ax0tr.transform((month,month_avg)))
    ptE = figtr.transform(ax1tr.transform((-2.1E3,6.74E6)))
    arrow = matplotlib.patches.FancyArrowPatch(ptB,ptE,transform=fig.transFigure,\
        connectionstyle="arc3,rad=-0.2",arrowstyle='->')
    fig.patches.append(arrow)

    save_figures(title)
    print(f'{i} number of lines have been plotted')

def plot_geodistribution_with_cities_Jiyingsproject(label: str, title:str, df_in: pd.DataFrame, cb_scale: float = 0):
    '''
    This module is aim to plot the input variable as geospatial scale (and DO have specifiy city view)
    Note: As the LSOA code is the unique identifier, this module accept the DataFrame which have one 
        column of LSOA code, and another column for the variable to plot

    Arguments:
    label: the legend label for colorbar
    title: title of the figure (be stored as file name as well)
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
    df: df should be two column pd.DataFrame which have one column of LSOA code, and another column for the variable to plot
                in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    def basic_settings(df_geo):
        # Set the plot
        color_theme = 'coolwarm'
        mosaic = '''
        A
        A
        '''
        fig = plt.figure(figsize=(11,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        UK_gdf.boundary.plot(ax=axs['A'],color='k',linewidth=0.5)
        boundary = df_geo.bounds
        boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
        axs['A'].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
        axs['A'].set_xlim(([boundary[0]-5E4,boundary[2]]))
        #plt.subplots_adjust(left=0)
        cax = fig.add_axes([0.9, 0.1, 0.02, 0.8])

        return axs, cax, color_theme, UK_gdf

    # Get Geospatial shapes:
    df_geo = retrieve_ONS_shape_from_KG()

    # Make a copy of df_in
    df = copy.deepcopy(df_in)
    
    # Initilize the graph
    axs, cax, color_theme, UK_gdf = basic_settings(df_geo)
    
    # Revising the data
    df_geo, val_values =data_treatment(df_geo, title, df)
    
    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    bottom = np.min(val_values)
    top = np.max(val_values)
    divnorm = cl.Normalize(vmin=bottom, vmax=top)

    axs_xbounds = [np.array([-2.815E5,-1E5]),np.array([-2.838E5,-1.05E5]),np.array([1.3E5,1.9E5]),np.array([-1.957E5,0.957E5])]
    axs_ybounds = [np.array([7.107E6,7.2652E6]),np.array([7.206E6,7.41E6]),np.array([6.78E6,6.88E6]),np.array([6.5E6,6.68E6])]
    
    tl = df_geo.plot(column=f"{title}",
                cmap=color_theme,
                antialiased=False,
                ax=axs['A'],
                legend=True,
                norm=divnorm,
                cax=cax)

    # Create a colorbar for the plot
    create_color_bar(color_theme, divnorm, label, axs, cax, df_geo[f"{title}"])
    cax.ticklabel_format(axis="y", style="sci", scilimits=(0,0))  

    axs['A'].set_xticks([])
    axs['A'].set_yticks([])
    axs['A'].spines["top"].set_visible(False)
    axs['A'].spines["right"].set_visible(False)
    axs['A'].spines["left"].set_visible(False)
    axs['A'].spines["bottom"].set_visible(False)

    order = [4,3,2,1]
    loc1 = [1,2,2,1]
    loc2 = [3,3,3,4]
    names = ['A','B','C','D']

    for f in range(4):
        if f == 0 or f == 1:
            axins2 = inset_axes(axs['A'], width=4, height=2.5,
                    bbox_to_anchor=(0.5, 0.3),
                    bbox_transform=axs['A'].transAxes, loc=order[f], borderpad=6)
        else:
            axins2 = inset_axes(axs['A'], width=4, height=2.5,
                    bbox_to_anchor=(0.5, 0.5),
                    bbox_transform=axs['A'].transAxes, loc=order[f], borderpad=6)
        plt.subplots_adjust(bottom = 0.225,left=0.07)
        plt.setp(axins2.get_xticklabels(), visible=False)
        plt.setp(axins2.get_yticklabels(), visible=False)
        UK_gdf.boundary.plot(ax=axins2,color='k',linewidth=0.5)
        axins2.set_xticks([])
        axins2.set_title(str(names[f]))
        axins2.set_yticks([])
        axins2.set_ylim(axs_ybounds[f])
        axins2.set_xlim(axs_xbounds[f])
        df_geo.plot(column=f"{title}",cmap=color_theme,\
                antialiased=False,\
                ax = axins2,\
                norm = divnorm)
        mark_inset(axs['A'],axins2,loc1=loc1[f],loc2=loc2[f],fc='none',ec='0')
    
    # Store the figures
    save_figures(title)

# ------------------------ Line chart Plot --------------------------------- #
def plot_line_chart(filename: str, y_label: str, df_in: pd.DataFrame, temproal = True):
    '''
    To create a line chart to represent the monthly data
    Note: the input dataframe should be looks like:
    For temproal = True:
# -------------------------------------------------------------#
#                Jan Feb Mar .... Nov Dec                      #
# [index_name]                                                 #
# [index_name]                                                 #
#    ...                                                       #
# -------------------------------------------------------------#
or for temproal = False:
# -------------------------------------------------------------#
#                col1 col2 col3 ...                            #
# [index_name]                                                 #
# [index_name]                                                 #
#    ...                                                       #
# -------------------------------------------------------------#

    so for how many rows were in this df, how many line will be plotted. noted that the column names 
    don't matter in the case of temproal = True as it default the sequence as Jan to Dec. But if which
    is False, the column name will be used as x-axis labels (in the second example, col1, col2, col3...)
    And in both cases, the [index_name] will be used as the label of this line

    If in some case you may want fill_between feature to represent a range, please provide several rows with same 
    [index_name], for those rows have same [index_name], the function will automatically fill between the row have
    max average value and the row have min average value, while for rows in the middle will still be plotted.
    i.e., sequence don't matter as long as they have the same [index_name]

    Arguments:
    filename: str, name of the figure file you may want to store
    y_label: str, name of the y-axis legend
    df: pd.DataFrame, dataframe to be processd
    temproal: if True, the plot have defalut x-axis from Jan - Dec, the df_in must be 12 columns representing data from Jan to Dec
    '''
    df = copy.deepcopy(df_in)
    # Group the rows by index name
    groups = df.groupby(df.index)

    # Initialize the plot
    fig, axs = plt.subplots(1,1,figsize=(5,3))
    plt.tight_layout()
    
    i = 0
    for index_name, group in groups:
        if len(group) == 1:
            # Plot the line for the single-data row
            sb.lineplot(x=group.columns, y=group.values[0], label=index_name, color='black', linewidth=2, linestyle=['solid', 'dashed', 'dotted', 'dashdot','solid', 'dashed', 'dotted', 'dashdot'][i])
            i+=1

        else:
            row_means = group.mean(axis=1)
            # Reset the index of the group
            group = group.reset_index(drop=True)
            group.index = row_means
            # Sort the group by the index
            group = group.sort_index()
            
            # Get the minimum and maximum rows
            y1 = group.iloc[0]
            y2 = group.iloc[-1]
            # Plot the fill between the minimum and maximum rows
            plt.fill_between(x=df.columns, y1=y1, y2=y2, color='black',linestyle=['solid', 'dashed', 'dotted', 'dashdot','solid', 'dashed', 'dotted', 'dashdot'][i],alpha=0.1)
            # Plot the remaining rows
            for _, row in group.iloc[1:-1].iterrows():
                sb.lineplot(x=df.columns, y=row, label=index_name, color='black', linewidth=2, linestyle=['solid', 'dashed', 'dotted', 'dashdot','solid', 'dashed', 'dotted', 'dashdot'][i])
                i+=1
    axs.legend(frameon=False)

    plt.subplots_adjust(left=0.175)
    if temproal == True:
        axs.set_xticks([0,1,2,3,4,5,6,7,8,9,10,11])
        axs.set_xticklabels(labels = ['J','F','M','A','M','J','J','A','S','O','N','D'])
    else:
        axs.set_xticks(range(len(df.columns)))
        axs.set_xticklabels(df.columns)
    axs.set_xlabel('')
    axs.set_ylabel(y_label)

    save_figures(filename)
    print(f'{i} number of lines have been plotted')

def plot_box_and_whisker_for_muiltiple_entities(filename: str, y_label: str, df_in: pd.DataFrame, pdf = False) :

    flierprops = dict(markerfacecolor="k", markersize=0.05, linestyle="none", markeredgecolor="k")
    df = copy.deepcopy(df_in)
    # Initialize the plot
    fig, axs = plt.subplots(1,1,figsize=(5,2.5))
    plt.tight_layout()
    y_data = df.iloc[:, 1:]
    ax_box = sb.boxplot(
        x="variable", y="value", data=pd.melt(y_data),
        fliersize=0.05,
        whis=2,
        linewidth=1.2,
        ax=axs,
        color="w",
        flierprops=flierprops,
    )
    sb.pointplot(
        x="variable", y="value", data=pd.melt(get_median(df_in=df)),
        ax=axs,
        color="r",
        markers=".",
    )
    for i, box in enumerate(ax_box.artists):
        box.set_edgecolor("black")
        box.set_facecolor("white")
        # iterate over whiskers and median lines
        for j in range(6 * i, 6 * (i + 1)):
            ax_box.lines[j].set_color("black")
    plt.subplots_adjust(
        left=0.175, bottom=0.092, right=0.967, top=0.949, wspace=0.2, hspace=0.14
    )
    axs.set_xlabel("")
    axs.set_ylabel(y_label)
    axs.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    num_columns = df.shape[1] - 1
    column_indices = list(range(num_columns))
    axs.set_xticks(column_indices)
    axs.set_xticklabels(labels = df.columns[1:])
    plt.legend()
    save_figures(filename, pdf)

def plot_box_and_whisker(filename: str, y_label: str, df_in: pd.DataFrame) :
    '''
    This function is dedecated to plot the monthly distribution in a box_and_whisker form
    Note: the input dataframe should be looks like:
# -------------------------------------------------------------#
#                LSOA_code Jan Feb Mar .... Nov Dec            #
# 0                                                            #
# 1                                                            #
#    ...                                                       #
# -------------------------------------------------------------#
    what is in the first column or first column's label don't matter because that will be ignored
    what is important is the following 2nd to 13th columns shall follow the data sequence from Jan to Dec
    Arguments:
    filename: str, name of the figure file you may want to store
    y_label: str, name of the y-axis legend
    df: pd.DataFrame, dataframe to be processd
    '''
    flierprops = dict(markerfacecolor="k", markersize=0.05, linestyle="none", markeredgecolor="k")
    df = copy.deepcopy(df_in)
    # Initialize the plot
    fig, axs = plt.subplots(1,1,figsize=(5,2.5))
    plt.tight_layout()
    y_data = df.iloc[:, 1:]
    ax_box = sb.boxplot(
        x="variable", y="value", data=pd.melt(y_data),
        fliersize=0.05,
        whis=2,
        linewidth=1.2,
        ax=axs,
        color="w",
        flierprops=flierprops,
    )
    sb.pointplot(
        x="variable", y="value", data=pd.melt(get_median(df_in=df)),
        ax=axs,
        color="r",
        markers=".",
    )
    for i, box in enumerate(ax_box.artists):
        box.set_edgecolor("black")
        box.set_facecolor("white")
        # iterate over whiskers and median lines
        for j in range(6 * i, 6 * (i + 1)):
            ax_box.lines[j].set_color("black")
    plt.subplots_adjust(
        left=0.175, bottom=0.092, right=0.967, top=0.949, wspace=0.2, hspace=0.14
    )
    axs.set_xlabel("")
    axs.set_ylabel(y_label)
    axs.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    axs.set_xticks([0,1,2,3,4,5,6,7,8,9,10,11])
    axs.set_xticklabels(labels = ['J','F','M','A','M','J','J','A','S','O','N','D'])
    plt.legend()
    save_figures(filename)

def plot_var_versus_result(filename: str, y_label: str, x_label:str, df_in: pd.DataFrame, independent_var: list):
    '''
    This function is dedicated to plot a line chart, using one independent variable as x-axis, while the result (dependent variable) as y-axis
    for example:
    emission = f (uptake)    The plot in this case then be plotting emission changing along with change of uptake.
    Note: 
    continue on this example, for a given list of uptake value, say [0,0.1,... 0.9,1], we need to calculate the emission and 
    arrange the result into a dataframe looks like:
# ----------------------------------------------------------------------------#
#                f (uptake[0])   f (uptake[1])  f (uptake[2])          ...    #
# [index_name]                                                                #
# [index_name]                                                                #
#    ...                                                                      #
# ----------------------------------------------------------------------------#
    where this emission may vary in different senario, such as tmin tas tmax, in that case, the calculation 
    result can be appended to the df using the SAME index name

    so for how many rows were in this df, how many line will be plotted. noted that the column names 
    don't matter as it default the sequence as independent_var. However, the [index_name] will be used as 
    the label of this line

    If in some case you may want fill_between feature to represent a range, please provide several rows with same 
    [index_name], for those rows have same [index_name], the function will automatically fill between the row have
    max average value and the row have min average value, while for rows in the middle will still be plotted.
    i.e., sequence don't matter as long as they have the same [index_name]

    Arguments:
    filename: str, name of the figure file you may want to store
    y_label: str, name of the y-axis legend
    x_label: str, name of the x-axis legend
    df: pd.DataFrame, dataframe to be processd
    independent_var: list, the independent variable
    '''
    # Initialize the plot
    plt.figure(figsize=(5,3.5))
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.tight_layout()

    # Group the rows by index name
    df = copy.deepcopy(df_in)
    groups = df.groupby(df.index)

    i = 0
    for index_name, group in groups:
        if len(group) == 1:
            # Plot the line for the single-data row
            plt.plot(independent_var, group.values[0], label=index_name, color='black', linewidth=2, linestyle=['solid', 'dashed', 'dotted', 'dashdot','solid', 'dashed', 'dotted', 'dashdot'][i])
            i+=1

        else:
            row_means = group.mean(axis=1)
            # Reset the index of the group
            group = group.reset_index(drop=True)
            group.index = row_means
            # Sort the group by the index
            group = group.sort_index()
            
            # Get the minimum and maximum rows
            y1 = group.iloc[0]
            y2 = group.iloc[-1]
            # Plot the fill between the minimum and maximum rows
            plt.fill_between(x=independent_var, y1=y1, y2=y2, color='black',linestyle=['solid', 'dashed', 'dotted', 'dashdot','solid', 'dashed', 'dotted', 'dashdot'][i],alpha=0.1)
            # Plot the remaining rows
            for _, row in group.iloc[1:-1].iterrows():
                plt.plot(independent_var, group.values[0], label=index_name, color='black', linewidth=2, linestyle=['solid', 'dashed', 'dotted', 'dashdot','solid', 'dashed', 'dotted', 'dashdot'][i])
                i+=1
    plt.ylim(bottom=0)
    plt.legend(frameon=False)
    plt.subplots_adjust(left = 0.127)
    save_figures(filename)

def plot_line_with_annotations(x_values, y1_values, y2_values, x_label, y1_label, y2_label, title, pdf = False):
    # Create figure and first subplot (left y-axis)
    fig, ax1 = plt.subplots()

    # Plot the first line (y1_values) with left y-axis
    line1, =ax1.plot(x_values, y1_values, marker='o', linestyle='-', color='blue', label = y1_label)
    ax1.set_ylabel(y1_label, color='black')
    ax1.tick_params(axis='y', labelcolor='black')

    # Create second subplot (right y-axis) and share x-axis with the first subplot
    ax2 = ax1.twinx()

    # Plot the second and third lines (y2_values, y3_values) with right y-axis
    line2, =ax2.plot(x_values, y2_values, marker='s', linestyle='-', color='green', label = y2_label)
    ax2.set_ylabel(y2_label, color='black')
    ax2.tick_params(axis='y', labelcolor='black')

    # Combine the legend from both subplots
    lines = [line1, line2]
    labels = [line.get_label() for line in lines]
    ax1.legend(lines, labels, loc='upper left')

    # Set x-axis label and title
    plt.xlabel(x_label)
    plt.title(title)

    save_figures(title, pdf)

# ------------------------ Scatter Plot --------------------------------- #
def scatter_plot(filename, x_values:np.array, y_values:np.array, x_label:str, y_label: str, title: str):
    
    # Plot scatter plot
    plt.scatter(x_values, y_values)

    # Plot y=x line
    x_line = np.linspace(min(x_values.flatten()), max(x_values.flatten()), 100)
    plt.plot(x_line, x_line, 'r--')

    # Set labels and title
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    plt.title(title)
    save_figures(filename)

def plot_kernel_density(data, filename, label, linelable = ''):
    #plt.clf() 
    density = sns.kdeplot(data, label = linelable)
    x_vals, y_vals = density.get_lines()[0].get_data()
    max_density_index = np.argmax(y_vals)
    max_density_x = x_vals[max_density_index]
    max_density_y = y_vals[max_density_index]
    
    # plt.axvline(max_density_x, color='red', linestyle='--', label='Max Density')
    plt.xlabel(label)
    plt.legend()
    save_figures(filename)

# ------------------------- 3D Plot ------------------------------------- #
def three_Dimensional_scatter_plot(x_array, y_array, z_array, 
                                   x_label, y_label, z_label,
                                   title, pdf = False):
# Generate random data for demonstration

    # Create the figure and 3D scatter plot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    scatter = ax.scatter(x_array, y_array, z_array, c=z_array, cmap='viridis')
    # Create a surface by connecting the points
    ax.plot_trisurf(x_array.flatten(), y_array.flatten(), z_array.flatten(), cmap='viridis', edgecolor='black')

    # Add color bar
    colorbar = plt.colorbar(scatter)
    colorbar.set_label(z_label)

    # Set labels for the axes
    ax.set_xlabel(x_label, fontsize=10)
    ax.set_ylabel(y_label, fontsize=10)
    ax.set_zlabel(z_label, fontsize=10)
    ax.set_title(title, fontsize=10)

    save_figures(title, pdf)
    plt.show()

# ------------------------ Routes ------------------------------------------ #
def output_inequality_index_df(url_cop, url_change_of_fuel, url_fuel_cost, url_inequality_index):
    # Read the temp data
    df_temp = retrieve_temp_from_KG()

    # Convert df into tensor
    unique_LSOA, temp_tensor = convert_to_tensor(input = df_temp)

    # call calculation agent
    cop = call_cop_agent(url_cop, temp_tensor, OM_DEGREE_C)

    # convert gas/elec consumption into tensor according to unique_LSOA
    # get gas/elec consumption df
    df_gas = retrieve_gas_data_from_KG(per_household=True)
    df_elec = retrieve_elec_data_from_KG(per_household=True)
    
    df_gas = select_column(df_gas, ['s','gas_consump_perhousehold'])
    df_elec = select_column(df_elec, ['s','elec_consump_perhousehold'])

    # get monthly distribution gas/elec
    gas_monthly_distribution = read_from_web_monthly_distribution_gas()
    elec_monthly_distribution = read_from_web_monthly_distribution_elec()

    # convert gas/elec df to tensor
    _, gas_tensor = convert_to_tensor(input = df_gas, monthly_ref= gas_monthly_distribution, LSOA_index_id= unique_LSOA, year=YEAR)
    _, elec_tensor = convert_to_tensor(input = df_elec, monthly_ref= elec_monthly_distribution, LSOA_index_id= unique_LSOA, year=YEAR)

    # compare the shape, select the shortest one
    temp_tensor, unique_LSOA, gas_tensor, cop, elec_tensor = compare_tensor(temp_tensor,unique_LSOA,gas_tensor,cop,elec_tensor)

    # call calculation agent
    uptake = 1
    # call change of fuel agent
    change_of_gas, change_of_elec = call_change_of_fuel_agent(url_change_of_fuel, uptake, gas_tensor, 0.9, cop)

    # fabricate into df
    df_change_of_gas = tensor_to_df(-change_of_gas,unique_LSOA,'tas',True)
    df_change_of_elec = tensor_to_df(change_of_elec,unique_LSOA,'tas',True)

    # calculate change of cost
    df_cost_total, _, _ = call_fuel_cost_agent(url_fuel_cost ,df_change_of_elec ,df_change_of_gas ,YEAR ,True)
    df_cost_total = select_column(df_cost_total,[0,1])

    # get the fuel poverty data, convert into tensor according to unique_LSOA
    df_fuel_poor = retrieve_fuel_poor_from_KG()
    df_fuel_poor = select_column(df_fuel_poor, ['s','result'])


    # calculate inequality index
    df_inequality_index = call_inequality_index_agent(url_inequality_index, df_cost_total, df_fuel_poor)

    return df_inequality_index

def output_fuel_cost_df(url):
    # Retrieve consumption data from KG
    df_elec = retrieve_elec_data_from_KG() 
    df_elec = select_column(df_elec, ['s','usage']) 
    # df_gas = retrieve_gas_data_from_KG()
    # df_gas = select_column(df_gas, ['s','usage']) df_gas_in = df_gas,

    # Call the Calculation agent
    df_cost, df_ele, df_gas = call_fuel_cost_agent(df_elec_in = df_elec,  url= url,  year = '2020',annual=True)

    return df_cost, df_ele, df_gas

def output_fuel_emission_df(url):
    # Retrieve consumption data from KG
    df_elec = retrieve_elec_data_from_KG() 
    df_elec = select_column(df_elec, ['s','usage']) 
    df_gas = retrieve_gas_data_from_KG()
    df_gas = select_column(df_gas, ['s','usage']) 

    # Call the Calculation agent
    df_emission_total, df_emission_elec, df_emission_gas = call_fuel_emission_agent(df_gas_in = df_gas, df_elec_in = df_elec,  url= url,  year = '2020',annual=True)
    
    return df_emission_total, df_emission_elec, df_emission_gas

def output_change_of_fuel_df(url_cop,url_change_of_fuel,):
    # Read the temp data
    df_temp = retrieve_temp_from_KG()

    # Convert df into tensor
    unique_LSOA, temp_tensor = convert_to_tensor(input = df_temp)

    # call calculation agent
    cop = call_cop_agent(url_cop, temp_tensor, OM_DEGREE_C)

    # call calculation agent
    uptake = 0.5

    # convert gas consumption into tensor according to unique_LSOA
    # get gas consumption df
    df_gas = retrieve_gas_data_from_KG()
    df_gas = select_column(df_gas, ['s','usage'])

    # get monthly distribution gas
    gas_monthly_distribution = read_from_web_monthly_distribution_gas()

    # convert gas df to tensor
    _, gas_tensor = convert_to_tensor(input = df_gas, monthly_ref= gas_monthly_distribution, LSOA_index_id= unique_LSOA, year=YEAR)

    # compare the shape, select the shorther one
    temp_tensor, unique_LSOA, gas_tensor, cop = compare_tensor(temp_tensor,unique_LSOA,gas_tensor,cop)

    # call agent
    change_of_gas, change_of_elec = call_change_of_fuel_agent(url_change_of_fuel, uptake, gas_tensor,0.9,cop)
    return change_of_gas, change_of_elec

# df_inequality_index = output_inequality_index_df('http://localhost:5010/api/lsoacalculationagent_cop/calculation/cop',\
#                            'http://localhost:5040/api/lsoacalculationagent_change_of_fuel/calculation/change_of_fuel',\
#                            'http://localhost:5020/api/lsoacalculationagent_fuel_cost/calculation/fuel_cost',\
#                            'http://localhost:5050/api/lsoacalculationagent_inequality_index/calculation/inequality_index',\
#                            )
# convert_df(df_inequality_index)
# plot_geodistribution('inequality index', 'inequality index', df_inequality_index, 0)


'''
df_full = call_pickle('./Data/pickles/df in function get_all_data')
df_full['LSOA_code'] = df_full['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
'''
'''
    LSOA_code	ons_shape	Electricity_consump	Electricity_meter	Electricty_cosumption_per_household	Gas_consump	Gas_meter	Gas_nonmeter	Gas_consumption_per_household	FuelPoor_%	Household_num	temp
'''
'''
df_elec = df_full[['LSOA_code', 'Electricity_consump']]
df_gas = df_full[['LSOA_code', 'Gas_consump']]
df_temp = df_full[['LSOA_code', 'temp']]
'''

######### Test for calling cop calculation agent #########
# Test for COP agent ---------------------------------------------
'''
# Read the temp data
df_temp = retrieve_temp_from_KG()

# Convert df into tensor
unique_LSOA, results_tensor = convert_to_tensor(input = df_temp)

# call calculation agent
url = 'http://localhost:5010/api/lsoacalculationagent_cop/calculation/cop'
cop = call_cop_agent(url, results_tensor, OM_DEGREE_C)
print(cop)
'''
# ----------------------------------------------------------------

# Test for fuel cost agent----------------------------------------
'''
# Retrieve consumption data from KG
df_elec = retrieve_elec_data_from_KG() 
df_elec = select_column(df_elec, ['s','usage']) 
# df_gas = retrieve_gas_data_from_KG()
# df_gas = select_column(df_gas, ['s','usage']) df_gas_in = df_gas,

# Call the Calculation agent
url = 'http://localhost:5004/api/lsoacalculationagent_fuel_cost/calculation/fuel_cost'
df_cost, df_ele, df_gas = call_fuel_cost_agent(df_elec_in = df_elec,  url= url,  year = '2020',annual=True)
print(df_ele)
'''
# ----------------------------------------------------------------

# Test for fuel emission agent------------------------------------
'''
# Retrieve consumption data from KG
df_elec = retrieve_elec_data_from_KG() 
df_elec = select_column(df_elec, ['s','usage']) 
df_gas = retrieve_gas_data_from_KG()
df_gas = select_column(df_gas, ['s','usage']) 

# Call the Calculation agent
url = 'http://localhost:5004/api/lsoacalculationagent_fuel_emission/calculation/fuel_emission'
df_emission_total, df_emission_elec, df_emission_gas = call_fuel_emission_agent(df_gas_in = df_gas, df_elec_in = df_elec,  url= url,  year = '2020',annual=True)
print(df_emission_elec)
'''
# ----------------------------------------------------------------

# Test for change of fuel agent ----------------------------------
'''
# Read the temp data
df_temp = retrieve_temp_from_KG()

# Convert df into tensor
unique_LSOA, temp_tensor = convert_to_tensor(input = df_temp)

# call calculation agent
url = 'http://localhost:5005/api/lsoacalculationagent_cop/calculation/cop'
cop = call_cop_agent(url, temp_tensor, OM_DEGREE_C)

# call calculation agent
url = 'http://localhost:5003/api/lsoacalculationagent_change_of_fuel/calculation/change_of_fuel'
uptake = 0.5

# convert gas consumption into tensor according to unique_LSOA
# get gas consumption df
df_gas = retrieve_gas_data_from_KG()
df_gas = select_column(df_gas, ['s','usage'])

# get monthly distribution gas
gas_monthly_distribution = read_from_web_monthly_distribution_gas()

# convert gas df to tensor
_, gas_tensor = convert_to_tensor(input = df_gas, monthly_ref= gas_monthly_distribution, LSOA_index_id= unique_LSOA, year=YEAR)

# compare the shape, select the shorther one
temp_tensor, unique_LSOA, gas_tensor, cop = compare_tensor(temp_tensor,unique_LSOA,gas_tensor,cop)

# call agent
change_of_gas, change_of_elec = call_change_of_fuel_agent(url, uptake, gas_tensor,0.9,cop)
print(change_of_elec)
'''
# ----------------------------------------------------------------

# Test for inequality index agent ----------------------------------
'''
# Read the temp data
df_temp = retrieve_temp_from_KG()

# Convert df into tensor
unique_LSOA, temp_tensor = convert_to_tensor(input = df_temp)

# call calculation agent
url = 'http://localhost:5005/api/lsoacalculationagent_cop/calculation/cop'
cop = call_cop_agent(url, temp_tensor, OM_DEGREE_C)

# convert gas/elec consumption into tensor according to unique_LSOA
# get gas/elec consumption df
df_gas = retrieve_gas_data_from_KG()
df_elec = retrieve_elec_data_from_KG()
df_gas = select_column(df_gas, ['s','usage'])
df_elec = select_column(df_elec, ['s','usage'])

# get monthly distribution gas/elec
gas_monthly_distribution = read_from_web_monthly_distribution_gas()
elec_monthly_distribution = read_from_web_monthly_distribution_elec()

# convert gas/elec df to tensor
_, gas_tensor = convert_to_tensor(input = df_gas, monthly_ref= gas_monthly_distribution, LSOA_index_id= unique_LSOA, year=YEAR)
_, elec_tensor = convert_to_tensor(input = df_elec, monthly_ref= elec_monthly_distribution, LSOA_index_id= unique_LSOA, year=YEAR)

# compare the shape, select the shortest one
temp_tensor, unique_LSOA, gas_tensor, cop, elec_tensor = compare_tensor(temp_tensor,unique_LSOA,gas_tensor,cop,elec_tensor)

# call calculation agent
url = 'http://localhost:5003/api/lsoacalculationagent_change_of_fuel/calculation/change_of_fuel'
uptake = 0.5
# call change of fuel agent
change_of_gas, change_of_elec = call_change_of_fuel_agent(url, uptake, gas_tensor, 0.9, cop)

# fabricate into df
df_change_of_gas = tensor_to_df(-change_of_gas,unique_LSOA,'tas',True)
df_change_of_elec = tensor_to_df(change_of_elec,unique_LSOA,'tas',True)

# calculate change of cost
url = 'http://localhost:5004/api/lsoacalculationagent_fuel_cost/calculation/fuel_cost'
df_cost_total, _, _ = call_fuel_cost_agent(url ,df_change_of_elec ,df_change_of_gas ,YEAR ,True)
df_cost_total = select_column(df_cost_total,[0,1])

# get the fuel poverty data, convert into tensor according to unique_LSOA
df_fuel_poor = retrieve_fuel_poor_from_KG()
df_fuel_poor = select_column(df_fuel_poor, ['s','result'])

# # First example dataframe
# np.random.seed(0)
# letters = ['a', 'b', 'c', 'd', 'e']
# df_cost_total = pd.DataFrame({'letters': letters, 'floats': np.random.randint(1000, 10000, size=len(letters))})

# # Second example dataframe
# np.random.seed(1)
# letters2 = letters + ['f', 'g', 'h', 'i', 'j', 'k', 'l']
# df_fuel_poor = pd.DataFrame({'letters': letters2, 'floats': np.random.rand(len(letters2))})

# calculate inequality index
url = 'http://localhost:5007/api/lsoacalculationagent_inequality_index/calculation/inequality_index'
df_inequality_index = call_inequality_index_agent(url, df_cost_total, df_fuel_poor)
print(df_inequality_index)
'''
# ----------------------------------------------------------------

# ----------------------------------------------------------------
##########################################################

############### Test for plot_geodistribution ############
# Test for geodistribution_with_cities ---------------------------
'''
df = retrieve_elec_data_from_KG()
df['Electricty cosumption per household'] = df['usage'].to_numpy() /df['meter'].to_numpy() 
df = drop_column(df,'meter')
df = drop_column(df,'usage')
plot_geodistribution_with_cities(label = 'kWh/year/household', title = 'Electricity Consumption', df =df, cb_scale = 1.5)
'''
# ----------------------------------------------------------------

# Test for multiple geospatial plot (using uptake%) --------------
'''
# Exclude NaN data
df_temp = remove_NaN(df_temp)
LSOA_index, results_tensor = convert_to_tensor(df_temp)
LSOA_index = {'http://statistics.data.gov.uk/id/statistical-geography/' + key: value for key, value in LSOA_index.items()}

uptake_list = [0,0.2,0.4,0.8,1]
array = np.empty((41726, 0))
df_toplot_final = pd.DataFrame(array)
df_toplot_final['LSOA_index'] = LSOA_index.keys()

for uptake in uptake_list:
    # Calculate COP
    cop_tensor = COP(results_tensor)

    # Calculate delta_gas
    _ , gassonsump_tensor = convert_to_tensor(df_gas, monthly_gas_consumption_2020, LSOA_index_id = LSOA_index)
    delta_gas_tensor = delta_gas(uptake, gassonsump_tensor)

    # Calculate delta_electricity
    _ , elecconsump_tensor = convert_to_tensor(df_elec, monthly_electricity_consumption_2020, LSOA_index_id = LSOA_index)
    delta_elec_tensor = delta_elec(delta_gas_tensor, cop_tensor)

    # Calculate remaning_elec_tensor
    remaning_elec_tensor = elecconsump_tensor + delta_elec_tensor
    
    # Get elec_array based on T_mean
    remaning_elec_arrays = remaning_elec_tensor[1]

    peak_power_values = np.zeros(len(remaning_elec_arrays[:,1]))
    for i in range(len(remaning_elec_arrays)):
        peak_power_values[i] = max(remaning_elec_arrays[i,:])
    
    df_toplot_final['new_column_name'] = peak_power_values
    df_toplot_final = df_toplot_final.rename(columns={'new_column_name': 'uptake as {}'.format(uptake)})

plot_multiple_geodistribution('Monthly Electrical Power Consumption (kWh/month)','peak_power_nationwide',df_toplot_final,1.5)
'''
# ----------------------------------------------------------------

# Test for plot_temperature_versus_var (COP as var) --------------
'''
# Prepare df_temproal
df_temp = remove_NaN(df_temp)
df_temproal = pd.DataFrame()
row1 = df_temp.loc[df_temp.iloc[:, 0] == ONS_ID + 'E01004731', :]
row1.index = [0]
row2 = df_temp.loc[df_temp.iloc[:, 0] == ONS_ID + 'E01019806', :]
row2.index = [0]

for i in range(12):
    for key, value in date_dict.items():
        if value == i:
            date_key = key
            df_temproal = df_temproal.assign(**{f'{date_key}': np.nan})
            for j in range(3):
                for key2, value2 in t_dict.items():
                    if value2 ==j:
                        temp_key = key2
                        df_temproal.loc[j,f'{date_key}'] = row1.at[0,'temp'][key][temp_key]
for i in range(12):
    for key, value in date_dict.items():
        if value == i:
            date_key = key
            for j in range(3):
                for key2, value2 in t_dict.items():
                    if value2 ==j:
                        temp_key = key2
                        df_temproal.loc[j+3,f'{date_key}'] = row2.at[0,'temp'][key][temp_key]

df_temproal.index = [ONS_ID + 'E01004731', ONS_ID + 'E01004731', ONS_ID + 'E01004731',
                     ONS_ID + 'E01019806', ONS_ID + 'E01019806', ONS_ID + 'E01019806']
df_temproal = df_temproal.applymap(COP)
# Prepare df_dict
values = []
for key, val in df_temp['temp'].items():
    for key2, val2 in val.items():
        if key2 == '2020-02-01T12:00:00.000Z':
                values.append(val2)

# Reserve temp reading, as values
values_copy = copy.deepcopy(values)

# Calculate COP
df_temp['temp'] = values_copy
for key, val in df_temp['temp'].items():
    for key2, val2 in val.items():
        df_temp['temp'][key][key2] = COP(val2)
df_cop = df_temp.rename(columns = {'temp':'COP'})

# Assign temperature value back
df_temp['temp'] = values
df_dict, _ = data_treatment(df_temp,arg_name=False,arg_value_in = df_cop)

plot_temperature_versus_var(label = 'Coefficient of Performance (-)',
                            title='cop',
                            df_dict = df_dict, 
                            month = 2, 
                            df_temproal = df_temproal, 
                            cb_scale = 0.5)
'''
# ----------------------------------------------------------------

# Test for generating multiple year's data (cost)-----------------

'''
# generating 2019 data
unique_LSOA = call_pickle('./Data/pickles/unique_LSOA in function valid_LSOA_list')
# city = "Cambridge"
# csv_file = f"./Data/Local Authority/{city}.csv"
# unique_LSOA = []
# Read the CSV file and append the 'LSOA code' values to the list
# with open(csv_file, 'r') as file:
#     reader = csv.reader(file)
#     next(reader)  # Skip the header row if it exists
#     for row in reader:
#         unique_LSOA.append(row[0])

df_final = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
df_final = df_final.sort_values(by='LSOA_code')
df_final.reset_index(drop=True, inplace=True)
df_final['LSOA_code'] = df_final['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
for year in ['2018','2019','2020','2021','2022']:
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])

    if year == "2022":
        elec_consump, elec_meter = read_from_excel_elec(year = "2021", dict=True)
        gas_consump, gas_meter, gas_non_meter = read_from_excel_gas(year = "2021", dict=True)
    else:
        elec_consump, elec_meter = read_from_excel_elec(year = year, dict=True)
        gas_consump, gas_meter, gas_non_meter = read_from_excel_gas(year = year, dict=True)

    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_consump.get(x, np.nan)),3))
    df['Electricity_meter'] = df['LSOA_code'].apply(lambda x: float(elec_meter.get(x, np.nan)))
    df['Electricty_cosumption_per_household'] = df['Electricity_consump'].to_numpy() /df['Electricity_meter'].to_numpy()
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_consump.get(x, np.nan)),3))
    df['Gas_meter'] = df['LSOA_code'].apply(lambda x: float(gas_meter.get(x, np.nan)))
    df['Gas_consumption_per_household'] = df['Gas_consump'].to_numpy() /df['Gas_meter'].to_numpy()

    # Get index
    monthly_electricity_consumption = read_from_web_monthly_distribution_elec(year=year)
    monthly_gas_consumption = read_from_web_monthly_distribution_gas(year=year)
    
    df_elec = df[['LSOA_code','Electricty_cosumption_per_household']]
    df_gas = df[['LSOA_code','Gas_consumption_per_household']]

    df_cop = pd.read_csv("./Data/properties_csv/COP_mean.csv")
    # Select and reorder rows in df_2 based on identifiers in df_1
    df_cop = df_cop[df_cop['LSOA_code'].isin(df_elec['LSOA_code'])]
    
    df_elec = df_elec.sort_values(by='LSOA_code')
    df_elec.reset_index(drop=True, inplace=True)
    df_gas = df_gas.sort_values(by='LSOA_code')
    df_gas.reset_index(drop=True, inplace=True)
    df_cop = df_cop.sort_values(by='LSOA_code')
    df_cop.reset_index(drop=True, inplace=True)
    
    # Calculate fuel cost
    cost_elec = read_from_web_price_elec(year=year)
    cost_gas = read_from_web_price_gas(year=year)
    
    df_cost_total, df_cost_elec, df_cost_gas = fuel_cost(df_elec,df_gas,cost_elec,cost_gas,monthly_electricity_consumption,monthly_gas_consumption, annual=True)
    
    df_elec = monthly_disaggregation(df_elec,monthly_electricity_consumption,annual=False)
    df_gas = monthly_disaggregation(df_gas,monthly_gas_consumption,annual=False)

    # if year in ['2017','2018','2019', '2020', '2021','2022']:
    #     df_elec.to_csv(f'./Data/properties_csv/{year}/Elec_Consump_per_house.csv', index=False)
    #     df_gas.to_csv(f'./Data/properties_csv/{year}/Gas_Consump_per_house.csv', index=False)
    #     df_cost_total.to_csv(f'./Data/properties_csv/{year}/Total_Cost.csv', index=False)
    #     df_cost_elec.to_csv(f'./Data/properties_csv/{year}/Elec_Cost.csv', index=False)
    #     df_cost_gas.to_csv(f'./Data/properties_csv/{year}/Gas_Cost.csv', index=False)

    uptake = 1

    elec_consump = df_elec.iloc[:, 1:13].values
    gas_consump = df_gas.iloc[:, 1:13].values
    cop = df_cop.iloc[:, 1:13].values

    delta_gas_array = delta_gas(uptake, gas_consump)
    delta_elect_array = delta_elec(delta_gas_array, cop) 

    resulted_elec_consump = elec_consump + delta_elect_array
    resulted_gas_consump = gas_consump - delta_gas_array


    final_cost = cost_elec*resulted_elec_consump + cost_gas*resulted_gas_consump
    final_cost = np.sum(final_cost, axis=1)

    df_original_cost = pd.read_csv('./Data/properties_csv/2020/Total_Cost.csv')
    df_original_cost = df_original_cost[df_original_cost['LSOA_code'].isin(df_elec['LSOA_code'])]
    
    original_cost = df_original_cost.iloc[:, 1].values

    delta_cost = final_cost - original_cost

    df_final_cost = pd.DataFrame(delta_cost, columns=[f'{year}'])
    df_final = df_final.append(df_final_cost, ignore_index=True)

#plot_multiple_geodistribution('Fuel Cost \n (£/month/household)',f'@{city}household_cost_2017-2021_geoplot',df_final,0)
plot_box_and_whisker_for_five_entities(f'@{city}household_cost_2017-2021_box_and_whisker','Fuel Cost \n (£/month/household)',df_final)

'''
# ----------------------------------------------------------------
##########################################################
# all fuel cost
'''
unique_LSOA = call_pickle('./Data/temp_Repo/unique_LSOA in function valid_LSOA_list')
months = ['January','February','March','April','May','June','July','August','September','October','November','December']
df_final = pd.DataFrame(columns=months)
for year in ['2015','2016','2017','2018','2019']:
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    elec_consump, elec_meter = read_from_excel_elec(year = year, dict=True)
    gas_consump, gas_meter, gas_non_meter = read_from_excel_gas(year = year, dict=True)
    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_consump.get(x, np.nan)),3))
    df['Electricity_meter'] = df['LSOA_code'].apply(lambda x: float(elec_meter.get(x, np.nan)))
    df['Electricty_cosumption_per_household'] = df['Electricity_consump'].to_numpy() /df['Electricity_meter'].to_numpy()
    
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_consump.get(x, np.nan)),3))
    df['Gas_meter'] = df['LSOA_code'].apply(lambda x: float(gas_meter.get(x, np.nan)))
    df['Gas_consumption_per_household'] = df['Gas_consump'].to_numpy() /df['Gas_meter'].to_numpy()

    df_elec = df[['LSOA_code', 'Electricty_cosumption_per_household']]
    df_gas = df[['LSOA_code', 'Gas_consumption_per_household']]

    # Get index
    cost_elec = read_from_web_price_elec(year=year)
    cost_gas = read_from_web_price_gas(year=year)
    monthly_electricity_consumption = read_from_web_monthly_distribution_elec(year=year)
    monthly_gas_consumption = read_from_web_monthly_distribution_gas(year=year)

    # Calculate fuel cost
    df_cost_total, df_cost_elec, df_cost_gas = fuel_cost(df_elec,df_gas,cost_elec,cost_gas,monthly_electricity_consumption,monthly_gas_consumption, annual=False)
    # Calculate median value
    df_cost_total = get_median(df_cost_total, f'{year}')
    df_final = pd.concat([df_final, df_cost_total], axis=0)
    '''



############### Test for temproal_line_chart #############
# Test for fuel cost ---------------------------------------------
'''
# Calculate fuel cost
df_cost_total, df_cost_elec, df_cost_gas = fuel_cost(df_elec,df_gas,cost_elec_2020,cost_gas_2020,monthly_electricity_consumption_2020,monthly_gas_consumption_2020, annual=False)

# Calculate median value
df_cost_total = get_median(df_cost_total, 'Total cost')
df_cost_elec = get_median(df_cost_elec, 'Electricity cost')
df_cost_gas = get_median(df_cost_gas, 'Gas cost')

# Construct df to plot
df_to_plot = pd.concat([df_cost_total, df_cost_elec, df_cost_gas], axis=0)
plot_line_chart(filename='household_cost', y_label = 'Fuel Cost \n (£/month/household)',df = df_to_plot)
'''
# ----------------------------------------------------------------

# Test for emission ----------------------------------------------
'''
# Calculate fuel emission
df_emission_total, df_emission_elec, df_emission_gas = fuel_emission(df_elec,df_gas,carbon_intensity_CO2e_elec_2020,carbon_intensity_CO2e_gas_2020,monthly_electricity_consumption_2020,monthly_gas_consumption_2020, annual=False)
# Calculate median value
df_emission_total = get_median(df_emission_total, 'Total emissions')
df_emission_elec = get_median(df_emission_elec, 'Electricity emissions')
df_emission_gas = get_median(df_emission_gas, 'Gas emissions')

# Construct df to plot
df_to_plot = pd.concat([df_emission_total, df_emission_elec, df_emission_gas], axis=0)
plot_line_chart(filename='household_emissions', y_label = 'Emissions \n (kgCO$_2$eq/month/household)',df = df_to_plot)
'''
# ----------------------------------------------------------------

# Test for resulting_elec ----------------------------------------
'''
# Exclude NaN data
df_temp = remove_NaN(df_temp)
LSOA_index, results_tensor = convert_to_tensor(df_temp)
LSOA_index = {key.replace('http://statistics.data.gov.uk/id/statistical-geography/', ''): value for key, value in LSOA_index.items()}

uptake_list = [0,0.5,1]
array = np.empty((0, 12))
df_toplot_final = pd.DataFrame(array)
for uptake in uptake_list:
    # Calculate COP
    cop_tensor = COP(results_tensor)

    # Calculate delta_gas
    _ , gassonsump_tensor = convert_to_tensor(df_gas, monthly_gas_consumption_2020, LSOA_index_id = LSOA_index)
    delta_gas_tensor = delta_gas(uptake, gassonsump_tensor)

    # Calculate delta_electricity
    _ , elecconsump_tensor = convert_to_tensor(df_elec, monthly_electricity_consumption_2020, LSOA_index_id = LSOA_index)
    delta_elec_tensor = delta_elec(delta_gas_tensor, cop_tensor)

    # Calculate remaning_elec_tensor
    remaning_elec_tensor = elecconsump_tensor + delta_elec_tensor

    # Convert tensor back to array
    remaning_elec_arrays = np.nansum(remaning_elec_tensor, axis=1)

    # Create df to plot
    df_toplot = pd.DataFrame(remaning_elec_arrays)
    df_toplot.index = [f"uptake = {uptake}"] * len(df_toplot)
    df_toplot_final = df_toplot_final.append(df_toplot)

plot_line_chart('remaining electricity','Electricity Consumption (kWh/month)',df_toplot_final)
'''
# ----------------------------------------------------------------

# Test for box & whisker -----------------------------------------
'''
df_elec_monthly = monthly_disaggregation(df_elec,monthly_electricity_consumption_2020)
plot_box_and_whisker('box_and_whisker','Electricity Consumption \n (kWh/month/household)',df_elec_monthly)
'''
# ----------------------------------------------------------------

# Test for plot_var_versus_result(Energy) ------------------------
'''
# Exclude NaN data
df_temp = remove_NaN(df_temp)
LSOA_index, results_tensor = convert_to_tensor(df_temp)
LSOA_index = {key.replace('http://statistics.data.gov.uk/id/statistical-geography/', ''): value for key, value in LSOA_index.items()}
uptake_list = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]
array = np.empty((6, 0))
df_toplot_final = pd.DataFrame(array, index=['Elec', 'Elec','Elec','Gas','Gas','Gas'])
# Calculate COP
cop_tensor = COP(results_tensor)

for uptake in uptake_list:
    # Calculate delta_gas
    _ , gassonsump_tensor = convert_to_tensor(df_gas, monthly_gas_consumption_2020, LSOA_index_id = LSOA_index)
    delta_gas_tensor = delta_gas(uptake, gassonsump_tensor)

    # Calculate delta_electricity
    _ , elecconsump_tensor = convert_to_tensor(df_elec, monthly_electricity_consumption_2020, LSOA_index_id = LSOA_index)
    delta_elec_tensor = delta_elec(delta_gas_tensor, cop_tensor)

    # Calculate remaning_elec_tensor
    remaning_elec_tensor = elecconsump_tensor + delta_elec_tensor

    # Calculate remaining_gas_tenosr
    remaning_gas_tensor = gassonsump_tensor - delta_gas_tensor

    # Convert tensor back to array
    remaning_elec_arrays = np.nansum(remaning_elec_tensor, axis=1)
    remaning_elec_arrays = remaning_elec_arrays.sum(axis=1)
    remaning_gas_arrays = np.nansum(remaning_gas_tensor, axis=1)
    remaning_gas_arrays = remaning_gas_arrays.sum(axis=1)
    result = np.concatenate((remaning_elec_arrays, remaning_gas_arrays), axis=0)

    df_toplot_final[f'Uptake_is_{uptake}'] = result

plot_var_versus_result('Energy uptake','Energy Consumption (kWh/year)','% Uptake',df_toplot_final,uptake_list)
'''
# ----------------------------------------------------------------

# Test for plot multiple year line chart (cost) ------------------
'''
unique_LSOA = call_pickle('./Data/pickles/unique_LSOA in function valid_LSOA_list')
months = ['January','February','March','April','May','June','July','August','September','October','November','December']
df_final = pd.DataFrame(columns=months)
for year in ['2015','2016','2017','2018','2019']:
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    elec_consump, elec_meter = read_from_excel_elec(year = year, dict=True)
    gas_consump, gas_meter, gas_non_meter = read_from_excel_gas(year = year, dict=True)
    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_consump.get(x, np.nan)),3))
    df['Electricity_meter'] = df['LSOA_code'].apply(lambda x: float(elec_meter.get(x, np.nan)))
    df['Electricty_cosumption_per_household'] = df['Electricity_consump'].to_numpy() /df['Electricity_meter'].to_numpy()
    
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_consump.get(x, np.nan)),3))
    df['Gas_meter'] = df['LSOA_code'].apply(lambda x: float(gas_meter.get(x, np.nan)))
    df['Gas_consumption_per_household'] = df['Gas_consump'].to_numpy() /df['Gas_meter'].to_numpy()

    df_elec = df[['LSOA_code', 'Electricty_cosumption_per_household']]
    df_gas = df[['LSOA_code', 'Gas_consumption_per_household']]

    # Get index
    cost_elec = read_from_web_price_elec(year=year)
    cost_gas = read_from_web_price_gas(year=year)
    monthly_electricity_consumption = read_from_web_monthly_distribution_elec(year=year)
    monthly_gas_consumption = read_from_web_monthly_distribution_gas(year=year)

    # Calculate fuel cost
    df_cost_total, df_cost_elec, df_cost_gas = fuel_cost(df_elec,df_gas,cost_elec,cost_gas,monthly_electricity_consumption,monthly_gas_consumption, annual=False)
    # Calculate median value
    df_cost_total = get_median(df_cost_total, f'{year}')
    df_final = pd.concat([df_final, df_cost_total], axis=0)
plot_line_chart(filename='household_cost_2015-2019', y_label = 'Fuel Cost \n (£/month/household)',df_in = df_final)
'''
# ----------------------------------------------------------------

# Test for plot multiple year in one-line (median annual cost) ---
'''
unique_LSOA = call_pickle('./Data/pickles/unique_LSOA in function valid_LSOA_list')
df_final = pd.DataFrame()
for year in ['2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020']:
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    elec_consump, elec_meter = read_from_excel_elec(year = year, dict=True)
    gas_consump, gas_meter, gas_non_meter = read_from_excel_gas(year = year, dict=True)
    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_consump.get(x, np.nan)),3))
    df['Electricity_meter'] = df['LSOA_code'].apply(lambda x: float(elec_meter.get(x, np.nan)))
    df['Electricty_cosumption_per_household'] = df['Electricity_consump'].to_numpy() /df['Electricity_meter'].to_numpy()
    
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_consump.get(x, np.nan)),3))
    df['Gas_meter'] = df['LSOA_code'].apply(lambda x: float(gas_meter.get(x, np.nan)))
    df['Gas_consumption_per_household'] = df['Gas_consump'].to_numpy() /df['Gas_meter'].to_numpy()

    df_elec = df[['LSOA_code', 'Electricty_cosumption_per_household']]
    df_gas = df[['LSOA_code', 'Gas_consumption_per_household']]

    # Get index
    cost_elec = read_from_web_price_elec(year=year)
    cost_gas = read_from_web_price_gas(year=year)
    monthly_electricity_consumption = read_from_web_monthly_distribution_elec(year=year)
    monthly_gas_consumption = read_from_web_monthly_distribution_gas(year=year)

    # Calculate fuel cost
    df_cost_total, df_cost_elec, df_cost_gas = fuel_cost(df_elec,df_gas,cost_elec,cost_gas,monthly_electricity_consumption,monthly_gas_consumption, annual=False)
    # Calculate median value
    df_cost_total = get_median(df_cost_total, 'Median Annual Cost')
    df_cost_total[f'{year}'] = df_cost_total.sum(axis=1)
    df_final[f'{year}'] = df_cost_total[f'{year}'] 
plot_line_chart(filename='household_cost_Median Annual Cost_2010-2020', y_label = 'Fuel Cost \n (£/year/household)',df_in = df_final, temproal=False)
'''
# ----------------------------------------------------------------

# Test for plot multiple year in one-line (annual electricity consumption) ---
'''
unique_LSOA = call_pickle('./Data/pickles/unique_LSOA in function valid_LSOA_list')
df_final = pd.DataFrame()
for year in ['2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020']:
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    elec_consump, elec_meter = read_from_excel_elec(year = year, dict=True)
    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_consump.get(x, np.nan)),3))
    
    df_sum = pd.DataFrame({'Sum': df[['Electricity_consump']].sum()})
    df_final[f'{year}'] = df_sum['Sum']
plot_line_chart(filename='National_Elec_consump_2010-2020', y_label = 'Electricity consumption \n (kwh/year/)',df_in = df_final, temproal=False)
'''
# ----------------------------------------------------------------

# Test for plot multiple year in one-line (annual gas consumption) ---
'''
unique_LSOA = call_pickle('./Data/pickles/unique_LSOA in function valid_LSOA_list')
df_final = pd.DataFrame()
for year in ['2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020']:
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    gas_consump, gas_meter, gas_non_meter = read_from_excel_gas(year = year, dict=True)
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_consump.get(x, np.nan)),3))
    
    df_sum = pd.DataFrame({'Sum': df[['Gas_consump']].sum()})
    df_final[f'{year}'] = df_sum['Sum']
plot_line_chart(filename='National_Gas_consump_2010-2020', y_label = 'Gas consumption \n (kwh/year/)',df_in = df_final, temproal=False)
'''
# ----------------------------------------------------------------
##########################################################

# Do scatter plot (fuel price)
# change year & all data 
'''
uptake = 1
df_cop = pd.read_csv("./Data/properties_csv/COP_mean.csv")

final = []
for year in ['2019','2022']:

    df_elec = pd.read_csv(f'./Data/properties_csv/{year}/Elec_Consump_per_house.csv')
    df_gas = pd.read_csv(f'./Data/properties_csv/{year}/Gas_Consump_per_house.csv')
    df_cost = pd.read_csv(f"./Data/properties_csv/{year}/Total_Cost.csv")
    df_cost = df_cost[['LSOA_code','Annual cost']]
    if year in ['2020','2021','2022']:
        df_fp = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
        df_fp = df_fp[df_fp['Proportion of households fuel poor (%)'] > 0.001]
        df_fp_ref = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
    else:
        df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_fp_ref = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")

    df_elec, df_gas, df_cop, df_cost, df_fp, _ = sort_muiltiple_df(df_elec, df_gas, df_cop, df_cost, df_fp, df_fp_ref)

    cop = df_cop.iloc[:, 1:13].values
    elec_consump = df_elec.iloc[:, 1:13].values
    gas_consump = df_gas.iloc[:, 1:13].values
    fp = df_fp.iloc[:, 1].values
    fp = fp*100

    delta_gas_array = delta_gas(uptake, gas_consump)
    delta_elect_array = delta_elec(delta_gas_array, cop) 

    resulted_elec_consump = elec_consump + delta_elect_array
    resulted_gas_consump = gas_consump - delta_gas_array

    cost_elec = read_from_web_price_elec(year=year)
    cost_gas = read_from_web_price_gas(year=year)

    final_cost = cost_elec * resulted_elec_consump + cost_gas * resulted_gas_consump
    final_cost = np.sum(final_cost, axis=1)

    original_cost = df_cost.iloc[:, 1:13].values
    original_cost = original_cost.reshape(final_cost.shape)

    delta_cost = abs(final_cost - original_cost) 
    min_deltaC_nth_percentile = 1
    max_deltaC_nth_percentile = 99
    min_fuel_poverty = 0
    max_fuel_poverty = 0.2

    min_fp = min_fuel_poverty * 100
    max_fp = max_fuel_poverty * 100
    min_dc = np.nanpercentile(delta_cost,min_deltaC_nth_percentile)
    max_dc = np.nanpercentile(delta_cost,max_deltaC_nth_percentile)

    index = calculate_inequality_index(fp, delta_cost, min_fp, max_fp, min_dc, max_dc)

    df_fp['Inequality Index'] = index
    df_fp = df_fp[['LSOA_code','Inequality Index']]
    df_fp['LSOA_code'] = df_fp['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))

    # plot_geodistribution_with_cities(f'Inequality Index @{uptake*100}% Uptake @{year}',
    #                                  f'Inequality Index @{uptake*100}% Uptake in {year}',
    #                                  df_fp,
    #                                  1)
    final.append(index)

#plot_kernel_density(final[0],f'Inequality index @{uptake*100}% Uptake 2022','Inequality index')

scatter_plot(f'Inequality index @{uptake*100}% Uptake 2019 vs 2022 ',final[0], final[1], 'Inequality index @2019 ','Inequality index @2022 ',f'Inequality index @{uptake*100}% Uptake\n 2019 vs 2022')
'''
# ----------------------------------------------------------------
# GIF option
'''
    final = np.array(final)
    # Initialize the figure and axes
    fig, ax = plt.subplots()

    # Set the axes limits
    ax.set_xlim(0, len(index))
    ax.set_ylim(-1, 1)  # Assuming data values range from 0 to 1

    # Create an empty scatter plot
    scatter = ax.scatter([], [], s=5, c='black')
    
    # Initialize the scatter plot with the starting positions of the points
    scatter.set_offsets(np.column_stack((np.zeros_like(final[0]), final[0])))
    # Initialize the title
    title = ax.set_title('Price Ratio: {}'.format(ratio_list[0]))
    
    
    # Function to update the plot for each frame of the animation
    def update(frame):

        x = np.arange(len(final[frame]))  # 当前帧的 x 坐标
        y = final[frame]  # 当前帧的 y 坐标

        # 更新点的位置
        scatter.set_offsets(np.column_stack((x, y)))
        ax.set_title(f'Price Ratio: {ratio_list[frame]}')  # Set the title to the current year
       
        return scatter, title


    # Create the animation
    animation = FuncAnimation(fig, update, frames=len(ratio_list), interval=1000, blit=False)

    # Show the plot
    # plt.show()
    save_gif('GIF_ratio_vs_index',animation, 1)
'''
# Change price only
def find_input_for_result(target_result, year, uptake,df_cop):
    # Initial guess for the input variable
    x = 4.25

    # Set the maximum number of iterations
    max_iterations = 100
    
    # Set the tolerance for convergence
    tolerance = 1e-6

    # Iterate until the target result is reached or the maximum iterations are reached
    for _ in range(max_iterations):

        _, result = calculate_change_of_cost(year, x, uptake,df_cop)
        print([x, result])

        # Check if the current result is close enough to the target result
        if abs(result - target_result) < tolerance:

            return x, result

        # Calculate the derivative of the function
        _, result_plus = calculate_change_of_cost(year, x+ tolerance, uptake,df_cop)
        _, result_minus = calculate_change_of_cost(year, x- tolerance, uptake,df_cop)
        derivative = (result_plus - result_minus) / (2 * tolerance)

        # Update the value of x using the Newton-Raphson formula
        x = x - result / derivative

    # Return None if the target result is not found within the maximum iterations
    return None


uptake = 1
df_cop = pd.read_csv("./Data/properties_csv/COP_mean.csv")

final = []
red_area_list = []
very_red_area_list = []
ratio_list = np.arange(0.8, 1.3, 0.05).tolist()
#ratio_list = [0.8, 0.9, 1, 1.1, 1.2]
ratio_list = np.round(ratio_list, decimals=2)


elec_increase_list = np.arange(-0.1696/2, 0.1696*3/2, 0.1696/10).tolist()
gas_increase_list = np.arange(-0.037/2, 0.037*3/2, 0.037/10).tolist()
# elec_increase_list = [0]
# gas_increase_list = [0]
elec_increase_list = np.round(elec_increase_list, decimals=5)
gas_increase_list = np.round(gas_increase_list, decimals=5)


# year_list = ['2019','2020', '2021']
year_list = ['2019']

#ratio, result = find_input_for_result(0, year, uptake, df_cop)
for year in year_list:
    # for ratio in ratio_list:
    if year in ['2020','2021','2022']:
        df_fp = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
        df_fp = df_fp[df_fp['Proportion of households fuel poor (%)'] > 0.001]
        df_ref = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
        df_ref = df_ref[['LSOA_code']]
        df_fp['LSOA_code'] = df_fp['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    else:
        df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_ref = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_ref = df_ref[['LSOA_code']]
        df_fp['LSOA_code'] = df_fp['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        
    for ratio in ratio_list:
    # for i in tqdm(range(len(elec_increase_list))):
        # red_area_list = []
        # very_red_area_list = []
        # for j in range(len(gas_increase_list)):
            # ratio = 1
            # elec_increase = elec_increase_list[i]
            # gas_increase = gas_increase_list[j]

            fp = df_fp.iloc[:, 1].values
            fp = fp*100
            delta_cost, _ = calculate_change_of_cost(year, ratio, uptake, df_cop)

        
            min_deltaC_nth_percentile = 1
            max_deltaC_nth_percentile = 99
            min_fuel_poverty = 0
            max_fuel_poverty = 0.2

            min_fp = min_fuel_poverty * 100
            max_fp = max_fuel_poverty * 100
            min_dc = np.nanpercentile(delta_cost,min_deltaC_nth_percentile) 
            max_dc = np.nanpercentile(delta_cost,max_deltaC_nth_percentile)
            # min_dc = 26.197 
            # max_dc = 124.262 

            index = calculate_inequality_index(fp, delta_cost, min_fp, max_fp, min_dc, max_dc)
            df_ref[f'{ratio}'] = index

    # df_ref[f'Change'] = df_ref[f'{ratio_list[1]}'] - df_ref[f'{ratio_list[0]}'] 
    # df_ref = df_ref[['LSOA_code','Change']]

            df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    plot_box_and_whisker_for_muiltiple_entities(f'index @{year} @price ratio as 4',
                                                f'index @{year} \n  @price ratio as 4 \n ',df_ref, pdf = False)
        
        # (f'Inequality Index @{year}','Inequality index',df_ref, pdf = False)
        # (f'Change of cost @{year}','Change of Fuel Cost \n (£/month/household)',df_ref, pdf = False)

            # red_area = round(np.sum(index > 0)/len(index)*100,3)
            # very_red_area = round(np.sum(index > 0.5)/len(index)*100,3)
            
            # red_area = delta_cost[0]
            # very_red_area = delta_cost[1000]
            # red_area = np.nanmin(delta_cost)
            # very_red_area = np.nanmax(delta_cost)
            # red_area_list.append(red_area)
            # very_red_area_list.append(very_red_area)
        
        # final.append(red_area_list)

# x_grid, y_grid = np.meshgrid(np.array(elec_increase_list)/cost_elec*100, np.array(gas_increase_list)/cost_gas*100)
# x_grid = np.round(x_grid, decimals=2)
# y_grid = np.round(y_grid, decimals=2)
# three_Dimensional_scatter_plot(x_grid, y_grid, np.array(final),
#                                'Electricity Price change(%)', 'Gas Price change(%)', 'Area index >0(%)',
#                                '3D elec gas price versus inequality')
# plot_line_with_annotations(ratio_list, red_area_list, very_red_area_list ,'X', 
#                            'Minimal change of cost',
#                            'Max change of cost',
#                             'Min & Max change of cost @Ratio 5', False)
# (ratio_list, red_area_list, very_red_area_list ,'ratio', 
#                            'Area index >0',
#                            'Area index >0.5',
#                             'Inequality Index versus Price', False)
# (ratio_list, red_area_list, very_red_area_list ,'ratio', 
#                            'Standard Deviation',
#                            'Median',
#                             'Statistical constants versus Price', False)
# (ratio_list, red_area_list, very_red_area_list ,'ratio', 
#                            '25th percentile',
#                            '75th percentile',
#                             'Inequality Index percentile versus Price Ratio', False)
# (ratio_list, red_area_list, very_red_area_list ,'ratio', 
# 'minimal change of cost P1 (£/year/household)',
# 'maximum change of cost P99 (£/year/household)',
#                    'min&max change of cost versus Price Ratio', False)


        #print(f'When Ratio: {ratio}, Area index >0 : {red_area}%, Area index >0.5 : {very_red_area}%')

        # 'ratio', 
        #                    'minimal change of cost P1 (£/year/household)','maximum change of cost P99 (£/year/household)',
        #                    'min&max change of cost versus Price Ratio', False)

        # df_fp['Inequality Index'] = index
        # df_fp = df_fp[['LSOA_code','Inequality Index']]
        # df_fp['LSOA_code'] = df_fp['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    
    # df_elec = pd.read_csv(f'./Data/properties_csv/{year}/Elec_Consump_per_house.csv')
    # df_gas = pd.read_csv(f'./Data/properties_csv/{year}/Gas_Consump_per_house.csv')
    # df_elec, df_gas, df_ref = sort_muiltiple_df(df_elec, df_gas, df_ref)
    # sum = df_elec.iloc[1:13].sum(axis=1, skipna=True)
    # df_elec['sum'] = sum
    # df_elec = df_elec[['LSOA_code', 'sum']]
    # df_elec['LSOA_code'] = df_elec['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
            # plot_geodistribution_with_cities (f'change of fuel cost @{year} X @{ratio} @price ratio as 4',
            #                                     f'change of fuel cost @{year} X @{ratio} @price ratio as 4',
            #                                     df_ref,
            #                                     1,
            #                                     False)
    # (f'change of inequality index @{year} @{ratio_list[0]} & @{ratio_list[1]}',
    #                                 f'change of inequality index @{year} @{ratio_list[0]} & @{ratio_list[1]}',
    #                                 df_ref,
    #                                 1,
    #                                 False)
        # (f'change of fuel cost @{year} @{ratio}',
        #                                 f'change of fuel cost @{year} @{ratio}',
        #                                 df_fp,
        #                                 1,
        #                                 False)
    
    #plot_kernel_density(index,f'Inequality Index density @{ratio} price ratio @ Full uptake @{year}','Inequality index',f'price ratio = {ratio}')

#scatter_plot(f'Inequality index @{uptake*100}% Uptake 2019 vs 2022 ',final[0], final[1], 'Inequality index @2019 ','Inequality index @2022 ',f'Inequality index @{uptake*100}% Uptake\n 2019 vs 2022')

# ----------------------------------------------------------------
# Get previous price ratio
'''
year_list = ['2017','2018','2019','2020','2021','2022']
price_ratio_list = []
elec_price_list = []
gas_price_list = []
for year in year_list:
    elect_price = read_from_web_price_elec(year)
    gas_price = read_from_web_price_gas(year)
    price_ratio = round(elect_price/gas_price,3)
    price_ratio_list.append(price_ratio)
    elec_price_list.append(elect_price)
    gas_price_list.append(gas_price)

plot_line_with_annotations(year_list,price_ratio_list,elec_price_list,gas_price_list, 
                           'year','Price ratio','Fuel Price (£/kWh)','Price ratio in 2017-2022', pdf = False
                           )
'''
'''
    #convert 2021 temp data to tensor
    dict_temp_2021 = call_pickle('./Data/pickles/temp_result_dict in function read_all_temperature_2021_reformatted')
    LSOA_index, results_tensor = convert_to_tensor(dict_temp_2021)
    print(results_tensor)
    '''