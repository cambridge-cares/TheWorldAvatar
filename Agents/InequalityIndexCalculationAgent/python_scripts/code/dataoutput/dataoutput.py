################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data output to generate figures
# in various format
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier


from code.dataretrieval.dataretrival import *
from code.datamodel.functionality import *
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
import seaborn as sns
import matplotlib.pyplot as plt

ONS_ID = "http://statistics.data.gov.uk/id/statistical-geography/"

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
        df_fp = df_fp[df_fp['Proportion of households fuel poor (%)'] > 0.01]
        df_fp_ref = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
        df_elec = pd.read_csv(f'./Data/properties_csv/2020/Elec_Consump_per_house.csv')
        df_gas = pd.read_csv(f'./Data/properties_csv/2020/Gas_Consump_per_house.csv')
    else:
        df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_fp_ref = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")

    df_elec, df_gas, df_cop, df_fp, _ = sort_muiltiple_df(df_elec, df_gas, df_cop, df_fp, df_fp_ref)

    cop = df_cop.iloc[:, 1:13].values
    gas_consump = df_gas.iloc[:, 1:13].values
    fp = df_fp.iloc[:, 1].values
    fp = fp*100

    delta_gas_array = delta_gas(uptake, gas_consump)
    delta_elect_array = delta_elec(delta_gas_array, cop) 

    # resulted_elec_consump = elec_consump + delta_elect_array
    # resulted_gas_consump = gas_consump - delta_gas_array

    # cost_elec = read_from_web_price_elec(year=year)
    cost_gas =  read_from_web_price_gas(year=year) 
    # cost_gas =  cost_elec / ratio
    cost_elec =  cost_gas * ratio

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
        fig = plt.figure(figsize=(4,3.5))
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
    df_geo = call_pickle('./Data/pickles/geometry')

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
    # axs.set_title(f"{title}",y=1.08)
    cax1.ticklabel_format(axis="y", style="sci", scilimits=(0,0))  
    
    # Store the figures
    save_figures(title, False)

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

    def normalization_a(val_values, cb_scale):
        '''
        Provide normalization for color bar
        Arguments:
                    *** Please use this function with plot_geodistribution***
        val_values: NDArray which have NO nan data
        cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
                    in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                    I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
        '''
        iqr = st.iqr(val_values)
        q1,q3 = st.mstats.idealfourths(val_values)
        # bottom = q1 - cb_scale * iqr
        # top = q3 + cb_scale * iqr
        bottom = -1
        top = 1
        divnorm = cl.Normalize(vmin=bottom, vmax=top)

        return divnorm
   
    # Get Geospatial shapes:
    df_geo = call_pickle('./Data/pickles/geometry')

    # Make a copy of df_in
    df = copy.deepcopy(df_in)
    
    # Revising the data
    df_geo, val_values =data_treatment(df_geo, title, df)
    
    # Initilize the graph
    axs, cax, color_theme, UK_gdf = basic_settings(df_geo)
    
    # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
    divnorm = normalization_a(val_values, cb_scale)

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

def plot_multiple_geodistribution(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
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
    df_geo = call_pickle('./Data/pickles/geometry')
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
    
    save_figures(title, pdf)

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
    df_geo = call_pickle('./Data/pickles/geometry')
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

def plot_normalized_line_with_annotations(x_values, y1_values, y2_values, x_label, y_legend, y1_label, y2_label,title, pdf = False):
    
    y_min = min(np.min(y1_values), np.min(y2_values))
    y_max = max(np.max(y1_values), np.max(y2_values))

    y_values1_norm = (y1_values - np.min(y1_values)) / np.min(y1_values)
    y_values2_norm = (y2_values- np.min(y2_values)) / np.min(y2_values)
    
    # Plot the lines
    plt.plot(x_values, y_values1_norm,  marker='o', linestyle='-', color='blue', label= y1_label)
    plt.plot(x_values, y_values2_norm, marker='s', linestyle='-', color='red', label= y2_label)

    # Set labels and title
    plt.xlabel(x_label)
    plt.ylabel(y_legend)

    # Add legend
    plt.legend(fontsize='small')

    x_ticks = np.linspace(np.min(x_values), np.max(x_values), num = 5)
    x_ticks = np.around(x_ticks, decimals=1)
    plt.xticks(ticks=x_ticks)

    y_ticks = np.linspace(min(np.min(y_values1_norm), np.min(y_values2_norm)),
                          max(np.max(y_values1_norm), np.max(y_values2_norm)), num = 6)
    y_ticks = np.around(y_ticks, decimals=1)
    plt.yticks(ticks=y_ticks)

    save_figures(title, pdf)

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
    line2, =ax2.plot(x_values, y2_values, marker='s', linestyle='-', color='red', label = y2_label)
    ax2.set_ylabel(y2_label, color='black')
    ax2.tick_params(axis='y', labelcolor='black')

    # Combine the legend from both subplots
    lines = [line1, line2]
    labels = [line.get_label() for line in lines]
    ax1.legend(lines, labels, loc='upper left', fontsize='small')

    # Set x-axis label and title
    ax1.set_xlabel(x_label)

    save_figures(title, pdf)

def plot_line_with_annotations_single_y_axis(x_values, y1_values, y2_values, x_label, y1_label, y2_label, y_legend, title, pdf = False):
    # Create figure and first subplot (left y-axis)
    fig, ax1 = plt.subplots()

    # Plot the first line (y1_values) with left y-axis
    line1, =ax1.plot(x_values, y1_values, marker='o', linestyle='-', color='blue', label = y1_label)
    ax1.set_ylabel(y_legend, color='black')
    ax1.tick_params(axis='y', labelcolor='black')

    # Plot the second and third lines (y2_values, y3_values) with right y-axis
    line2, =ax1.plot(x_values, y2_values, marker='s', linestyle='-', color='red', label = y2_label)

    # Combine the legend from both subplots
    lines = [line1, line2]
    labels = [line.get_label() for line in lines]
    ax1.legend(lines, labels, loc='upper left', fontsize='small')

    # Set x-axis label and title
    ax1.set_xlabel(x_label)
    
    save_figures(title, pdf)

# ------------------------ Scatter Plot --------------------------------- #
def scatter_plot(filename, x_values:np.array, y_values:np.array, x_label:str, y_label: str, title: str):
    
    # Plot scatter plot
    plt.scatter(x_values, y_values, s=1, c='black')

    # Plot y=x line
    x_line = np.linspace(min(x_values.flatten()), max(x_values.flatten()), 100)
    plt.plot(x_line, x_line, 'r--')

    # Set labels and title
    plt.xlabel(x_label)
    plt.ylabel(y_label)
    
    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(x_values) - min(x_values)
    y_range = max(y_values) - min(y_values)
    max_range = max(x_range, y_range)
    min_value = min(min(x_values), min(y_values)) - margin * max_range
    max_value = max(max(x_values), max(y_values)) + margin * max_range

    # Set the same range for both axes with margin
    plt.xlim(min_value, max_value)
    plt.ylim(min_value, max_value)
    # plt.xlim(-4, 2)
    # plt.ylim(-4, 2)

    save_figures(filename, False)

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

# ---------------------- Utility function ------------------------------- #
def calculate_delta_cost(ratio, delta_elect_array, delta_gas_array, sum = True):
    
    cost_gas =  read_from_web_price_gas('2019') 
    # cost_gas =  cost_elec / ratio
    cost_elec =  cost_gas * ratio

    delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
    if sum == True:
        delta_cost = np.sum(delta_cost, axis=1)

    return delta_cost

def calculate_current_cost(ratio, elec_consump, gas_consump):
    
    cost_gas =  read_from_web_price_gas('2019') 
    # cost_gas =  cost_elec / ratio
    cost_elec =  cost_gas * ratio

    cost = cost_elec * elec_consump + cost_gas * gas_consump
    cost = np.sum(cost, axis=1)

    return cost

def calculate_index_original_tom(fp, delta_cost):
    
    min_deltaC_nth_percentile = 1
    max_deltaC_nth_percentile = 99
    min_fuel_poverty = 0
    max_fuel_poverty = 0.2

    min_fp = min_fuel_poverty * 100
    max_fp = max_fuel_poverty * 100
    # min_dc = 26.197 
    # max_dc = 124.262 
    min_dc = np.nanpercentile(delta_cost,min_deltaC_nth_percentile) 
    max_dc = np.nanpercentile(delta_cost,max_deltaC_nth_percentile)
    index = calculate_inequality_index(fp, delta_cost, min_fp, max_fp, min_dc, max_dc)

    return index

def calculate_index_normalization_variation(fp, delta_cost, variation):
    '''
    variation = 'original', 'std', 'quartile', '',  '',  '', 
    '''
    fp_norm = calculate_normalized_index(fp,0,20)

    if variation == 'original':
        min_deltaC_nth_percentile = 1
        max_deltaC_nth_percentile = 99
        min_dc = np.nanpercentile(delta_cost,min_deltaC_nth_percentile) 
        max_dc = np.nanpercentile(delta_cost,max_deltaC_nth_percentile)
        delta_cost = calculate_normalized_index(delta_cost, min_dc, max_dc)
        index = delta_cost*fp_norm

    if variation == 'std':
        delta_cost = (delta_cost) / np.nanstd(delta_cost)
        index = delta_cost*fp_norm

    if variation == 'quartile':
        
        delta_cost = delta_cost / (np.nanpercentile(delta_cost, 75) - np.nanpercentile(delta_cost, 25))
        index = delta_cost*fp_norm

    return index

def calculate_standarizationed_index(var):
    
    mean = np.nanmean(var)
    median = np.nanmedian(var)
    std = np.nanstd(var)

    index = (var - median) / std

    return index

def calculate_normalized_temp(var, min, max, coefficient):

    result = coefficient *((var-min)/(max-min))

    # result = 1

    return result

def calculate_normalized_gas(var, min, max, coefficient=1):

    result = coefficient *((var-min)/(max-min))

    # result = 1

    return result

def calculate_normalized_fp(var, min, max, coefficient):

    result = coefficient *((var-min)/(max-min))

    # result = 1

    return result

def calculate_normalized_index(var, min, max):

    result = ((var-min)/(max-min))

    return result

def calculate_normalized_index_median_zero(var):

    median = np.nanmedian(var)
    diff = max(abs(var - median))
    result = ((var - median) / diff) * 2 - 1

    return result

def calculate_index_all_normalization(cost_ratio, fp):

    min_fuel_poverty = 0
    max_fuel_poverty = 0.2

    min_fp = min_fuel_poverty * 100
    max_fp = max_fuel_poverty * 100
    
    # min_cost_ratio = np.nanpercentile(cost_ratio, 1)
    # max_cost_ratio = np.nanpercentile(cost_ratio, 99)
    min_cost_ratio = np.nanmax(cost_ratio)
    max_cost_ratio = np.nanmin(cost_ratio)

    a = ((fp-min_fp)/(max_fp-min_fp))

    b = (2*(cost_ratio-min_cost_ratio)/(max_cost_ratio-min_cost_ratio))-1

    inequality_index = a*b
    
    # return np.around(inequality_index,decimals=3)
    return np.around(inequality_index, decimals=3)

def calcualte_index_all_standardrization(cost, fp):

    a = calculate_standarizationed_index(fp)

    b = calculate_standarizationed_index(cost)

    inequality_index = a*b
    
    return np.around(inequality_index, decimals=3)



