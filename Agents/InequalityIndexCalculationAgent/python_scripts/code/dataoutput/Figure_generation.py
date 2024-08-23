from code.dataoutput.dataoutput import *
from matplotlib.ticker import MaxNLocator
import math
from matplotlib.colors import LinearSegmentedColormap
from scipy.stats import gaussian_kde
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import geopandas as gpd
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
from mpl_toolkits.axes_grid1.inset_locator import mark_inset
import seaborn as sb 
import pandas as pd
import numpy as np
import copy
import scipy.stats as st
import matplotlib.colors as cl 
import csv
import seaborn as sns
import matplotlib.pyplot as plt


# Specify a customised colorbar here, by inserting the RGB color
colors_rgb = [
    (57, 81, 162),
    (114, 170, 207),
    (202, 232 , 242),
    (254, 251 , 186),
    (253, 185 , 107),
    (236, 93 , 59),
    (168, 3 , 38)
]
colors_normalized = [(r/255, g/255, b/255) for r, g, b in colors_rgb]
positions = np.linspace(0, 1, len(colors_normalized))
cmap_dict = {'red': [], 'green': [], 'blue': []}

# Populate the dictionary with the normalized RGB values and positions
for pos, color in zip(positions, colors_normalized):
    r, g, b = color
    cmap_dict['red'].append((pos, r, r))
    cmap_dict['green'].append((pos, g, g))
    cmap_dict['blue'].append((pos, b, b))

# Create the custom colormap using LinearSegmentedColormap
cmap_name = 'custom_cmap'
custom_cmap = LinearSegmentedColormap(cmap_name, cmap_dict)
## From now on you can specify the color theme as 'custom_cmap' If you wish to use custom colorbar 
## If not you can just specify the color theme as you wish

# Specify the text size for the figure generated
plt.rcParams['font.size'] = 12

# Define new list
final = []
red_area_list = []
very_red_area_list = []

ratio_list = [3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5]
ratio_list = np.round(ratio_list, decimals=2)

# retrieveing data
# specify the key variable here
uptake = 1
year = '2019'
gas_price_2020 = read_from_web_price_gas('2020')
elec_price_2020 = read_from_web_price_elec('2020')
ratio_2020 = elec_price_2020 / gas_price_2020 # 4.84

gas_price_2022 = read_from_web_price_gas('2022')
elec_price_2022 = read_from_web_price_elec('2022')
ratio_2022 = elec_price_2022 / gas_price_2022 # 3.74

gas_price_2019 = read_from_web_price_gas('2019')
elec_price_2019 = read_from_web_price_elec('2019')
ratio_2019 = elec_price_2019 / gas_price_2019 # 4.49

df_temp = pd.read_csv(f"./Data/properties_csv/2019/Temperature_mean.csv")
df_temp_min = pd.read_csv(f"./Data/properties_csv/2019/Temperature_min.csv")
df_cop = pd.read_csv(f"./Data/properties_csv/2019/COP_mean.csv")
df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
df_fp = df_fp[df_fp['Proportion of households fuel poor (%)'] > 0.001]
df_fp_2022 = pd.read_csv(f"./Data/properties_csv/2020/fuel_poverty.csv")
df_ref = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
df_ref = df_ref[['LSOA_code']]
df_elec = pd.read_csv(f'./Data/properties_csv/{year}/Elec_Consump_per_house.csv')
df_gas = pd.read_csv(f'./Data/properties_csv/{year}/Gas_Consump_per_house.csv')

df_meter = pd.read_excel(f'./Data/source_data/LSOA_domestic_gas_2010-21.xlsx', sheet_name=year, skiprows=4)
df_meter = df_meter[['LSOA code','Number\nof meters\n']]
df_meter['LSOA_code']=df_meter['LSOA code']

# base case calculation
df_meter, df_elec, df_gas, df_cop, df_fp, _, df_temp, df_temp_min = sort_muiltiple_df(df_meter, df_elec, df_gas, df_cop, df_fp, df_ref, df_temp, df_temp_min)

temp = df_temp.iloc[:, 1:13].values
min_temp = df_temp_min.iloc[:, 1:13].values
cop = df_cop.iloc[:, 1:13].values
meters = df_meter.iloc[:, 1].values

gas_consump = df_gas.iloc[:, 1:13].values
elec_consump = df_elec.iloc[:, 1:13].values
fp = df_fp.iloc[:, 1].values
fp = fp*100
fp_2020 = df_fp_2022.iloc[:, 1].values
fp_2020 = fp_2020*100
temp_mean = np.mean(temp, axis=1)

delta_gas_array = delta_gas(uptake, gas_consump)
delta_elect_array = delta_elec(delta_gas_array, cop) 

# Here I provided ways to generate many figures, which may be used or not used in the preprints
# Hope that will be meaningful for you, not only in reproducing the figures
# But also other formats of visualisation
def scatter_plot_base_case_versus_magitude():

    def calculate_delta_cost_on_scale(ratio, scale, delta_elect_array, delta_gas_array):
    
        cost_gas =  read_from_web_price_gas(year) * scale
        # cost_gas =  cost_elec / ratio
        cost_elec =  cost_gas * ratio

        delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
        delta_cost = np.sum(delta_cost, axis=1)

        return delta_cost
    
    def scatter_plot_a(filename, x_values:np.array, y_values:np.array, x_label:str, y_label: str, title: str):
    
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

        save_figures(f'./{filename}', False)
    
    ratio = 4
    scaling_factor_list = [0.5, 2]
    for scaling_factor in scaling_factor_list:
        delta_cost = calculate_delta_cost_on_scale(ratio, scaling_factor, delta_elect_array, delta_gas_array)
        index = calculate_index_original_tom(fp, delta_cost)
        final.append(index)
    
    scatter_plot_a(f'Inequality index @two scaling factor',
                final[0], final[1], 'Inequality index @scaling factor=0.5 ','Inequality index @scaling factor=2 ',
                f'Inequality index @two scaling factor')

def scatter_plot_inequality_20192022_as_xyaxis_fp():
    
    # ratio_list = [ratio_2019, ratio_2022]
    ratio_list = [5, 3.25]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        final.append(index)

    ratio_list = [ratio_2019, 3.25, 5, ratio_2022]
    cost_ratio_list = []
    sigma_term_list = []
    fp_norm = calculate_normalized_index(fp,0,20)
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost * 100
        cost_ratio_list.append(cost_ratio)
        # index = calculate_standarizationed_index(delta_cost)
        std = np.nanstd(delta_cost)
        sigma_term = delta_cost/std
        sigma_term_list.append(sigma_term)
    # Create a figure and axes
    fig, ax = plt.subplots(figsize = (5,5.5))

    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(final[0]) - min(final[0])
    y_range = max(final[1]) - min(final[1])
    max_range = max(x_range, y_range)
    min_value = min(min(final[0]), min(final[1])) - margin * max_range
    max_value = max(max(final[0]), max(final[1])) + margin * max_range

    margin = 0.1  # adjust the margin as desired
    min_value = -10 + margin * 20
    max_value = 10 - margin * 20
    cax = fig.add_axes([0.25, 0.14, 0.6, 0.03])
    to_be_zipped = [fp, 35, 'Proportion of fuel poverty (2019) (%)', 5, 35]
    
    limit = to_be_zipped[1]
    # Plot scatter plot
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    
    cbar4 = plt.colorbar(scatter2, shrink = 0.6, orientation='horizontal', cax=cax)
    cbar4.set_label(to_be_zipped[2])
    ticks = [5, 15, 25, 35]

    ticks_label = [str(tick) for tick in ticks[1:-1]]
    ticks_label.insert(0,f'< {ticks[0]}')
    ticks_label.append(f'> {ticks[-1]}')
    cbar4.set_ticks(ticks)  
    cbar4.set_ticklabels(ticks_label)

    # Plot y=x line
    x_line = np.linspace(-10, 10, 100)
    ax.plot(x_line, x_line, 'r--')

    # Set labels and title
    ax.xaxis.set_ticks_position('top')
    ax.set_xlabel('Inequality index @price ratio=5 (-)')
    # Move the x-axis ticks and label to the top
    ax.xaxis.set_label_position('top')
    ax.set_ylabel('Inequality index @price ratio=3.25 (-)')

    # Set the same range for both axes with margin
    # ax.set_xlim(-10, 10)
    # ax.set_ylim(-10, 10)
    ax.set_xlim(0, 10)
    ax.set_xticks([0,2,4,6,8,10])
    ax.set_xticklabels(['','2','4','6','8','10'])
    ax.set_yticks([-10, -8, -6, -4, -2, 0])
    ax.set_yticklabels(['-10','-8','-6','-4','-2',''])
    ax.text(-0.4, 0.3, '0', fontsize=12, color='black')
    ax.set_ylim(-10, 0)

    # Set both x and y axes to have only 5 tickers
    # ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
    # ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.subplots_adjust(left=0.13, right=0.97, top = 0.92, bottom=0.21)

    try:
        save_figures(f'./Inequality index 2019 vs 2022 with {to_be_zipped[2][:7]}', False)
    except:
        save_figures(f'./Inequality index 2019 vs 2022 placeholder', False)

def scatter_plot_inequality_20192022_as_xyaxis_temperature():
    
    # ratio_list = [ratio_2019, ratio_2022]
    ratio_list = [5, 3.25]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        final.append(index)

    ratio_list = [ratio_2019, 3.25, 5, ratio_2022]
    cost_ratio_list = []
    sigma_term_list = []
    fp_norm = calculate_normalized_index(fp,0,20)
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost * 100
        cost_ratio_list.append(cost_ratio)
        # index = calculate_standarizationed_index(delta_cost)
        std = np.nanstd(delta_cost)
        sigma_term = delta_cost/std
        sigma_term_list.append(sigma_term)
    # Create a figure and axes
    fig, ax = plt.subplots(figsize = (5,5.5))

    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(final[0]) - min(final[0])
    y_range = max(final[1]) - min(final[1])
    max_range = max(x_range, y_range)
    min_value = min(min(final[0]), min(final[1])) - margin * max_range
    max_value = max(max(final[0]), max(final[1])) + margin * max_range

    margin = 0.1  # adjust the margin as desired
    min_value = -10 + margin * 20
    max_value = 10 - margin * 20
    cax = fig.add_axes([0.25, 0.14, 0.6, 0.03])
    to_be_zipped = [temp_mean, 12,'Average air temperature (2019) (\u00b0C)',9, 12]
    
    limit = to_be_zipped[1]
    # Plot scatter plot
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    
    cbar4 = plt.colorbar(scatter2, shrink = 0.6, orientation='horizontal', cax=cax)
    cbar4.set_label(to_be_zipped[2])
    ticks = [9, 10, 11, 12]

    ticks_label = [str(tick) for tick in ticks[1:-1]]
    ticks_label.insert(0,f'< {ticks[0]}')
    ticks_label.append(f'> {ticks[-1]}')
    cbar4.set_ticks(ticks)  
    cbar4.set_ticklabels(ticks_label)

    # Plot y=x line
    x_line = np.linspace(-10, 10, 100)
    ax.plot(x_line, x_line, 'r--')

    # Set labels and title
    ax.xaxis.set_ticks_position('top')
    ax.set_xlabel('Inequality index @price ratio=5 (-)')
    # Move the x-axis ticks and label to the top
    ax.xaxis.set_label_position('top')
    ax.set_ylabel('Inequality index @price ratio=3.25 (-)')

    # Set the same range for both axes with margin
    # ax.set_xlim(-10, 10)
    # ax.set_ylim(-10, 10)
    ax.set_xlim(0, 10)
    ax.set_xticks([0,2,4,6,8,10])
    ax.set_xticklabels(['','2','4','6','8','10'])
    ax.set_yticks([-10, -8, -6, -4, -2, 0])
    ax.set_yticklabels(['-10','-8','-6','-4','-2',''])
    ax.text(-0.4, 0.3, '0', fontsize=12, color='black')
    ax.set_ylim(-10, 0)

    # Set both x and y axes to have only 5 tickers
    # ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
    # ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.subplots_adjust(left=0.13, right=0.97, top = 0.92, bottom=0.21)

    try:
        save_figures(f'./Inequality index 2019 vs 2022 with {to_be_zipped[2][:7]}', False)
    except:
        save_figures(f'./Inequality index 2019 vs 2022 placeholder', False)

def scatter_plot_inequality_20192022_as_xyaxis_change_in_cost():
    
    # ratio_list = [ratio_2019, ratio_2022]
    ratio_list = [5, 3.25]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        final.append(index)

    ratio_list = [ratio_2019, 3.25, 5, ratio_2022]
    cost_ratio_list = []
    sigma_term_list = []
    fp_norm = calculate_normalized_index(fp,0,20)
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost * 100
        cost_ratio_list.append(cost_ratio)
        # index = calculate_standarizationed_index(delta_cost)
        std = np.nanstd(delta_cost)
        sigma_term = delta_cost/std
        sigma_term_list.append(sigma_term)
    # Create a figure and axes
    fig, ax = plt.subplots(figsize = (5,5.5))

    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(final[0]) - min(final[0])
    y_range = max(final[1]) - min(final[1])
    max_range = max(x_range, y_range)
    min_value = min(min(final[0]), min(final[1])) - margin * max_range
    max_value = max(max(final[0]), max(final[1])) + margin * max_range

    margin = 0.1  # adjust the margin as desired
    min_value = -10 + margin * 20
    max_value = 10 - margin * 20
    cax = fig.add_axes([0.25, 0.14, 0.6, 0.03])
    to_be_zipped = [cost_ratio_list[3], -0.5, 'Normalised estimated change in\nannual household fuel cost, $\Delta\hat{C}$ (2022) (%)', -2, 2]
    
    limit = to_be_zipped[1]
    # Plot scatter plot
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    
    cbar4 = plt.colorbar(scatter2, shrink = 0.6, orientation='horizontal', cax=cax)
    cbar4.set_label(to_be_zipped[2])
    ticks = [-2, -1, 0, 1, 2]

    ticks_label = [str(tick) for tick in ticks[1:-1]]
    ticks_label.insert(0,f'< {ticks[0]}')
    ticks_label.append(f'> {ticks[-1]}')
    cbar4.set_ticks(ticks)  
    cbar4.set_ticklabels(ticks_label)

    # Plot y=x line
    x_line = np.linspace(-10, 10, 100)
    ax.plot(x_line, x_line, 'r--')

    # Set labels and title
    ax.xaxis.set_ticks_position('top')
    ax.set_xlabel('Inequality index @price ratio=5 (-)')
    # Move the x-axis ticks and label to the top
    ax.xaxis.set_label_position('top')
    ax.set_ylabel('Inequality index @price ratio=3.25 (-)')

    # Set the same range for both axes with margin
    # ax.set_xlim(-10, 10)
    # ax.set_ylim(-10, 10)
    ax.set_xlim(0, 10)
    ax.set_xticks([0,2,4,6,8,10])
    ax.set_xticklabels(['','2','4','6','8','10'])
    ax.set_yticks([-10, -8, -6, -4, -2, 0])
    ax.set_yticklabels(['-10','-8','-6','-4','-2',''])
    ax.text(-0.4, 0.3, '0', fontsize=12, color='black')
    ax.set_ylim(-10, 0)

    # Set both x and y axes to have only 5 tickers
    # ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
    # ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.subplots_adjust(left=0.13, right=0.97, top = 0.92, bottom=0.21)

    try:
        save_figures(f'./Inequality index 2019 vs 2022 with {to_be_zipped[2][:7]}', False)
    except:
        save_figures(f'./Inequality index 2019 vs 2022 placeholder', False)

def scatter_plot_inequality_20192022_as_xyaxis():
    
    # ratio_list = [ratio_2019, ratio_2022]
    ratio_list = [5, 3.25]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        final.append(index)

    ratio_list = [ratio_2019, 3.25, 5, ratio_2022]
    cost_ratio_list = []
    sigma_term_list = []
    fp_norm = calculate_normalized_index(fp,0,20)
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost * 100
        cost_ratio_list.append(cost_ratio)
        # index = calculate_standarizationed_index(delta_cost)
        std = np.nanstd(delta_cost)
        sigma_term = delta_cost/std
        sigma_term_list.append(sigma_term)
    # Create a figure and axes
    fig, ax = plt.subplots(figsize = (5,5.5))

    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(final[0]) - min(final[0])
    y_range = max(final[1]) - min(final[1])
    max_range = max(x_range, y_range)
    min_value = min(min(final[0]), min(final[1])) - margin * max_range
    max_value = max(max(final[0]), max(final[1])) + margin * max_range

    margin = 0.1  # adjust the margin as desired
    min_value = -10 + margin * 20
    max_value = 10 - margin * 20
    cax = fig.add_axes([0.25, 0.14, 0.6, 0.03])
    # to_be_zipped = [np.sum(elec_consump,axis=1), 5000, 'Electricity Consumption (KWh/Household)',2900, 5000]
    # to_be_zipped = [np.sum(gas_consump,axis=1), 30000, 'Gas Consumption (KWh/Household)',9000, 17000]
    # to_be_zipped = [np.mean(cop, axis=1), 3.5,'Average COP (-)',2.9, 3.5]
    # to_be_zipped = [temp_mean, 12,'Average air temperature (2019) (\u00b0C)',9, 12]
    # to_be_zipped = [fp_2020, 35, 'Proportion of fuel poverty (2019) (%)',5, 35]
    to_be_zipped = [cost_ratio_list[3], -0.5, 'Normalised estimated change in\nannual household fuel cost, $\Delta\hat{C}$ (2022) (%)', -2, 2]
    # to_be_zipped = [cost_ratio_list[0], 9, 'Normalised estimated change in\nannual household fuel cost, $\Delta\hat{C}$ (2019) (%)', 4, 8]
    # to_be_zipped = [sigma_term_list[0], 5, r'$\frac{\Delta C}{\sigma}$ (2019 prices)', 2, 5]
    # to_be_zipped = [sigma_term_list[3], 1, r'$\frac{\Delta C}{\sigma}$ (2022 prices)', -2, 1]
    # to_be_zipped = [fp_norm, 1.2, r'Normalised fuel poverty', 0, 1.5]
    
    # to_be_zipped = [cost_ratio_list[1], -4, '$\Delta\hat{C}$ (price ratio = 3.25) (%)',-8, -4]
    # to_be_zipped = [cost_ratio_list[2], 15, '$\Delta\hat{C}$ (price ratio = 5) (%)',8, 15]
    # to_be_zipped = [cost, 2000, 'Total cost (2019) (£/year/household)',850, 2000]
    # to_be_zipped = [delta_cost, 110, '\u0394C (2019) (£/year/household)',40, 130]
    
    limit = to_be_zipped[1]
    # Plot scatter plot
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data <=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    scatter2 = ax.scatter([data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          [data for data, fp_data in zip(final[1], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          c=[fp_data for data, fp_data in zip(final[0], to_be_zipped[0]) if not math.isnan(fp_data) and fp_data >=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=to_be_zipped[3], vmax=to_be_zipped[4])
    
    cbar4 = plt.colorbar(scatter2, shrink = 0.6, orientation='horizontal', cax=cax)
    cbar4.set_label(to_be_zipped[2])
    # ticks = [2,3,4,5]
    # ticks = [-2, -1, 0, 1]
    # ticks = [0, 1.5]
    ticks = [-2, -1, 0, 1, 2]
    # ticks = [9, 10, 11, 12]
    # ticks = [5, 15, 25, 35]
    # ticks = [4, 5, 6, 7, 8]
    # ticks = [850, 1080, 1310, 1540, 1770, 2000]

    ticks_label = [str(tick) for tick in ticks[1:-1]]
    ticks_label.insert(0,f'< {ticks[0]}')
    ticks_label.append(f'> {ticks[-1]}')
    cbar4.set_ticks(ticks)  
    cbar4.set_ticklabels(ticks_label)

    # Plot y=x line
    x_line = np.linspace(-10, 10, 100)
    ax.plot(x_line, x_line, 'r--')

    # Set labels and title
    ax.xaxis.set_ticks_position('top')
    ax.set_xlabel('Inequality index @price ratio=5 (-)')
    # Move the x-axis ticks and label to the top
    ax.xaxis.set_label_position('top')
    ax.set_ylabel('Inequality index @price ratio=3.25 (-)')

    # Set the same range for both axes with margin
    # ax.set_xlim(-10, 10)
    # ax.set_ylim(-10, 10)
    ax.set_xlim(0, 10)
    ax.set_xticks([0,2,4,6,8,10])
    ax.set_xticklabels(['','2','4','6','8','10'])
    ax.set_yticks([-10, -8, -6, -4, -2, 0])
    ax.set_yticklabels(['-10','-8','-6','-4','-2',''])
    ax.text(-0.4, 0.3, '0', fontsize=12, color='black')
    ax.set_ylim(-10, 0)

    # Set both x and y axes to have only 5 tickers
    # ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
    # ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.subplots_adjust(left=0.13, right=0.97, top = 0.92, bottom=0.21)

    try:
        save_figures(f'./Inequality index 2019 vs 2022 with {to_be_zipped[2][:7]}', False)
    except:
        save_figures(f'./Inequality index 2019 vs 2022 placeholder', False)

def scatter_plot_changeofcost_20192022_as_xyaxis():
    
    ratio_list = [ratio_2019]
    # ratio_list = [5, 3.25]
    final.append(temp.T)
    for ratio in ratio_list:
        cost_gas =  read_from_web_price_gas(year) 
        # cost_gas =  cost_elec / ratio
        cost_elec =  cost_gas * ratio
        delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
        delta_cost = delta_cost.T
        final.append(delta_cost)
    

    label = ['Jan','Feb','Mar',
             'Apr','May','June',
             'July','Aug','Sept',
             'Oct','Nov','Dec']
    for i in range(12):
        # Create a figure and axes
        fig, ax = plt.subplots(figsize = (5,5.5))

        # Calculate the range with margin
        margin = 0.1  # adjust the margin as desired
        x_range = max(final[0][i]) - min(final[0][i])
        y_range = max(final[1][i]) - min(final[1][i])
        max_range = max(x_range, y_range)
        min_value = min(min(final[0][i]), min(final[1][i])) - margin * max_range
        max_value = max(max(final[0][i]), max(final[1][i])) + margin * max_range

        margin = 0.1  # adjust the margin as desired
        min_value = -10 + margin * 20
        max_value = 10 - margin * 20
        ax.scatter(final[0][i], final[1][i], s = 1, marker='o', color='black')

        # Set labels and title
        ax.xaxis.set_ticks_position('top')
        ax.set_xlabel('Temperature (degreeC)')
        # Move the x-axis ticks and label to the top
        ax.xaxis.set_label_position('top')
        ax.set_ylabel('Change of cost @2019 price ratio (-)')

        plt.subplots_adjust(left=0.13, right=0.97, top = 0.92, bottom=0.21)

        try:
            save_figures(f'./{i}. change of cost 2019 with temp at {label[i]}', False)
        except:
            save_figures(f'./change of cost 2019 vs 2022 placeholder', False)

def scatter_plot_temperature_versus_gas_consumption_20192022_as_xyaxis():
    df_fp = pd.concat([df_gas, df_temp_min], axis=1)
    label = ['Jan','Feb','Mar',
             'Apr','May','June',
             'July','Aug','Sept',
             'Oct','Nov','Dec']
    for i in range(12):
        # Create a figure and axes
        fig, ax = plt.subplots(figsize = (9,4))
        df_fp = df_fp.sort_values(by=f'month_{i+1}')
        # Reset the index if needed
        df_fp = df_fp.reset_index(drop=True)
        df_fp= df_fp.dropna()
        df_fp = df_fp.reset_index(drop=True)
        try:
            ax.scatter(df_fp.index, df_fp[f'2020-0{i+1}-01T12:00:00.000Z'].values, s = 1, marker='o', color='black')
        except:
            
            ax.scatter(df_fp.index, df_fp[f'2020-{i+1}-01T12:00:00.000Z'].values, s = 1, marker='o', color='black')
        # Set labels and title
        ax.set_xlabel(f'{label[i]}')
        ax.set_ylabel('Gas consumption (Kwh)')

        plt.subplots_adjust(left=0.13, right=0.97, top = 0.92, bottom=0.21)

        try:
            save_figures(f'./{i}. change of cost 2019 with temp at {label[i]}', False)
        except:
            save_figures(f'./change of cost 2019 vs 2022 placeholder', False)

def scatter_plot_other_var_as_xyaxis_2019_as_color():
    
    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        final.append(index)
    # Create a figure and axes
    fig, ax = plt.subplots(figsize = (5,4))

    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(final[0]) - min(final[0])
    y_range = max(final[1]) - min(final[1])
    max_range = max(x_range, y_range)
    min_value = min(min(final[0]), min(final[1])) - margin * max_range
    max_value = max(max(final[0]), max(final[1])) + margin * max_range

    margin = 0.1  # adjust the margin as desired
    min_value = -10 + margin * 20
    max_value = 10 - margin * 20
    
    to_be_zipped_ele = [np.sum(elec_consump,axis=1), 5000, 'Electricity Consumption (KWh/Household)',2900, 5000]
    to_be_zipped_gas = [np.sum(gas_consump,axis=1), 30000, 'Gas Consumption (KWh/Household)',9000, 17000]
    to_be_zipped_cop = [np.mean(cop, axis=1), 3.5,'Average COP (-)',2.9, 3.5]
    to_be_zipped_temp = [temp_mean, 12,'Average Temperature (\u00b0C)',8, 12]
    to_be_zipped_fp = [fp, 35, 'Fuel Poverty Proportion (%)',5, 35]
    
    zipped_xy = [to_be_zipped_gas,to_be_zipped_fp]

    limit = 4
    # Plot scatter plot
    scatter2 = ax.scatter([data for data, fp_data in zip(zipped_xy[0][0], final[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          [data for data, fp_data in zip(zipped_xy[1][0], final[0]) if not math.isnan(fp_data) and fp_data <=limit], 
                          c=[fp_data for data, fp_data in zip(zipped_xy[0][0], final[0]) if not math.isnan(fp_data) and fp_data <=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=-4, vmax=4)
    scatter2 = ax.scatter([data for data, fp_data in zip(zipped_xy[0][0], final[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          [data for data, fp_data in zip(zipped_xy[1][0], final[0]) if not math.isnan(fp_data) and fp_data >=limit], 
                          c=[fp_data for data, fp_data in zip(zipped_xy[0][0], final[0]) if not math.isnan(fp_data) and fp_data >=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=-4, vmax=4)
    # cbar4 = plt.colorbar(scatter2, shrink = 0.9)
    # cbar4.set_label('Inequality Index (-)')
    # # ticks = [8, 9, 10, 11, 12]
    # ticks = [-4, -2, 0, 2, 4]
    # ticks_label = [str(tick) for tick in ticks[1:-1]]
    # ticks_label.insert(0,f'< {ticks[0]}')
    # ticks_label.append(f'> {ticks[-1]}')
    # cbar4.set_ticks(ticks)  
    # cbar4.set_ticklabels(ticks_label)

    # Set labels and title
    ax.set_xlabel(zipped_xy[0][2])
    ax.set_ylabel(zipped_xy[1][2])

    # Set the same range for both axes with margin
    # ax.set_xlim(-10, 10)
    # ax.set_ylim(0,49)

    # Set both x and y axes to have only 5 tickers
    ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
    ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.subplots_adjust(right=0.95, top = 0.975, bottom=0.15)

    save_figures(f'./Inequality index 2019 with temp and fp', False)

def scatter_plot_other_var_as_xyaxis_2022_as_color():
    
    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        final.append(index)
    # Create a figure and axes
    fig, ax = plt.subplots(figsize = (5,4))

    # Calculate the range with margin
    margin = 0.1  # adjust the margin as desired
    x_range = max(final[0]) - min(final[0])
    y_range = max(final[1]) - min(final[1])
    max_range = max(x_range, y_range)
    min_value = min(min(final[0]), min(final[1])) - margin * max_range
    max_value = max(max(final[0]), max(final[1])) + margin * max_range

    margin = 0.1  # adjust the margin as desired
    min_value = -10 + margin * 20
    max_value = 10 - margin * 20
    
    to_be_zipped_ele = [np.sum(elec_consump,axis=1), 5000, 'Electricity Consumption (KWh/Household)',2900, 5000]
    to_be_zipped_gas = [np.sum(gas_consump,axis=1), 30000, 'Gas Consumption (KWh/Household)',9000, 17000]
    to_be_zipped_cop = [np.mean(cop, axis=1), 3.5,'Average COP (-)',2.9, 3.5]
    to_be_zipped_temp = [temp_mean, 12,'Average Temperature (\u00b0C)',8, 12]
    to_be_zipped_fp = [fp, 35, 'Fuel Poverty Proportion (%)',5, 35]
    
    zipped_xy = [to_be_zipped_ele,to_be_zipped_fp]

    limit = -4
    # Plot scatter plot
    scatter2 = ax.scatter([data for data, fp_data in zip(zipped_xy[0][0], final[1]) if not math.isnan(fp_data) and fp_data >=limit], 
                          [data for data, fp_data in zip(zipped_xy[1][0], final[1]) if not math.isnan(fp_data) and fp_data >=limit], 
                          c=[fp_data for data, fp_data in zip(zipped_xy[0][0], final[1]) if not math.isnan(fp_data) and fp_data >=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=-4, vmax=4)
    scatter2 = ax.scatter([data for data, fp_data in zip(zipped_xy[0][0], final[1]) if not math.isnan(fp_data) and fp_data <=limit], 
                          [data for data, fp_data in zip(zipped_xy[1][0], final[1]) if not math.isnan(fp_data) and fp_data <=limit], 
                          c=[fp_data for data, fp_data in zip(zipped_xy[0][0], final[1]) if not math.isnan(fp_data) and fp_data <=limit], marker='o', s=1, 
                        cmap=custom_cmap, alpha=1, vmin=-4, vmax=4)
    # cbar4 = plt.colorbar(scatter2, shrink = 0.9)
    # cbar4.set_label('Inequality Index (-)')
    # # ticks = [8, 9, 10, 11, 12]
    # ticks = [-4, -2, 0, 2, 4]
    # ticks_label = [str(tick) for tick in ticks[1:-1]]
    # ticks_label.insert(0,f'< {ticks[0]}')
    # ticks_label.append(f'> {ticks[-1]}')
    # cbar4.set_ticks(ticks)  
    # cbar4.set_ticklabels(ticks_label)

    # Set labels and title
    ax.set_xlabel(zipped_xy[0][2])
    ax.set_ylabel(zipped_xy[1][2])

    # Set the same range for both axes with margin
    # ax.set_xlim(-10, 10)
    ax.set_ylim(0,49)

    # Set both x and y axes to have only 5 tickers
    ax.xaxis.set_major_locator(MaxNLocator(nbins=5))
    ax.yaxis.set_major_locator(MaxNLocator(nbins=5))
    plt.subplots_adjust(right=0.95, top = 0.975, bottom=0.15)

    save_figures(f'./Inequality index 2022 with temp and fp', False)

def scatter_plot_base_case_versus_magitude_test():

    def calculate_delta_cost_on_scale(ratio, scale, delta_elect_array, delta_gas_array):
    
        cost_gas =  read_from_web_price_gas(year) * scale
        # cost_gas =  cost_elec / ratio
        cost_elec =  cost_gas * ratio

        delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
        delta_cost = np.sum(delta_cost, axis=1)

        return delta_cost
    
    def scatter_plot_a(filename, x_values:np.array, y_values:np.array, x_label:str, y_label: str, title: str):
    
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

        save_figures(f'./{filename}', False)
    
    ratio = 4
    scaling_factor_list = [0.5, 2]
    for scaling_factor in scaling_factor_list:
        delta_cost = calculate_delta_cost_on_scale(ratio, scaling_factor, delta_elect_array, delta_gas_array)
        # index = calculate_compared_index(fp, delta_cost)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        final.append(index)
    
    scatter_plot_a(f'STANDARD Inequality index @two scaling factor',
                final[0], final[1], 'Inequality index @scaling factor=0.5 ','Inequality index @scaling factor=2 ',
                f'STANDARD Inequality index @two scaling factor')

def scatter_plot_change_of_cost():

    def calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array):
    
        delta_cost = elec_price * delta_elect_array - gas_price * delta_gas_array
        delta_cost = np.sum(delta_cost, axis=1)

        return delta_cost
    
    elec_price_list = [elec_price_2019*0.6, elec_price_2019*0.8, elec_price_2019, elec_price_2019*1.2, elec_price_2019*1.4]
    gas_price_list = [gas_price_2019*0.6, gas_price_2019*0.8, gas_price_2019, gas_price_2019*1.2, gas_price_2019*1.4]

    median_array = np.zeros((5, 5))
    range_array = np.zeros((5, 5))
    i = 0
    j = 0
    for elec_price in elec_price_list:
        for gas_price in gas_price_list:
            delta_cost = calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array)
            median = np.nanmedian(delta_cost)
            # range_val = np.nanpercentile(delta_cost, 75) - np.nanpercentile(delta_cost, 25)
            range_val = np.nanstd(delta_cost)
            median_array[i, j] = median
            range_array[i, j] = range_val
            j+=1
        j=0
        i+=1

    fig, ax = plt.subplots(figsize = (5,5))

    # # Create x, y coordinates for the 5x5 grid
    x_coords, y_coords = np.meshgrid(range(5), range(5))
    x_coords = x_coords.flatten()
    y_coords = y_coords.flatten()
    z_coords = median_array.flatten()

    # Flatten the data arrays
    colors = median_array.flatten()
    sizes = range_array.flatten()

    # Create the scatter plot with color and size
    scatter = ax.scatter(x_coords, y_coords, c=colors, s=sizes, cmap=custom_cmap, alpha=0.7, edgecolors='k')

    ax.set_xticks([0,1,2,3,4])
    ax.set_yticks([0,1,2,3,4])
    ax.set_xticklabels([0.6,0.8,1,1.2,1.4])
    ax.set_yticklabels([0.6,0.8,1,1.2,1.4])
    ax.set_xlabel('Electricity Price \n (Multiples of 2019 Data)')
    ax.set_ylabel('Gas Price \n (Multiples of 2019 Data)')
    plt.subplots_adjust(left=0.18,bottom=0.15, top=0.95, right=0.95)

    save_figures(f'./Change of cost scatter', False)

def scatter_plot_change_of_cost_3D():

    def calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array):
    
        delta_cost = elec_price * delta_elect_array - gas_price * delta_gas_array
        delta_cost = np.sum(delta_cost, axis=1)

        return delta_cost
    
    elec_price_list = [elec_price_2019*0.6, elec_price_2019*0.8, elec_price_2019, elec_price_2019*1.2, elec_price_2019*1.4]
    gas_price_list = [gas_price_2019*0.6, gas_price_2019*0.8, gas_price_2019, gas_price_2019*1.2, gas_price_2019*1.4]

    median_array = np.zeros((5, 5))
    range_array = np.zeros((5, 5))
    i = 0
    j = 0
    for elec_price in elec_price_list:
        for gas_price in gas_price_list:
            delta_cost = calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array)
            median = np.nanmedian(delta_cost)
            # range_val = np.nanpercentile(delta_cost, 75) - np.nanpercentile(delta_cost, 25)
            range_val = np.nanstd(delta_cost)
            median_array[i, j] = median
            range_array[i, j] = range_val
            j+=1
        j=0
        i+=1

    # fig, ax = plt.subplots()

    # # Create x, y coordinates for the 5x5 grid
    x_coords, y_coords = np.meshgrid(range(5), range(5))
    x_coords = x_coords.flatten()
    y_coords = y_coords.flatten()
    z_coords = median_array.flatten()

    # # Flatten the data arrays
    # colors = median_array.flatten()
    # sizes = range_array.flatten()

    # # Create the scatter plot with color and size
    # scatter = ax.scatter(x_coords, y_coords, c=colors, s=sizes, cmap=custom_cmap, alpha=0.7, edgecolors='k')

    # # Add colorbar
    # cbar = plt.colorbar(scatter)
    # cbar.set_label('Color Bar')

    # # Add legend for point sizes
    # legend_sizes = [10, 30, 50]  # Change these sizes according to your data
    # legend = ax.legend(*scatter.legend_elements("sizes", num=3), title="Point Size", loc="upper left", bbox_to_anchor=(1.05, 1.0))
    # legend.set_title("Point Size")

    # plt.xlabel('Electricity Price (Multiples of 2019 Data)')

    # plt.ylabel('Gas Price (Multiples of 2019 Data)')
    # # Create the 3D scatter plot with color and size
    fig = plt.figure(figsize=(7,7))
    ax = fig.add_subplot(111, projection='3d')
    scatter = ax.scatter(x_coords, y_coords, z_coords, c=z_coords, s=range_array.flatten(), cmap=custom_cmap, alpha=0.7, edgecolors='k')

    # Add colorbar
    cbar = plt.colorbar(scatter, pad=0.1, shrink=0.6)
    cbar.set_label('Median \u0394C \n (£/year/household)')

    # Add legend for point sizes outside the plot
    legend_sizes = [10, 30, 50]  # Change these sizes according to your data
    legend = ax.legend(*scatter.legend_elements("sizes", num=3), title="Point Size", loc="upper left", bbox_to_anchor=(0.7, 1.3))
    legend.set_title("Standard deviation (£/year/household)")
    
    # Label x, y, and z axes
    ax.set_xticks([0,1,2,3,4])
    ax.set_yticks([0,1,2,3,4])
    ax.set_xticklabels([0.6,0.8,1,1.2,1.4])
    ax.set_yticklabels([0.6,0.8,1,1.2,1.4])
    ax.set_xlabel('Electricity Price \n (Multiples of 2019 Data)',labelpad=10)
    ax.set_ylabel('Gas Price \n (Multiples of 2019 Data)',labelpad=10)
    ax.set_zlabel('Median change in domestic energy cost \n (£/year/household)',labelpad=10)
    # Remove grid background
    ax.grid(False)

    # # Set point size label to the left-hand side and outside the plot
    # ax.yaxis.set_label_position("top")
    # ax.yaxis.tick_left()

    plt.show()

    save_figures('./Scatter Plot with Color and Size', False)

def line_plot_inequality_20192022_as_xyaxis_subsidy_red():
    global df_fp
    ratio_list = [ratio_2022,ratio_2019]
    # ratio_list = [5, 3.25]
    normalised_temperature = calculate_normalized_temp(temp_mean, np.min(temp_mean), np.max(temp_mean),1)
    fp_norm = calculate_normalized_fp(fp, 0, 20,1)
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # delta_cost = (delta_cost) * (1 - 0.3 * fp_norm  - 0.86 * normalised_temperature ) / np.nanstd(delta_cost)
       
        delta_cost = ((delta_cost) * (1-1.3 * normalised_temperature)) / np.nanstd(delta_cost)
        delta_cost = delta_cost / (100 * fp_norm)
        index = delta_cost * calculate_normalized_index(fp, 0, 20)
        df_fp[f'index @{ratio}'] = index

    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)

    # Plotting
    fig, ax = plt.subplots(figsize = (9,4))
    # Line chart
    n = 100

    plt.bar(df_fp.index, df_fp[f'index @{ratio_list[1]}'],  
            color=colors_normalized[-1], 
            alpha = 1)
    
    average_indices = [df_fp[f'index @{ratio_list[1]}'][:i+1].mean() for i in range(0, len(df_fp), n)]
    ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='dotted', label = '' ,color='black')

    average_indices = [df_fp[f'index @{ratio_list[1]}'][:i+1].std()/10 for i in range(0, len(df_fp), n)]
    ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', color='black', label = 'Median')

    # Create custom legend lines
    from matplotlib.lines import Line2D
    legend_lines = [Line2D([0], [0], color='black', linestyle='dotted', markersize=5, label='Moving average'),
                    Line2D([0], [0], color='black', linestyle='-', markersize=5, label='Moving standard error')]

    ax.legend(handles=legend_lines, frameon=False)
    # Adding labels and title
    # plt.xlabel('Regions arranged by increasing fuel poverty (left to right)')
    ax.set_xticks([])
    ax.set_ylim(0, 8)
    ax.set_yticks([ 0, 2, 4, 6, 8 ])
    ax.set_yticklabels(['0','2','4','6','8'])
    plt.ylabel('Inequality index (-)')
    ax.text(50, 5, 'Increased domestic\nheating cost', fontsize=12, color=colors_normalized[-1], ha = 'left')
    ax.annotate(
                '',
                xy=(50, 1.5),
                xytext=(50, 4),
                arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[-1]),
                fontsize=12,
                color=colors_normalized[-1],
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    # ax.annotate(
    #             '',
    #             xy=(33000, -0.5),
    #             xytext=(0, -0.5),
    #             arrowprops=dict(arrowstyle='->', lw=2, color=colors_normalized[-1]),
    #             fontsize=12,
    #             color=colors_normalized[-1],
    #             ha='center',  # Horizontal alignment set to 'center'
    #             va='center',  # Vertical alignment set to 'center'
    #         )
    
    
    ax.text(25000, 7.4, '2019 prices',
            weight='bold',
            fontsize=12, 
            color=colors_normalized[-1], 
            alpha = 0.6, ha = 'left')
    # plt.title('Index Values for LSOA')
    # plt.legend(loc="lower right")  # Display legend
    
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.subplots_adjust(bottom=0.1, top = 0.95, left= 0.12, right= 0.99)

    save_figures(f'./Inequality index bar chart red', False)

def line_plot_inequality_20192022_as_xyaxis_subsidy_blue():
    global df_fp
    ratio_list = [ratio_2022,ratio_2019]
    # ratio_list = [5, 3.25]
    normalised_temperature = calculate_normalized_temp(temp_mean, np.min(temp_mean), np.max(temp_mean),1)
    fp_norm = calculate_normalized_fp(fp, 0, 20, 1)
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # delta_cost = (delta_cost) * (1 - 0.3 * fp_norm - 0.86 * normalised_temperature) / np.nanstd(delta_cost)
        
        delta_cost = ((delta_cost) * (1-1.3 * normalised_temperature)) / np.nanstd(delta_cost)
        delta_cost = delta_cost / (100 * fp_norm)
        index = delta_cost * calculate_normalized_index(fp, 0, 20)
        df_fp[f'index @{ratio}'] = index

    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)

    # Plotting
    fig, ax = plt.subplots(figsize = (9,4))
    # Line chart
    n = 100

    plt.bar(df_fp.index, df_fp[f'index @{ratio_list[0]}'], 
            label=f'Inequality index for price ratio at 2022',  
            color=colors_normalized[0], 
            alpha = 1)

    average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].mean() for i in range(0, len(df_fp), n)]
    ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='dotted', color='black')

    average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].std() / 10 for i in range(0, len(df_fp), n)]
    ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', color='black')

    # Adding labels and title
    plt.xlabel('Regions arranged by increasing proportion\nof fuel poverty (2019, left to right)')
    ax.set_xticks([])
    ax.set_ylim(-4, 4)
    ax.set_yticks([-4, -2, 0, 2, 4])
    plt.ylabel('Inequality index (-)')
    ax.text(50, 2.3, 'Increased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    ax.annotate(
                '',
                xy=(0, 0.5),
                xytext=(0, 2),
                arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
                fontsize=12,
                color=colors_normalized[-1],
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    ax.annotate(
                '',
                xy=(33000, -4),
                xytext=(0, -4),
                arrowprops=dict(arrowstyle='->', lw=2, color='black'),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    ax.text(50, -3, 'Decreased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    ax.annotate(
                '',
                xy=(50, -0.5),
                xytext=(50, -2),
                arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
                fontsize=12,
                color=colors_normalized[0],
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    
    ax.text(25000, -3.5, '2022 prices',
            weight='bold',
            fontsize=12, 
            color=colors_normalized[0], 
            alpha = 0.6, ha = 'left')
    
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.subplots_adjust(bottom=0.15, top = 0.95, left= 0.12, right= 0.94)

    save_figures(f'./Inequality index bar chart blue', False)

def line_plot_change_of_cost_comparison():

    df_cop_max = pd.read_csv(f"./Data/properties_csv/2019/COP_max.csv")
    df_cop_min = pd.read_csv(f"./Data/properties_csv/2019/COP_min.csv")
    identifier = 'E01004731'
    
    data_for_identifier = df_cop_min[df_cop_min["LSOA_code"] == identifier]
    # Calculate the minimum value for the data columns and add it to the cop_min list
    cop_min = data_for_identifier.iloc[:, 1:].values[0]
    
    data_for_identifier = df_cop[df_cop["LSOA_code"] == identifier]
    # Calculate the minimum value for the data columns and add it to the cop_min list
    cop_mean = data_for_identifier.iloc[:, 1:].values[0]

    data_for_identifier = df_cop_max[df_cop_max["LSOA_code"] == identifier]
    # Calculate the minimum value for the data columns and add it to the cop_min list
    cop_max = data_for_identifier.iloc[:, 1:].values[0]

    data_for_identifier = df_gas[df_gas["LSOA_code"] == identifier]
    # Calculate the minimum value for the data columns and add it to the cop_min list
    gas_consump = data_for_identifier.iloc[:, 1:13].values[0]

    cop_list = [cop_min, cop_mean, cop_max]
    delta_cost_2019_list = []
    delta_cost_2022_list = []
    for cop in cop_list:
        delta_gas_array = delta_gas(uptake, gas_consump)
        delta_elect_array = delta_elec(delta_gas_array, cop)
        delta_cost_2019 = elec_price_2019 * delta_elect_array - gas_price_2019 * delta_gas_array
        delta_cost_2022 = elec_price_2022 * delta_elect_array - gas_price_2022 * delta_gas_array
        delta_cost_2019_list.append(delta_cost_2019)
        delta_cost_2022_list.append(delta_cost_2022)
    
    # Initialize the plot
    x_axis = [0,1,2,3,4,5,6,7,8,9,10,11]
    fig, axs = plt.subplots(figsize=(6,4))
    
    sb.lineplot(x=x_axis, y=delta_cost_2019_list[1], label="\u0394C @2019", color=colors_normalized[-1], linewidth=2, linestyle='solid')
    plt.fill_between(x=x_axis, y1=delta_cost_2019_list[0], y2=delta_cost_2019_list[2], color=colors_normalized[-2],linestyle='solid', alpha=0.3)
    sb.lineplot(x=x_axis, y=delta_cost_2022_list[1], label="\u0394C @2022", color=colors_normalized[0], linewidth=2, linestyle='solid')
    plt.fill_between(x=x_axis, y1=delta_cost_2022_list[0], y2=delta_cost_2022_list[2], color=colors_normalized[1],linestyle='solid', alpha=0.3)
    
    axs.set_xticks(x_axis)
    axs.set_xticklabels(labels = ['J','F','M','A','M','J','J','A','S','O','N','D'])
    axs.set_ylabel('\u0394C (£/month/household)')
    axs.set_xlabel('Month')
    axs.set_ylim(-40)
    axs.legend(frameon=False,loc='lower left')

    plt.title(f'LSOA:{identifier}')
    save_figures('./change in fuel cost comparison temporal line plot', False)

def line_plot_normalized_proportion_of_inequality_region():
    
    def plot_normalized_line_with_annotations_a(x_values, y1_values, y2_values, x_label, 
                                                y_legend, y1_label, y2_label,title, format,
                                                pdf = False):
    

        y_values1_norm = y1_values
        y_values2_norm = y2_values

        fig, ax1 = plt.subplots(figsize=(7,4))
        if format == 'line':
            # Plot the lines
            line1, =ax1.plot(x_values, y_values1_norm,  marker='o', linestyle='-', color=colors_normalized[0], label= 'Proportion of Area\nInequality index > 0')
            ax2 = ax1.twinx()
            line2, =ax2.plot(x_values, y_values2_norm, marker='o', linestyle='-', color=colors_normalized[-1], label= 'Proportion of Area\nInequality index > 5')

            # Add legend
            lines = [line1, line2]
            labels = [line.get_label() for line in lines]
            ax1.legend(lines, labels, loc='lower right', frameon=False, fontsize=12)
            y_ticks = np.linspace(0, 100, num = 5)
            y_ticks = np.around(y_ticks, decimals=0)
            ax1.set_yticks(ticks=y_ticks)
            ax1.set_ylim(-10,100)
            ax2.set_yticks(ticks=[0,1,2,3])
            ax2.set_ylim(-0.3,3)

        if format == 'bar':
            width = 0.1  # Width of the bars
            x_values_bar1 = [x + width/2 for x in x_values]
            x_values_bar2 = [x - width/2 for x in x_values]
            
            colors_normalized[3] = (253/255, 200/255, 0)
            colors_normalized[2] = (135/255,206/255, 235/255)
            colors_normalized.append(colors_normalized[0])
            ax2 = ax1.twinx()
            bars1 = ax2.bar(x_values_bar1, y_values1_norm, width, 
                            color=colors_normalized, alpha = 0.5, label='')
            bars2 = ax1.bar(x_values_bar2, y_values2_norm, width, 
                            color=colors_normalized, alpha = 1, label='')

            y_ticks = np.linspace(0, 100, num = 5)
            y_ticks = np.around(y_ticks, decimals=0)
            ax2.set_yticks(ticks=y_ticks)

            ax2.set_ylim(0,100)
            ax1.set_yticks(ticks=[0,1, 2, 3, 4])
            ax1.set_ylim(0,4)

        # Set labels and title
        ax2.set_ylabel(y_legend, fontsize='small')
        ax1.set_yticklabels(ax1.get_yticklabels(), rotation=90)
        ax2.set_yticklabels(ax2.get_yticklabels(), rotation=90)
        ax1.set_ylabel('Proportions of LSOAs  with inequality index > 5 (%)',fontsize='small')

        ax1.set_xticks([])
        ax1.set_xlim(3.15, 5.1) 
        # Remove the top and right spines
        ax1.spines['top'].set_visible(False)
        # Remove the left and bottom spines for ax2
        ax1.spines['bottom'].set_visible(False)
        ax2.spines['top'].set_visible(False)
        # ax2.spines['right'].set_visible(False)

        # ax2.spines['left'].set_visible(False)
        ax2.spines['bottom'].set_visible(False)


        plt.tight_layout()  
        plt.subplots_adjust(bottom=0.05)

        save_figures(f'./{title}+{format}', pdf)

    ratio_list = [3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        red_area = round(np.sum(index > 0)/len(index)*100,3)
        very_red_area = round(np.sum(index > 5)/len(index)*100,3)

        red_area_list.append(red_area)
        very_red_area_list.append(very_red_area)

    plot_normalized_line_with_annotations_a(ratio_list, red_area_list, very_red_area_list ,'Price Ratio', 
                                            'Proportions of LSOAs with inequality index > 0 (%)',
                                            'Proportions of LSOAs with inequality index > 0',
                                            'Proportions of LSOAs with inequality index > 5',
                                            'STANDARD normalized Inequality Index versus Price ratio',
                                            'bar',
                                            False)

def line_plot_change_of_cost():
    
        def calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array):
        
            delta_cost = elec_price * delta_elect_array - gas_price * delta_gas_array
            delta_cost = np.sum(delta_cost, axis=1)

            return delta_cost
        
        def elec():

            elec_price_list = [elec_price_2019*0.6, elec_price_2019*0.8, elec_price_2019, elec_price_2019*1.2, elec_price_2019*1.4]
            gas_price_list = [gas_price_2019*0.6, gas_price_2019*0.8, gas_price_2019, gas_price_2019*1.2, gas_price_2019*1.4]

            median_array = np.zeros((5, 5))
            std_array = np.zeros((5, 5))
            i = 0
            j = 0
            for elec_price in elec_price_list :
                for gas_price in gas_price_list :
                    delta_cost = calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array)
                    median = np.nanmedian(delta_cost)
                    median_array[i, j] = median
                    std_array[i, j] = np.nanstd(delta_cost)
                    j+=1
                j=0
                i+=1
            
            x_values = [0.6, 0.8, 1, 1.2, 1.4]

            fig = plt.figure(figsize=(5,5))
            plt.tight_layout()
            colors = [colors_normalized[0],
                    colors_normalized[1],
                        colors_normalized[4],
                        colors_normalized[5],
                        colors_normalized[-1],
                    ]
            # Plot the lines
            x_values_text = ['0.6','0.8','1.0','1.2','1.4']
            for i in range(5):
                plt.plot(x_values, median_array[i],  marker='o', linestyle='-', color=colors[i], label= fr'{x_values_text[i]} $\times$ 2019 electricity price')
            
            # Set labels and title
            plt.xlabel('Multiple of 2019 gas price')
            plt.ylabel('Median \u0394C (£/year/household)')

            x_ticks = np.linspace(np.min(x_values), np.max(x_values), num = 5)
            x_ticks = np.around(x_ticks, decimals=1)
            plt.xticks(ticks=x_ticks)

            # # y_ticks = np.linspace(np.min(median_array)*1.1,np.max(median_array)*1.1, num = 6)
            # y_ticks = np.around(y_ticks, decimals=)
            y_ticks = [-400, -200, 0, 200, 400, 600]
            plt.yticks(ticks=y_ticks)
            plt.subplots_adjust(left=0.2, right=0.975, bottom=0.10, top=0.975)

            save_figures(f'./Change of cost 5 line elec cost', False)
            
            fig = plt.figure(figsize=(5,5))
            plt.tight_layout()
            # Plot the lines
            for i in range(5):
                plt.plot(x_values, std_array[i],  marker='o', linestyle='-', color=colors[i], label= fr'{x_values_text[i]} $\times$ 2019 electricity price')
            
            # Set labels and title
            plt.xlabel('Multiple of 2019 gas price')
            plt.ylabel('Standard Deviation of \u0394C (£/year/household)')

            plt.xticks(ticks=x_ticks)

            # # y_ticks = np.linspace(np.min(median_array)*1.1,np.max(median_array)*1.1, num = 6)
            # y_ticks = np.around(y_ticks, decimals=)
            plt.ylim(0, 130)
            y_ticks = [30, 60, 90, 120]
            plt.yticks(ticks=y_ticks)
            # Add legend
            plt.legend(frameon=False,fontsize='small')
            plt.subplots_adjust(left=0.15, bottom=0.10, top=0.975, right=0.975)

            save_figures(f'./standard deviation 5 line elec cost', False)
        
        def ratio():

            colors = [colors_normalized[0],
                    colors_normalized[1],
                        colors_normalized[4],
                        colors_normalized[5],
                        colors_normalized[-1],
                    ]
            
            elec_price_list = [elec_price_2019*0.6, elec_price_2019*0.8, elec_price_2019, elec_price_2019*1.2, elec_price_2019*1.4]
            
            gas_price_list = [gas_price_2019*0.6, gas_price_2019*0.8, gas_price_2019, gas_price_2019*1.2, gas_price_2019*1.4]

            median_array_elec = np.zeros((5, 5))
            std_array_elec = np.zeros((5, 5))
            i = 0
            j = 0
            x_values_text = ['0.6','0.8','1.0','1.2','1.4']
            for elec_price in elec_price_list :
                for gas_price in gas_price_list :
                    delta_cost = calculate_delta_cost_by_price(gas_price, elec_price, delta_elect_array, delta_gas_array)
                    median_array_elec[i, j] = np.nanmedian(delta_cost)
                    std_array_elec[i, j] = np.nanstd(delta_cost)
                    j+=1
                j=0
                i+=1
            
            ratio = elec_price_2019 / gas_price_2019
            x_values = [0.6, 0.8, 1, 1.2, 1.4] 
            ratio_list = [x_values.copy() for _ in range(5)]

            fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True,figsize=(9,6))
            plt.tight_layout()
            colors = [colors_normalized[0],
                    colors_normalized[1],
                    colors_normalized[4],
                    colors_normalized[5],
                    colors_normalized[-1],
                    ]
            # Plot the lines
            for i in range(5):
                scatter = ax1.scatter([round(x_values[i] / x ,1) for x in ratio_list[i]], median_array_elec[i],  marker='o', s=40, c=colors[i])

            for i in range(5):
                scatter = ax2.scatter([round(x_values[i] / x ,1) for x in ratio_list[i]], std_array_elec[i],  marker='o', s=40, c=colors[i], label= fr'{x_values_text[i]} $\times$ 2019 electricity price')

            # plt.fill_between(x=[2,3,4,5,6,7,8,9,10,11], 
            #                  y1=[550, 400, 250, 100, 0, -70, -120, -150,-180, -200 ], 
            #                  y2=[520, 300, 100, 0, -70, -120, -150, -180, -200, -400 ], 
            #                  color=colors[-2],
            #                  linestyle='solid', alpha=0.3)

            ax2.legend(loc='lower right', frameon=False,fontsize='small')
            # ax1.plot([2.4, 0.4],
            #          [500, -400],
            #           color=colors[-2],
            #           linestyle='--',
            #           marker = 'none'
            # )
            # ax2.plot([2.4, 0.4],
            #          [100, 50],
            #           color=colors[-2],
            #           linestyle='--',
            #           marker = 'none'
            # )
            # Set labels and title
            ax2.set_xlabel('Multiple of 2019 price ratio')
            ax1.set_ylabel('Median \u0394C \n (£/year/household)')
            ax2.set_ylabel('Standard deviation of \u0394C \n (£/year/household)')


            x_ticks = np.linspace(0.4, 2.4, num = 5)
            x_ticks = np.around(x_ticks, decimals=1)
            ax1.set_xticks(ticks=x_ticks)

            # # y_ticks = np.linspace(np.min(median_array)*1.1,np.max(median_array)*1.1, num = 6)
            # y_ticks = np.around(y_ticks, decimals=)
            y_ticks_1 = [-400, -200, 0, 200, 400, 600]
            ax1.set_yticks(ticks=y_ticks_1)
            y_ticks = [0, 25, 50, 75, 100, 125]
            ax2.set_yticks(ticks=y_ticks)
            plt.subplots_adjust(left=0.13)
            plt.subplots_adjust(bottom=0.1)

            save_figures(f'./Change of cost 5 line ratio', False)

        elec()
        # ratio()

def line_plot_inequality_20192022_as_xyaxis_red():
    global df_fp
    ratio_list = [ratio_2022,ratio_2019]
    # ratio_list = [5, 3.25]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        # cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        # cost_ratio = delta_cost / cost 
        # # nsd = np.nanstd(delta_cost)
        # normalised_cost = delta_cost/ nsd
        df_fp[f'index @{ratio}'] = index

    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)

    # Plotting
    fig, ax = plt.subplots(figsize = (9,4))
    # Line chart
    n = 100

    plt.bar(df_fp.index, df_fp[f'index @{ratio_list[1]}'],  
            color=colors_normalized[-1], 
            alpha = 1)
    
    average_indices = [df_fp[f'index @{ratio_list[1]}'][:i+1].mean() for i in range(0, len(df_fp), n)]
    standarderror_indices = [df_fp[f'index @{ratio_list[1]}'][:i+1].std()/10 for i in range(0, len(df_fp), n)]
    upper = [average_indices[i]+ 3 * standarderror_indices[i] for i in range(len(average_indices))]
    lower = [average_indices[i]- 3 * standarderror_indices[i] for i in range(len(average_indices))]
    ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', label = 'Moving standard average' ,color='black')
    
    plt.fill_between(x= [i for i in range(0, len(df_fp), n)], y1= upper, y2=lower, 
                     color='black', linestyle='solid', alpha=0.2,label="3 times standard error interval")
    
    # average_indices = [df_fp[f'index @{ratio_list[1]}'][:i+1].std()/10 for i in range(0, len(df_fp), n)]
    # ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', color='black', label = 'Median')

    # Create custom legend lines
    ax.legend( frameon=False)
    # Adding labels and title
    # plt.xlabel('Regions arranged by increasing fuel poverty (left to right)')
    ax.set_xticks([])
    ax.set_ylim(0, 8)
    ax.set_yticks([ 0, 2, 4, 6, 8 ])
    ax.set_yticklabels(['0','2','4','6','8'])
    plt.ylabel('Inequality index (-)')
    ax.text(1000, 3.3, 'Increased domestic\nheating cost', fontsize=12, color=colors_normalized[-1], ha = 'left')
    ax.annotate(
                '',
                xy=(1000, 1.5),
                xytext=(1000, 3),
                arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[-1]),
                fontsize=12,
                color=colors_normalized[-1],
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    # ax.annotate(
    #             '',
    #             xy=(33000, -0.5),
    #             xytext=(0, -0.5),
    #             arrowprops=dict(arrowstyle='->', lw=2, color=colors_normalized[-1]),
    #             fontsize=12,
    #             color=colors_normalized[-1],
    #             ha='center',  # Horizontal alignment set to 'center'
    #             va='center',  # Vertical alignment set to 'center'
    #         )
    
    
    ax.text(25000, 7.4, '2019 prices',
            weight='bold',
            fontsize=12, 
            color=colors_normalized[-1], 
            alpha = 0.6, ha = 'left')
    # plt.title('Index Values for LSOA')
    # plt.legend(loc="lower right")  # Display legend
    
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.subplots_adjust(bottom=0.1, top = 0.95, left= 0.12, right= 0.99)

    save_figures(f'./Inequality index bar chart red', False)

def line_plot_inequality_20192022_as_xyaxis_blue():
    global df_fp
    ratio_list = [ratio_2022,ratio_2019]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        df_fp[f'index @{ratio}'] = index

    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)

    # Plotting
    fig, ax = plt.subplots(figsize = (9,4))
    # Line chart
    n = 100
    # ax2 = ax.twinx()
    # ax2.yaxis.set_visible(False)
    # ax2.set_ylim(-1.5, 1.5)
    # ax2.spines['bottom'].set_visible(False)
    # ax2.spines['top'].set_visible(False)
    # ax2.spines['right'].set_visible(False)

    plt.bar(df_fp.index, df_fp[f'index @{ratio_list[0]}'],  
            color=colors_normalized[0], 
            alpha = 1)
    
    average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].mean() for i in range(0, len(df_fp), n)]
    standarderror_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].std()/10 for i in range(0, len(df_fp), n)]
    upper = [average_indices[i]+ 3 * standarderror_indices[i] for i in range(len(average_indices))]
    lower = [average_indices[i]- 3 * standarderror_indices[i] for i in range(len(average_indices))]
    ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', label = 'Moving standard average' ,color='black')
    
    plt.fill_between(x= [i for i in range(0, len(df_fp), n)], y1= upper, y2=lower, 
                     color='black', linestyle='solid', alpha=0.2,label="3 times standard error interval")

    # Adding labels and title
    ax.set_xlabel('Regions arranged by increasing proportion\nof fuel poverty (2019, left to right)')
    # Set x-axis labels to align to the right
    ax.tick_params(axis='x', labelrotation=0)  # Set label rotation to 0 degrees (horizontal)

    # Get the x-axis labels and adjust their horizontal alignment
    xtick_labels = ax.get_xticklabels()
    for label in xtick_labels:
        label.set_ha('right')
    ax.set_xticks([])
    ax.set_ylim(-4, 4)
    ax.set_yticks([-4, -2, 0, 2, 4])
    plt.ylabel('Inequality index (-)')
    ax.text(1000, 2.3, 'Increased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    ax.annotate(
                '',
                xy=(1000, 0.5),
                xytext=(1000, 2),
                arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
                fontsize=12,
                color=colors_normalized[-1],
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    ax.annotate(
                '',
                xy=(33000, -4),
                xytext=(0, -4),
                arrowprops=dict(arrowstyle='->', lw=2, color='black'),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    ax.text(1000, -3, 'Decreased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    ax.annotate(
                '',
                xy=(1000, -0.5),
                xytext=(1000, -2),
                arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
                fontsize=12,
                color=colors_normalized[0],
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    
    ax.text(25000, -3.5, '2022 prices',
            weight='bold',
            fontsize=12, 
            color=colors_normalized[0], 
            alpha = 0.6, ha = 'left')
    
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.subplots_adjust(bottom=0.15, top = 0.95, left= 0.12, right= 0.94)

    save_figures(f'./Inequality index bar chart blue', False)

def line_plot_price_ratio_only_with_prediction():
   
    def elec_and_gas():
        # Create figure and first subplot (left y-axis)
        fig, (ax1, ax2) = plt.subplots(2, 1, sharex=True, figsize=(9, 6))
        # Plot the first line (y1_values) with left y-axis
        line1, =ax1.plot(year_list, elec_price_list, linestyle='-', color=colors_normalized[0], label = 'Historic electricity price')
        ax1.set_ylabel('Electricity price (£/KWh)', color='black')
        ax1.tick_params(axis='y', labelcolor='black')

        ax1.set_ylim(0, 0.8)
        # Set the y ticks
        ax1.set_yticks([0.2, 0.4, 0.6, 0.8])

        # Ensure the y tick labels are formatted appropriately
        ax1.yaxis.set_major_formatter('{:.2f}'.format)

        
        prediction_year_list = [2023,2024,2025,2026,2027,
                                2028,2029,2030,2031,2032,2033,
                                2034,2035,2036,2037,2038,2039,
                                2040,2041,2042,2043,2044,2045,
                                2046,2047,2048,2049,2050]
        
        prediction_year_list = list(map(lambda year: str(year), prediction_year_list))
        data = pd.read_excel('./Data/properties_csv/predicted prices.csv', sheet_name='electricity')

        # Extract data from specific columns and rows
        low_case_ratio = data.iloc[0:29, 1].tolist()   # Column B, rows 1 to 28
        central_case_ratio = data.iloc[0:29, 2].tolist()   # Column C, rows 1 to 28
        high_case_ratio = data.iloc[0:29, 3].tolist()   # Column D, rows 1 to 28
        low_case_ratio = [data/100 for data in low_case_ratio]
        central_case_ratio = [data/100 for data in central_case_ratio]
        high_case_ratio = [data/100 for data in high_case_ratio]

        # Plot the second and third lines (y2_values, y3_values) with right y-axis
        line2, =ax1.plot(prediction_year_list, central_case_ratio, linestyle='-', color=colors_normalized[1], label = 'Forecast electricity price')
        
        ax1.fill_between(x=prediction_year_list, 
                        y1=low_case_ratio, 
                        y2=high_case_ratio, 
                        color=colors_normalized[2], 
                        linestyle='solid', alpha=0.6)


        ax1.set_xticks(['2010', '2020', '2030', '2040', '2050'])
        ax1.set_xlim(xmax= len(prediction_year_list)+len(year_list) - 2)

        # Add text indicating the forecast value
        ax1.text('2027', 0.65, 'Forecast', ha='center', va='center', color=colors_normalized[0], rotation=90, fontsize=12, fontweight='bold')
        ax1.axvspan('2023', '2050', facecolor=colors_normalized[2], alpha=0.2)
        ax1.legend(frameon=False, loc='upper left')
        
        # -----------------------------#
        line1, =ax2.plot(year_list, gas_price_list, linestyle='-', color=colors_normalized[-1], label = 'Historic gas price')
        ax2.set_xlabel('Year')
        ax2.set_ylabel('Gas price (£/KWh)', color='black')
        ax2.tick_params(axis='y', labelcolor='black') 
        # Add text indicating the forecast value
        ax2.text('2027', 0.18, 'Forecast', ha='center', va='center', color=colors_normalized[-1], rotation=90, fontsize=12, fontweight='bold')
        
        ax2.axvspan('2023', '2050', facecolor=colors_normalized[-3], alpha=0.2)
        ax2.annotate('COVID-19 \nLockdown', xy=('2020', 0), xytext=('2016', 0.1), 
                    arrowprops=dict(arrowstyle='->'), fontsize=12)

        # Add arrow symbol and label for '2022'
        ax2.annotate('Russian-Ukraine \n Conflict', xy=('2022', 0), xytext=('2030', 0.1), 
                    arrowprops=dict(arrowstyle='->'), fontsize=12)

        ax2.set_ylim(0, 0.22)
        # Set the y ticks
        ax2.set_yticks([0.05, 0.1, 0.15, 0.2])

        # Ensure the y tick labels are formatted appropriately
        ax2.yaxis.set_major_formatter('{:.2f}'.format)

        data = pd.read_excel('./Data/properties_csv/predicted prices.csv', sheet_name='gas')

        # Extract data from specific columns and rows
        low_case_ratio = data.iloc[0:29, 1].tolist()   # Column B, rows 1 to 28
        central_case_ratio = data.iloc[0:29, 2].tolist()   # Column C, rows 1 to 28
        high_case_ratio = data.iloc[0:29, 3].tolist()   # Column D, rows 1 to 28
        low_case_ratio = [data/100 for data in low_case_ratio]
        central_case_ratio = [data/100 for data in central_case_ratio]
        high_case_ratio = [data/100 for data in high_case_ratio]

        # Plot the second and third lines (y2_values, y3_values) with right y-axis
        line2, =ax2.plot(prediction_year_list, central_case_ratio, linestyle='-', color=colors_normalized[-2], label = 'Forecast gas price')
        ax2.legend(frameon=False, loc='upper left')
        plt.fill_between(x=prediction_year_list, 
                        y1=low_case_ratio, 
                        y2=high_case_ratio, 
                        color=colors_normalized[-3], 
                        linestyle='solid', alpha=0.6)
        
        plt.subplots_adjust(left=0.1,right=0.975, top=0.975, hspace=0)

        save_figures('./Electricity Price with prediction', pdf = False)

    def ratio():
        # Create figure and first subplot (left y-axis)
        fig, ax1 = plt.subplots(figsize=(9, 4))
        # Plot the first line (y1_values) with left y-axis
        line1, =ax1.plot(year_list, price_ratio_list, linestyle='-', color=colors_normalized[0], label = 'Historic Price Ratio')
        ax1.set_ylabel('Price ratio (-)', color='black')
        ax1.tick_params(axis='y', labelcolor='black')


        ax1.set_ylim(3, 5.3)
        # Set the y ticks
        ax1.set_yticks([3, 3.5, 4, 4.5, 5])

        # Ensure the y tick labels are formatted appropriately
        ax1.yaxis.set_major_formatter('{:.1f}'.format)

        
        prediction_year_list = [2023,2024,2025,2026,2027,
                                2028,2029,2030,2031,2032,2033,
                                2034,2035,2036,2037,2038,2039,
                                2040,2041,2042,2043,2044,2045,
                                2046,2047,2048,2049,2050]
        
        prediction_year_list = list(map(lambda year: str(year), prediction_year_list))
        
        low_case_ratio = [3.54,4.24,4.57,4.93,4.71,4.65,4.65,4.62,4.40,4.40,4.46,4.51,4.43,4.43,
        4.32,4.38,4.35,4.47,4.41,4.41,4.40,4.45,4.36,4.44,4.37,4.27,4.34,4.26,]

        central_case_ratio = [3.69, 3.59, 4.07, 4.27, 4.34, 4.22, 4.21, 4.18, 3.99, 3.99, 4.01,
                            4.06, 4.00, 3.96, 3.87, 3.92 , 3.89, 4.00, 3.94, 3.94, 3.9, 3.98, 3.89, 3.97, 3.91, 3.81, 3.88, 3.81]

        high_case_ratio = [3.69,3.68,3.71,3.57,3.85,3.77,3.76,3.74,3.57,3.57,3.60,3.64,3.59,3.56, 3.47, 3.52
                            , 3.49, 3.59, 3.54, 3.54, 3.53, 3.57, 3.49, 3.56, 3.5, 3.42, 3.48, 3.41]


        # Plot the second and third lines (y2_values, y3_values) with right y-axis
        line2, =ax1.plot(prediction_year_list, central_case_ratio, linestyle='-', color=colors_normalized[-1], label = 'Forecast Price Ratio')
        
        plt.fill_between(x=prediction_year_list, 
                        y1=low_case_ratio, 
                        y2=high_case_ratio, 
                        color=colors_normalized[-1], 
                        linestyle='solid', alpha=0.2)


        ax1.set_xticks(['2010', '2020', '2030', '2040', '2050'])
        ax1.set_xticklabels(['2010', '2020', '2030', '2040', '2050'],fontsize=15)
        ax1.set_xlim(xmax= len(prediction_year_list)+len(year_list) - 2)
        ax1.set_xlabel('Year', fontsize=15)
        
        # Add text indicating the forecast value
        ax1.text('2024', 5.0, 'Forecast', ha='center',
                  va='center', color=colors_normalized[-1], rotation=90, 
                  fontsize=12, fontweight='bold')
        
        ax1.axvspan('2023', '2050', facecolor=colors_normalized[-1], alpha=0.05)
        # Add horizontal dashed lines between y=3.5 and y=4.5
        ax1.axhline(y=3.25, xmin=0, xmax=8/42, color='gray', linestyle='--')
        ax1.axhline(y=3.25, xmin=13/42, xmax=15/42, color='gray', linestyle='--')
        ax1.axhline(y=3.25, xmin=22/42, color='gray', linestyle='--')
        ax1.axhline(y=5, xmin=0, xmax=15/42, color='gray', linestyle='--')
        ax1.axhline(y=5, xmin=17/42, xmax=29/42, color='gray', linestyle='--')
        # Combine the legend from both subplots
        # Add arrow symbol and label for '2020'
        ax1.annotate('COVID-19\nlockdown', xy=('2020', 3), xytext=('2016', 3.2), 
                    arrowprops=dict(arrowstyle='->'), fontsize=15)

        # Add arrow symbol and label for '2022'
        ax1.annotate('Russian-Ukraine\nconflict', xy=('2022', 3), xytext=('2023', 3.2), 
                    arrowprops=dict(arrowstyle='->'), fontsize=15)
        
        ax1.legend(frameon=False, loc = 'upper right')
        plt.subplots_adjust(left=0.05,right=0.95,bottom=0.14)

        save_figures('./Price ratio with prediction', pdf = False)

    year_list = ['2010', '2011', '2012','2013', '2014','2015','2016','2017','2018','2019','2020','2021','2022', '2023']
    price_ratio_list = []
    elec_price_list = []
    gas_price_list = []

    for year in year_list:
        
        if year == '2023':
            price_ratio_list.append(3.69)
            elec_price_list.append(41.71/100)
            gas_price_list.append(11.3/100)
        else:
            elect_price = read_from_web_price_elec(year)
            gas_price = read_from_web_price_gas(year)
            price_ratio = round(elect_price/gas_price,3)
            elec_price_list.append(elect_price)
            gas_price_list.append(gas_price)
            price_ratio_list.append(price_ratio)
    
    ratio()
    # elec_and_gas()

def subsidy_plot_2019_density():

    global df_fp
    ratio_list = [ratio_2022,ratio_2019]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        STD_delta_cost = np.nanstd(delta_cost)
        df_fp[f'index @{ratio}'] = index
    
    subsidy_array = np.zeros_like(delta_cost)
    inequality_target = 1
    fp_norm = calculate_normalized_index(fp,0,20)
    for i in range(len(delta_cost)):
            if index[i] > inequality_target:
                subsidy_array[i] = delta_cost[i]  - STD_delta_cost *  inequality_target / fp_norm[i]
    
    delta_cost = calculate_delta_cost(ratio_2019, delta_elect_array, delta_gas_array, True)
    delta_cost = delta_cost - subsidy_array
    total_subsidy = subsidy_array * meters
    total_subsidy = np.nansum(total_subsidy, axis=0)
    rounded_number = round(total_subsidy / 1000000)
    # delta_cost = np.sum(delta_cost, axis=1)
    # fp_subsidy = np.zeros_like(delta_cost)
    # fp_baseline = 5
    # for i in range(len(fp_subsidy)):
    #     if fp[i] < fp_baseline:
    #         fp_subsidy[i] = 0 
    #     else:
    #         fp_subsidy[i]= (fp[i] - fp_baseline) * 1.5
    # delta_cost = delta_cost - fp_subsidy
    delta_cost = (delta_cost) / STD_delta_cost
    index_after_subsidy = delta_cost*fp_norm
    df_fp[f'index after subsidy'] = index_after_subsidy
    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)
    df_fp= df_fp.dropna()
    df_fp = df_fp.reset_index(drop=True)

    # Select relevant columns from df_fp
    df = df_fp[['index after subsidy', f'index @{ratio_list[1]}']]

    # Set the style for seaborn (white background with ticks)
    sns.set(style="white", rc={"axes.facecolor": (0, 0, 0, 0)})

    # Create the ridge plot
    plt.figure(figsize=(4, 3))
    sns.kdeplot(data=df[f'index @{ratio_list[1]}'], color=colors_normalized[-1], fill=True, alpha=0.5, linewidth=3)
    sns.kdeplot(data=df['index after subsidy'], color=colors_normalized[0], fill=True, alpha=0.5, linewidth=3)

    # Set labels and limits
    plt.ylabel('Density')
    plt.yticks([])
    plt.xticks([-4, 0, 4, 8])
    plt.xlabel('Inequality index (-)')
    plt.ylim(0, 1)
    plt.xlim(-4, 10)
    sns.despine(left=False, right=True, top=True)
    plt.subplots_adjust(bottom=0.18, top=0.97, left=0.06, right=0.98)

    save_figures(f'./Inequality index bar chart 2019 after subsidy density', False)

def subsidy_plot_2019_bar():
    
    global df_fp
    ratio_list = [ratio_2022,ratio_2019]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        STD_delta_cost = np.nanstd(delta_cost)
        df_fp[f'index @{ratio}'] = index
    # geo_avg_temp = [3.848,	6.520,	7.802,	8.856,	11.139,	14.335,	17.608,	17.144,	14.236,	10.026,	6.210,	5.675]
    # subsidy_array = np.zeros_like(temp)
    # for i in range(len(temp)):
    #     for j in range(12):
    #         if temp[i,j] < geo_avg_temp[j]:
    #             subsidy_array[i,j] = math.ceil(geo_avg_temp[j] - temp[i,j])*3
    subsidy_array = np.zeros_like(delta_cost)
    inequality_target = 1
    fp_norm = calculate_normalized_index(fp,0,20)
    for i in range(len(delta_cost)):
            if index[i] > inequality_target:
                subsidy_array[i] = delta_cost[i]  - STD_delta_cost *  inequality_target / fp_norm[i]
    
    delta_cost = calculate_delta_cost(ratio_2019, delta_elect_array, delta_gas_array, True)
    delta_cost = delta_cost - subsidy_array
    # for i in range(len(delta_cost)):
    #         if index[i] < inequality_target:
    #             delta_cost[i] = 0
    total_subsidy = subsidy_array * meters
    total_subsidy = np.nansum(total_subsidy, axis=0)
    rounded_number = round(total_subsidy / 1000000)
    # delta_cost = np.sum(delta_cost, axis=1)
    # fp_subsidy = np.zeros_like(delta_cost)
    # fp_baseline = 5
    # for i in range(len(fp_subsidy)):
    #     if fp[i] < fp_baseline:
    #         fp_subsidy[i] = 0 
    #     else:
    #         fp_subsidy[i]= (fp[i] - fp_baseline) * 1.5
    # delta_cost = delta_cost - fp_subsidy
    delta_cost = (delta_cost) / STD_delta_cost
    index_after_subsidy = delta_cost*fp_norm
    df_fp[f'index after subsidy'] = index_after_subsidy
    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)
    df_fp= df_fp.dropna()
    df_fp = df_fp.reset_index(drop=True)

    # Plotting
    fig, ax = plt.subplots(figsize = (9,4))
    # # Line chart
    # n = 100

    plt.bar(df_fp.index, df_fp[f'index @{ratio_list[1]}'], 
            label=f'Inequality index before subsidy',  
            color=colors_normalized[-1], 
            alpha = 1, align='edge',width=2)

    new_index = pd.RangeIndex(start=35000, stop=35000 + len(df_fp), step=1)
    plt.bar(new_index, df_fp[f'index after subsidy'], 
            label=f'Inequality index after subsidy',  
            color=colors_normalized[0], 
            alpha = 1, align='edge', width=1)
    
    # ax.text(70000, 7.4, '2019 prices',
    #         weight='bold',
    #         fontsize=12, 
    #         color=colors_normalized[0], 
    #         alpha = 0.6, ha = 'left')
    
    ax.text(-2000, 5.5, f'Total funding amount: \n£{rounded_number} million',
            weight='bold',
            fontsize=12, 
            color='black', 
            alpha = 1, ha = 'left')

    # average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].mean() for i in range(0, len(df_fp), n)]
    # ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='dotted', color='black')

    # average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].std() / 10 for i in range(0, len(df_fp), n)]
    # ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', color='black')

    # Adding labels and title
    plt.xlabel('Regions arranged by increasing proportion of fuel poverty (2019, left to right)')
    ax.set_xticks([])
    ax.set_ylim(-0.5, 8)
    ax.set_yticks([0, 4, 8])
    plt.ylabel('Inequality index (-)')
    # ax.text(50, 2.3, 'Increased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    # ax.annotate(
    #             '',
    #             xy=(0, 0.5),
    #             xytext=(0, 2),
    #             arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
    #             fontsize=12,
    #             color=colors_normalized[-1],
    #             ha='center',  # Horizontal alignment set to 'center'
    #             va='center',  # Vertical alignment set to 'center'
    #         )
    ax.annotate(
                '',
                xy=(30000, -0.5),
                xytext=(5000, -0.5),
                arrowprops=dict(arrowstyle='->', lw=2, color='black'),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    
    ax.annotate(
                '',
                xy=(65000, -0.5),
                xytext=(40000, -0.5),
                arrowprops=dict(arrowstyle='->', lw=2, color='black'),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    # ax.text(50, -3, 'Decreased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    # ax.annotate(
    #             '',
    #             xy=(50, -0.5),
    #             xytext=(50, -2),
    #             arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
    #             fontsize=12,
    #             color=colors_normalized[0],
    #             ha='center',  # Horizontal alignment set to 'center'
    #             va='center',  # Vertical alignment set to 'center'
    #         )
    
    # ax.text(25000, -3.5, '2022 prices',
    #         weight='bold',
    #         fontsize=12, 
    #         color=colors_normalized[-1], 
    #         alpha = 0.6, ha = 'left')
    
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.legend(frameon=False,loc = 'upper left')
    plt.subplots_adjust(bottom=0.08, top = 0.97, left= 0.05, right= 0.98)

    save_figures(f'./Inequality index bar chart 2019 after subsidy', False)

def subsidy_plot_2022_density():

    global df_fp
    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        STD_delta_cost = np.nanstd(delta_cost)
        df_fp[f'index @{ratio}'] = index
    
    subsidy_array = np.zeros_like(delta_cost)
    inequality_target = 1
    fp_norm = calculate_normalized_index(fp,0,20)
    for i in range(len(delta_cost)):
            if index[i] > inequality_target:
                subsidy_array[i] = delta_cost[i]  - STD_delta_cost *  inequality_target / fp_norm[i]
    
    delta_cost = calculate_delta_cost(ratio_2022, delta_elect_array, delta_gas_array, True)
    delta_cost = delta_cost - subsidy_array
    total_subsidy = subsidy_array * meters
    total_subsidy = np.nansum(total_subsidy, axis=0)
    rounded_number = round(total_subsidy / 1000000)
    # delta_cost = np.sum(delta_cost, axis=1)
    # fp_subsidy = np.zeros_like(delta_cost)
    # fp_baseline = 5
    # for i in range(len(fp_subsidy)):
    #     if fp[i] < fp_baseline:
    #         fp_subsidy[i] = 0 
    #     else:
    #         fp_subsidy[i]= (fp[i] - fp_baseline) * 1.5
    # delta_cost = delta_cost - fp_subsidy
    delta_cost = (delta_cost) / STD_delta_cost
    index_after_subsidy = delta_cost*fp_norm
    df_fp[f'index after subsidy'] = index_after_subsidy
    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)
    df_fp= df_fp.dropna()
    df_fp = df_fp.reset_index(drop=True)

    # Select relevant columns from df_fp
    df = df_fp[['index after subsidy', f'index @{ratio_list[1]}']]

    # Convert columns to arrays
    index_col = df[f'index @{ratio_list[1]}'].to_numpy()
    subsidy_col = df['index after subsidy'].to_numpy()

    # Set the style for seaborn (white background with ticks)
    sns.set(style="white", rc={"axes.facecolor": (0, 0, 0, 0)})

    # Create the ridge plot
    plt.figure(figsize=(4, 3))
    sns.kdeplot(data=df[f'index @{ratio_list[1]}'], color=colors_normalized[-1], fill=True, alpha=0.5, linewidth=3)
    sns.kdeplot(data=df['index after subsidy'], color=colors_normalized[0], fill=True, alpha=0.5, linewidth=3)

    # Set labels and limits
    plt.ylabel('Density')
    plt.yticks([])
    plt.xticks([-4, 0, 4])
    plt.xlabel('Inequality index (-)')
    plt.ylim(0, 1)
    plt.xlim(-4, 6)
    sns.despine(left=False, right=True, top=True)
    plt.subplots_adjust(bottom=0.18, top=0.97, left=0.06, right=0.98)

    save_figures(f'./Inequality index bar chart 2022 after subsidy density', False)

def subsidy_plot_2022_bar():
    
    global df_fp
    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        STD_delta_cost = np.nanstd(delta_cost)
        df_fp[f'index @{ratio}'] = index

    subsidy_array = np.zeros_like(delta_cost)
    inequality_target = 1
    fp_norm = calculate_normalized_index(fp,0,20)
    for i in range(len(delta_cost)):
            if index[i] > inequality_target:
                subsidy_array[i] = delta_cost[i]  - STD_delta_cost *  inequality_target / fp_norm[i]
    
    delta_cost = calculate_delta_cost(ratio_2022, delta_elect_array, delta_gas_array, True)
    delta_cost = delta_cost - subsidy_array
    total_subsidy = subsidy_array * meters
    total_subsidy = np.nansum(total_subsidy, axis=0)
    rounded_number = round(total_subsidy / 1000000)
    delta_cost = (delta_cost) / STD_delta_cost
    index_after_subsidy = delta_cost*fp_norm
    df_fp[f'index after subsidy'] = index_after_subsidy
    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')

    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)
    df_fp= df_fp.dropna()
    df_fp = df_fp.reset_index(drop=True)

    # Plotting
    fig, ax = plt.subplots(figsize = (9,4))
    # # Line chart
    # n = 100

    plt.bar(data=df_fp, x = df_fp.index, height = df_fp[f'index @{ratio_list[1]}'], 
            label=f'Inequality index before subsidy',  
            color=colors_normalized[-1], )
            # alpha = 1, align='edge',width=1)
    new_index = df_fp.index + 35000
    # new_index = pd.RangeIndex(start=35000, stop=35000 + len(df_fp), step=1)
    plt.bar(data=df_fp, x = new_index, height =  df_fp[f'index after subsidy'], 
            label=f'Inequality index after subsidy',  
            color=colors_normalized[0], )
            # alpha = 1, align='edge', width=1)
    
    ax.text(53000, 5.5, '2022 prices',
            weight='bold',
            fontsize=12, 
            color=colors_normalized[0], 
            alpha = 0.6, ha = 'left')
    
    ax.text(-2000, 3.1, f'Total funding amount: \n£{rounded_number} million',
            weight='bold',
            fontsize=12, 
            color='black', 
            alpha = 1, ha = 'left')

    # average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].mean() for i in range(0, len(df_fp), n)]
    # ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='dotted', color='black')

    # average_indices = [df_fp[f'index @{ratio_list[0]}'][:i+1].std() / 10 for i in range(0, len(df_fp), n)]
    # ax.plot(range(0, len(df_fp), n), average_indices, markersize=1, alpha = 1, linestyle='-', color='black')

    # Adding labels and title
    plt.xlabel('Regions arranged by increasing proportion of fuel poverty (2019, left to right)')
    ax.set_xticks([])
    ax.set_ylim(-4, 6)
    ax.set_yticks([-4, 0, 4])
    plt.ylabel('Inequality index (-)')
    # ax.text(50, 2.3, 'Increased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    # ax.annotate(
    #             '',
    #             xy=(0, 0.5),
    #             xytext=(0, 2),
    #             arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
    #             fontsize=12,
    #             color=colors_normalized[-1],
    #             ha='center',  # Horizontal alignment set to 'center'
    #             va='center',  # Vertical alignment set to 'center'
    #         )
    ax.annotate(
                '',
                xy=(30000, -4),
                xytext=(5000, -4),
                arrowprops=dict(arrowstyle='->', lw=2, color='black'),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    
    
    ax.annotate(
                '',
                xy=(65000, -4),
                xytext=(40000, -4),
                arrowprops=dict(arrowstyle='->', lw=2, color='black'),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
    # ax.text(50, -3, 'Decreased domestic\nheating cost', fontsize=12, color=colors_normalized[0], ha = 'left')
    # ax.annotate(
    #             '',
    #             xy=(50, -0.5),
    #             xytext=(50, -2),
    #             arrowprops=dict(arrowstyle='<-', lw=2, color=colors_normalized[0]),
    #             fontsize=12,
    #             color=colors_normalized[0],
    #             ha='center',  # Horizontal alignment set to 'center'
    #             va='center',  # Vertical alignment set to 'center'
    #         )
    
    # ax.text(25000, -3.5, '2022 prices',
    #         weight='bold',
    #         fontsize=12, 
    #         color=colors_normalized[-1], 
    #         alpha = 0.6, ha = 'left')
    
    ax.spines['bottom'].set_visible(False)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    plt.legend(frameon=False,loc = 'upper left')
    plt.subplots_adjust(bottom=0.08, top = 0.97, left= 0.07, right= 1)

    save_figures(f'./Inequality index bar chart 2022 after subsidy', False)

def two_vertical_scatter_plot():

    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost,'std')
        final.append(index)

    y_data_1 = final[0]
    y_data_2 = final[1]
    x0_data = [0 for _ in y_data_1]
    x1_data = [1 for _ in y_data_1]
    fig, ax2 = plt.subplots(figsize=(3, 5))

    # Scatter plot for the first y-axis
    scatter1 = ax2.scatter(x0_data, y_data_1, c=fp, marker='o', s=50, cmap=custom_cmap, alpha=0.5)

    # Scatter plot for the second y-axis
    # ax2.scatter(x1_data, y_data_2,marker='o', s=50, facecolors='none', edgecolors='black', alpha= 0.3)
    ax2.scatter(x1_data, y_data_2, c=fp, marker='o', s=50, cmap=custom_cmap, alpha=0.5)
    cbar = plt.colorbar(scatter1)
    cbar.set_label('Fuel Poverty Proportion (%)')

    # Connect the points with vertical lines
    for y1, y2 in zip(y_data_1, y_data_2):
        abs_diff = abs(y1 - y2)
        # if abs_diff <= 1:
        #     ax2.plot([0, 1], [y1, y2], color='red', linestyle='solid', linewidth=0.5, zorder=1)
        if abs_diff >= 15:
            ax2.plot([0, 1], [y1, y2], color='red', linestyle='solid', linewidth=0.5, alpha=0.5, zorder=3)
        else:
            ax2.plot([0, 1], [y1, y2], color=colors_normalized[2], linestyle='solid', linewidth=0.5, alpha=0.3, zorder=2)

    # Box plot for the first y-axis (right of the scatter plot)
    y_data_1 = [x for x in y_data_1 if not math.isnan(x)]
    y_data_2 = [x for x in y_data_2 if not math.isnan(x)]
    # ax2.boxplot(y_data_1, positions =[-0.2],  widths=0.1, showfliers=False,)
    # ax2.boxplot(y_data_2, positions =[1.2], widths=0.1, showfliers=False,)
    violin1 = ax2.violinplot(dataset=y_data_1, positions=[-0.2], widths=0.1, showmedians=True)
    violin2 = ax2.violinplot(dataset=y_data_2, positions=[1.2], widths=0.1, showmedians=True)
    # Set individual colors for the violins
    for i, pc in enumerate(violin1['bodies']):
        pc.set_facecolor(colors_normalized[1])
        pc.set_alpha(0.5)
    for i, pc in enumerate(violin2['bodies']):
        pc.set_facecolor(colors_normalized[-1])
        pc.set_alpha(0.5)
    # Set the alpha (transparency) of the violins
    violin1['bodies'][0].set_alpha(0.7)
    violin2['bodies'][0].set_alpha(0.7)
    ax2.set_xlim(-0.5,1.5)
    ax2.set_xticks([0,1])
    ax2.set_xticklabels(['2019','2022'])
    ax2.set_ylabel('Inequality Index (-)')
    plt.subplots_adjust(left=0.25, right=0.85)

    save_figures(f'./two vertical scatter plot', False)

def two_vertical_scatter_plot_identify_dependency_change_of_cost_proportion():

    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        # index = calculate_standarizationed_index(delta_cost)
        final.append(cost_ratio * 100)

    y_data_1 = final[0]
    y_data_2 = final[1]
    x0_data = [0 for _ in y_data_1]
    x1_data = [1 for _ in y_data_1]
    fig, ax2 = plt.subplots(figsize=(11, 8))
    
    gs = gridspec.GridSpec(5, 11, height_ratios=[10, 1,1,1,1])
    ax2 = plt.subplot(gs[0, :11])
    # Position the colorbars subplot
    cbar_ax_1 = plt.subplot(gs[2, 0:5]) 
    cbar_ax_2 = plt.subplot(gs[2, 6:11]) 
    cbar_ax_3 = plt.subplot(gs[4, 0:5]) 
    cbar_ax_4 = plt.subplot(gs[4, 6:11]) 

    # Scatter plot for the first y-axis
    scatter1 = ax2.scatter(x0_data, y_data_1, c=colors_normalized[2], marker='o', s=1, alpha=1)

    # Scatter plot for the second y-axis
    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    ax2.scatter(x1_data, y_data_2, c=colors_normalized[2], marker='o', s=1, alpha=1)
    
    shift = [np.random.uniform(-0.4, 0.4) for _ in y_data_1]
    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter2 =ax2.scatter([-1 + data for data in shift], y_data_1, c=np.sum(elec_consump,axis=1), marker='o',
                           s=1, cmap=custom_cmap, alpha=1,vmin=2900, vmax=3900)
    cbar1 = plt.colorbar(scatter2,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
    cbar1.set_label('Electricity/Consumption (KWh/Household)')
    cbar_ax_1.ticklabel_format(style="sci", scilimits=(0,0))

    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter3 =ax2.scatter([-2 + data for data in shift], y_data_1, c=np.sum(gas_consump,axis=1), marker='o', 
                          s=1, cmap=custom_cmap, alpha=1,vmin=9000, vmax=16000)
    cbar2 = plt.colorbar(scatter3,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_2)
    cbar2.set_label('Gas Consumption (KWh/Household)')
    cbar_ax_2.ticklabel_format(style="sci", scilimits=(0,0))

    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter4 =ax2.scatter([-3 + data for data in shift], y_data_1, c=np.mean(cop, axis=1), marker='o',
                           s=1, cmap=custom_cmap, alpha=1,vmin=2.9, vmax=3.5)
    cbar3 = plt.colorbar(scatter4,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_3)
    cbar3.set_label('Average COP (-)')

    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter5 =ax2.scatter([-4 + data for data in shift], y_data_1, c=fp, marker='o', s=1, 
                          cmap=custom_cmap, alpha=1, vmin=0, vmax=30)
    cbar4 = plt.colorbar(scatter5,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_4)
    cbar4.set_label('Fuel Poverty Proportion (%)')

    # Connect the points with vertical lines
    for y1, y2 in zip(y_data_1, y_data_2):
        abs_diff = abs(y1 - y2)
        # if abs_diff <= 1:
        #     ax2.plot([0, 1], [y1, y2], color='red', linestyle='solid', linewidth=0.5, zorder=1)
        if abs_diff >= 14:
            ax2.plot([0, 1], [y1, y2], color='red', linestyle='solid', linewidth=0.5, alpha=0.5, zorder=3)
        else:
            ax2.plot([0, 1], [y1, y2], color=colors_normalized[-3], linestyle='solid', linewidth=0.5, alpha=0.3, zorder=2)

    ax2.set_xlim(-5,1.5)
    ax2.set_xticks([-4, -3,-2,-1, 0, 1])
    ax2.set_xticklabels(['Fuel Poverty\nProportion (%)',
                         'Average COP (-)',
                         'Gas\nConsumption\n(KWh/Household)',
                         'Electricity\nConsumption\n(KWh/Household)',
                         '2019\n(High Price Ratio)',
                         '2022\n(Low  Price Ratio)'])
    ax2.set_ylabel(' Normalized \u0394C (%)')
    plt.subplots_adjust(left=0.075, right=0.975)

    save_figures(f'./two vertical scatter plot_change_in_cost_ratio', False)

def two_vertical_scatter_plot_identify_dependency_inequality_index():

    ratio_list = [ratio_2019, ratio_2022]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost, 'std')
        final.append(index)

    y_data_1 = final[0]
    y_data_2 = final[1]
    x0_data = [0 for _ in y_data_1]
    x1_data = [1 for _ in y_data_1]
    fig, ax2 = plt.subplots(figsize=(11, 8))
    
    gs = gridspec.GridSpec(5, 11, height_ratios=[10, 1,1,1,1])
    ax2 = plt.subplot(gs[0, :11])
    # Position the colorbars subplot
    cbar_ax_1 = plt.subplot(gs[2, 0:5]) 
    cbar_ax_2 = plt.subplot(gs[2, 6:11]) 
    cbar_ax_3 = plt.subplot(gs[4, 0:5]) 
    cbar_ax_4 = plt.subplot(gs[4, 6:11]) 

    # Scatter plot for the first y-axis
    scatter1 = ax2.scatter(x0_data, y_data_1, c=colors_normalized[2], marker='o', s=1, alpha=1)

    # Scatter plot for the second y-axis
    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    ax2.scatter(x1_data, y_data_2, c=colors_normalized[2], marker='o', s=1, alpha=1)
    shift = [np.random.uniform(-0.4, 0.4) for _ in y_data_1]
    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter2 =ax2.scatter([-1 + data for data in shift], y_data_1, c=np.sum(elec_consump,axis=1), marker='o',
                           s=1, cmap=custom_cmap, alpha=1,vmin=2900, vmax=3900)
    cbar1 = plt.colorbar(scatter2,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
    cbar1.set_label()
    cbar_ax_1.ticklabel_format(style="sci", scilimits=(0,0))

    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter3 =ax2.scatter([-2 + data for data in shift], y_data_1, c=np.sum(gas_consump,axis=1), marker='o', 
                          s=1, cmap=custom_cmap, alpha=1,vmin=9000, vmax=16000)
    cbar2 = plt.colorbar(scatter3,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_2)
    cbar2.set_label('Gas Consumption (KWh/Household)')
    cbar_ax_2.ticklabel_format(style="sci", scilimits=(0,0))

    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter4 =ax2.scatter([-3 + data for data in shift], y_data_1, c=np.mean(cop, axis=1), marker='o',
                           s=1, cmap=custom_cmap, alpha=1,vmin=2.9, vmax=3.5)
    cbar3 = plt.colorbar(scatter4,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_3)
    cbar3.set_label('Average COP (-)')

    # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
    scatter5 =ax2.scatter([-4 + data for data in shift], y_data_1, c=fp, marker='o', s=1, 
                          cmap=custom_cmap, alpha=1, vmin=0, vmax=30)
    cbar4 = plt.colorbar(scatter5,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_4)
    cbar4.set_label('Fuel Poverty Proportion (%)')

    # Connect the points with vertical lines
    for y1, y2 in zip(y_data_1, y_data_2):
        abs_diff = abs(y1 - y2)
        # if abs_diff <= 1:
        #     ax2.plot([0, 1], [y1, y2], color='red', linestyle='solid', linewidth=0.5, zorder=1)
        if abs_diff >= 15:
            ax2.plot([0, 1], [y1, y2], color='red', linestyle='solid', linewidth=0.5, alpha=0.5, zorder=3)
        else:
            ax2.plot([0, 1], [y1, y2], color=colors_normalized[-3], linestyle='solid', linewidth=0.5, alpha=0.3, zorder=2)

    ax2.set_xlim(-5,1.5)
    ax2.set_xticks([-4, -3, -2, -1, 0, 1])
    ax2.set_xticklabels(['Fuel Poverty\nProportion (%)',
                         'Average COP (-)',
                         'Gas\nConsumption\n(KWh/Household)',
                         'Electricity\nConsumption\n(KWh/Household)',
                         '2019\n(High Price Ratio)',
                         '2022\n(Low  Price Ratio)'])
    ax2.set_ylabel('Inequality Index (-)')
    plt.subplots_adjust(left=0.075, right=0.975)

    save_figures(f'./two vertical scatter plot_inequality_index', False)

def two_vertical_scatter_plot_identify_dependency_change_of_cost_proportion_on_ratio():

    ratio_list = [3.5, 3.75, 4, 4.25, 4.5, 4.75]
    data_list = []
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        data_list.append(cost_ratio*100)

    fig, ax2 = plt.subplots(figsize=(11, 8))
    gs = gridspec.GridSpec(2, 11, height_ratios=[10, 1])
    ax2 = plt.subplot(gs[0, :11])
    # Position the colorbars subplot
    cbar_ax_1 = plt.subplot(gs[1, 2:9]) 
    shift = [np.random.uniform(-0.1, 0.1) for _ in data_list[0]]

    for i in range(6):
        # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
        scatter4 =ax2.scatter([ratio_list[i] + data for data in shift], data_list[i], c=np.mean(cop, axis=1), marker='o',
                            s=1, cmap=custom_cmap, alpha=1,vmin=2.9, vmax=3.5)
        if i == 5:
            cbar3 = plt.colorbar(scatter4,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
            cbar3.set_label('Average COP (-)')
    '''
        scatter2 =ax2.scatter([-ratio_list[0] + data for data in shift], data_list[0], c=np.sum(elec_consump,axis=1), marker='o',
                            s=1, cmap=custom_cmap, alpha=1,vmin=2900, vmax=3900)
    cbar1 = plt.colorbar(scatter2,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
    cbar1.set_label('Electricity/Consumption (KWh/Household)')
    cbar_ax_1.ticklabel_format(style="sci", scilimits=(0,0))

        # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
        scatter3 =ax2.scatter([-2 + data for data in shift], y_data_1, c=np.sum(gas_consump,axis=1), marker='o', 
                            s=1, cmap=custom_cmap, alpha=1,vmin=9000, vmax=16000)
        cbar2 = plt.colorbar(scatter3,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
        cbar2.set_label('Gas Consumption (KWh/Household)')
        cbar_ax_1.ticklabel_format(style="sci", scilimits=(0,0))


        # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
        scatter5 =ax2.scatter([-4 + data for data in shift], y_data_1, c=fp, marker='o', s=1, 
                            cmap=custom_cmap, alpha=1, vmin=0, vmax=30)
        cbar4 = plt.colorbar(scatter5,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
        cbar4.set_label('Fuel Poverty Proportion (%)')

    '''
    ax2.set_xlim(3.25,5)
    ax2.set_xticks([3.5, 3.75, 4, 4.25, 4.5, 4.75])
    ax2.set_xlabel('Price Ratio (-)')
    ax2.set_ylabel('Normalzed \u0394C (%)')
    ax2.set_yticks([-5, 0, 5, 10])
    plt.subplots_adjust(left=0.075, right=0.975)

    save_figures(f'./two vertical scatter plot_change_in_cost_ratio_versus ratio', False)

def two_vertical_scatter_plot_identify_dependency_inequality_index_on_ratio():

    ratio_list = [3.5, 3.75, 4, 4.25, 4.5, 4.75]
    data_list = []
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp, delta_cost, 'std')
        data_list.append(index)

    fig, ax2 = plt.subplots(figsize=(9, 5))
    gs = gridspec.GridSpec(3, 11, height_ratios=[10, 1, 1])
    ax2 = plt.subplot(gs[0, :11])
    # Position the colorbars subplot
    cbar_ax_1 = plt.subplot(gs[2, 2:9]) 
    shift = [np.random.uniform(-0.1, 0.1) for _ in data_list[0]]

    limit = 30000
    to_be_zipped = np.sum(gas_consump,axis=1)
    for i in range(6):
        scatter_low = ax2.scatter([ratio_list[i] + data for data, cop in zip(shift,to_be_zipped) if cop <= limit],
                                [y for y, cop in zip(data_list[i],  to_be_zipped) if cop <= limit],
                                c=[cop for cop in to_be_zipped if cop <= limit] ,
                                marker='o', s=1, cmap=custom_cmap, alpha=0.5, vmin=9000, vmax=16000)

        # Scatter plot for points with values less than or equal to 3500
        scatter_high = ax2.scatter([ratio_list[i] + data for data, cop in zip(shift, to_be_zipped) if cop >=limit],
                                [y for y, cop in zip(data_list[i],  to_be_zipped) if cop >= limit],
                                c=[cop for cop in to_be_zipped if cop >= limit] ,
                                marker='o', s=1, cmap=custom_cmap, alpha=0.5, vmin=9000, vmax=16000)
        
        if i == 5:
            cbar2 = plt.colorbar(scatter_low,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
            cbar2.set_label('Gas Consumption (KWh/Household)')
            cbar_ax_1.ticklabel_format(style="sci", scilimits=(0,0))
        
    # Connect the points with vertical lines
    for i in range(5):
        x1_list = [ratio_list[i] + data for data, fp_s in zip(shift, to_be_zipped) if not np.isnan(fp_s) and fp_s >= limit]
        x2_list = [ratio_list[i+1] + data for data, fp_s in zip(shift, to_be_zipped) if not np.isnan(fp_s) and fp_s >= limit]
        y1_list = [y for y, fp_s in zip(data_list[i],  to_be_zipped) if not np.isnan(fp_s) and fp_s >= limit]
        y2_list = [y for y, fp_s in zip(data_list[i+1],  to_be_zipped) if not np.isnan(fp_s) and fp_s >= limit]
        j=0
        for y1, y2 in zip(y1_list, y2_list):
            ax2.plot([x1_list[j], x2_list[j]], [y1, y2], color=colors_normalized[-1], linestyle='solid', linewidth=1, alpha=0.3, zorder=3)
            j+=1
    '''
        scatter_low = ax2.scatter([ratio_list[i] + data for data, cop in zip(shift, np.mean(cop, axis=1)) if cop <= 3.49],
                                [y for y, cop in zip(data_list[i],  np.mean(cop, axis=1)) if cop <= 3.49],
                                c=[cop for cop in np.mean(cop, axis=1) if cop <= 3.49] ,
                                marker='o', s=1, cmap=custom_cmap, alpha=0.5, vmin=3.1, vmax=3.5)

        # Scatter plot for points with values less than or equal to 3500
        scatter_high = ax2.scatter([ratio_list[i] + data for data, cop in zip(shift, np.mean(cop, axis=1)) if cop >= 3.49],
                                [y for y, cop in zip(data_list[i],  np.mean(cop, axis=1)) if cop >= 3.49],
                                c=[cop for cop in np.mean(cop, axis=1) if cop >= 3.49] ,
                                marker='o', s=1, cmap=custom_cmap, alpha=0.5, vmin=3.1, vmax=3.5)
        if i == 5:
            cbar3 = plt.colorbar(scatter_low, shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
            cbar3.set_label('Average COP (-)')
    '''
    '''
        # Scatter plot for points with values less than or equal to 3500
        scatter_low =ax2.scatter([ratio_list[i] + data for data, fp_s in zip(shift, fp) if not np.isnan(fp_s) and fp_s <= limit],
                                [y for y, fp_s in zip(data_list[i],  fp) if not np.isnan(fp_s) and fp_s <= limit],
                                c= [fp_s for fp_s in fp if not np.isnan(fp_s) and fp_s <= limit] ,
                                marker='o', s=1, cmap=custom_cmap, alpha=0.5,  vmin=0, vmax=30)
        
        scatter_high =ax2.scatter([ratio_list[i] + data for data, fp_s in zip(shift, fp) if not np.isnan(fp_s) and fp_s >= limit],
                                [y for y, fp_s in zip(data_list[i],  fp) if not np.isnan(fp_s) and fp_s >= limit],
                                c= [fp_s for fp_s in fp if not np.isnan(fp_s) and fp_s >= limit] ,
                                marker='o', s=1, cmap=custom_cmap, alpha=0.5,  vmin=0, vmax=30)
        
        if i == 5:
            cbar4 = plt.colorbar(scatter_low,shrink=0.5, orientation='horizontal', pad=0.2, cax=cbar_ax_1)
            cbar4.set_label('Fuel Poverty Proportion (%)')
    
    '''
    '''

        # ax2.scatter(x1_data, y_data_2,marker='o', s=1, facecolors='none', edgecolors='black', alpha= 0.3)
        scatter4 =ax2.scatter([ratio_list[i] + data for data in shift], data_list[i], c=np.mean(cop, axis=1), marker='o',
                            s=1, cmap=custom_cmap, alpha=0.5,vmin=3.1, vmax=3.5)

        scatter2 =ax2.scatter([-ratio_list[0] + data for data in shift], data_list[0], c=np.sum(elec_consump,axis=1), marker='o',
                            s=1, cmap=custom_cmap, alpha=1,vmin=2900, vmax=3900)
    cbar1 = plt.colorbar(scatter2,shrink=0.7, orientation='horizontal', pad=0.1, cax=cbar_ax_1)
    cbar1.set_label('Electricity/Consumption (KWh/Household)')
    cbar_ax_1.ticklabel_format(style="sci", scilimits=(0,0))


    '''
    '''
        x1_list = [ratio_list[i] + data for data, cop in zip(shift, np.mean(cop, axis=1)) if cop >= 35]
        x2_list = [ratio_list[i+1] + data for data, cop in zip(shift, np.mean(cop, axis=1)) if cop >= 35]
        y1_list = [y for y, cop in zip(data_list[i],  np.mean(cop, axis=1)) if cop >= 35]
        y2_list = [y for y, cop in zip(data_list[i+1],  np.mean(cop, axis=1)) if cop >= 35]
    
        
        x1_list = [ratio_list[i] + data for data, fp_s in zip(shift, fp) if not np.isnan(fp_s) and fp_s >= limit]
        x2_list = [ratio_list[i+1] + data for data, fp_s in zip(shift, fp) if not np.isnan(fp_s) and fp_s >= limit]
        y1_list = [y for y, fp_s in zip(data_list[i],  fp) if not np.isnan(fp_s) and fp_s >= limit]
        y2_list = [y for y, fp_s in zip(data_list[i+1],  fp) if not np.isnan(fp_s) and fp_s >= limit]
    '''
    ax2.set_xlim(3.25,5)
    ax2.set_xticks([3.5, 3.75, 4, 4.25, 4.5, 4.75])
    ax2.set_yticks([-20, -10, 0, 10])
    ax2.set_xlabel('Price Ratio (-)')
    ax2.set_ylabel('Inequality Index (-)')
    # ax2.set_yticks([-5, 0, 5, 10])
    plt.subplots_adjust(left=0.075, right=0.975)

    save_figures(f'./two vertical scatter plot gas consump versus Inequality', False)

def box_whisker_plot_standard_deviation_and_median():

    def plot_box_and_whisker_for_muiltiple_entities_a(filename: str, y_label: str, df_in: pd.DataFrame, pdf = False) :
        
        flierprops = dict(markerfacecolor="k", markersize=0.05, linestyle="none", markeredgecolor="k")
        
        df = copy.deepcopy(df_in)
        # Initialize the plot
        fig, axs = plt.subplots(1,1,figsize=(7,4))
        plt.tight_layout()

        y_data = df.iloc[:, 1:]
        plt.subplots_adjust(
            left=0.1, bottom=0.15, right=0.967, top=0.949, wspace=0.2, hspace=0.14
        )
        axs.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
        num_columns = df.shape[1] - 1
        # Add density plot  
        ax_box = sb.boxplot(
            x="variable", y="value", data=pd.melt(y_data),
            fliersize=50,
            whis=0,
            linewidth=1.5,
            ax=axs,
            color='w',
            flierprops=flierprops,
            zorder = 1,
            width=0.5,
            showfliers= False
        )
        
        pointplot = sb.pointplot(
                x="variable", y="value", data=pd.melt(get_median(df_in=df)),
                ax=axs,
                color=colors_normalized[-1],
                markers=".",
                capsize=5
            )
        for i, box in enumerate(ax_box.artists):
            box.set_edgecolor(colors_normalized[-2])
            # box.set_facecolor("white")
            # iterate over whiskers and median lines
            for j in range(6 * i, 6 * (i + 1)):
                ax_box.lines[j].set_color(colors_normalized[-2])
            
            # Adjust the line width
            for line in pointplot.lines:
                line.set_linewidth(2)

        sb.set_style("whitegrid")
        violin = sb.violinplot(data=y_data, inner=None, cut=0, linewidth=0.1, 
                      ax=axs, color=colors_normalized[-1])
        # alpha_range = [ 0.8, 0.8, 0.8, 0.8, 0.8, 0.8]
        alpha_range = [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
        i = 0
        for patch in violin.collections[1:]:
            patch.set_alpha(alpha_range[i])  # Adjust the transparency of the violin body
            i+=1
            # x = patch.get_paths()[0].vertices[:, 0]
            # y = patch.get_paths()[0].vertices[:, 1]
            # stick_range = np.linspace(min(y), max(y), num=2)  # Adjust the number of stick segments
            # for stick_y in stick_range:
            #     axs.plot(x, [stick_y] * len(x), color="black", linewidth=1, alpha=1)

        # melted_df = pd.melt(y_data, var_name='Column', value_name='Value')
        # scatter = sb.scatterplot(x='Column', y='Value', data=melted_df, c='black', edgecolor='None', s=5)

        column_indices = list(range(num_columns))
        axs.set_xticks(column_indices)
        axs.set_xticklabels(labels=df.columns[1:])
        axs.set_xlabel("Price Ratio (-)")
        axs.set_ylabel(y_label)

        # Set y-axis as logarithmic scale with magnitude of 2
        axs.set_yscale("symlog", base=4)
        y_ticks = [-16, -4, -1, 0, 1, 4, 16]
        # y_ticks = [-1, 0, 1]
        axs.set_yticks(y_ticks)
        axs.set_yticklabels([str(y) if y != 0 else "0" for y in y_ticks])

        save_figures(f'./{filename}', pdf)
    # 
    ratio_list = [3.5, 3.75, 4, 4.25, 4.5, 4.75]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_original_tom(fp, delta_cost)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        df_ref[f'{ratio}'] = index

    
    df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    plot_box_and_whisker_for_muiltiple_entities_a(f'index by std',
                                                f'Inequality Index (-)', df_ref, pdf = False)

def plot_box_and_whisker_validation_for_index(ratio):

    def calculate_delta_cost_on_scale(ratio, scale, delta_elect_array, delta_gas_array):
    
        cost_gas =  read_from_web_price_gas(year) * scale
        # cost_gas =  cost_elec / ratio
        cost_elec =  cost_gas * ratio

        delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
        delta_cost = np.sum(delta_cost, axis=1)

        return delta_cost
    
    scaling_factor_list = [0.5, 0.7, 0.9, 1.1, 1.3, 1.5]
    for scaling_factor in scaling_factor_list:
        delta_cost = calculate_delta_cost_on_scale(ratio, scaling_factor, delta_elect_array, delta_gas_array)
        # index = calculate_compared_index(fp, delta_cost)
        index = calculate_index_normalization_variation(fp, delta_cost,'std')
        df_ref[f'{scaling_factor}'] = index

    flierprops = dict(markerfacecolor="black", markersize=0.05, linestyle="none", markeredgecolor="black")
    # Initialize the plot
    fig, axs = plt.subplots(1,1,figsize=(5,4))
    plt.tight_layout()
    y_data = df_ref.iloc[:, 1:]
    if ratio == 4:
        ax_box = sb.boxplot(
            x="variable", y="value", data=pd.melt(y_data),
            fliersize=0.05,
            whis=2,
            linewidth=1.2,
            ax=axs,
            color=(253/255, 200/255, 0),
            flierprops=flierprops,
        )
    
    elif ratio == 5:
        ax_box = sb.boxplot(
            x="variable", y="value", data=pd.melt(y_data),
            fliersize=0.05,
            whis=2,
            linewidth=1.2,
            ax=axs,
            color=colors_normalized[0],
            flierprops=flierprops,
        )

    for i, box in enumerate(ax_box.artists):
        box.set_edgecolor("black")
        # box.set_facecolor("white")
        # iterate over whiskers and median lines
        for j in range(6 * i, 6 * (i + 1)):
            ax_box.lines[j].set_color("black")
    plt.subplots_adjust(
        left=0.12, bottom=0.12, right=0.967, top=0.949, wspace=0.2, hspace=0.14
    )
    axs.set_xlabel("Scaling factor (-)")
    axs.set_ylabel('Inequality index (-)')
    axs.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    num_columns = df_ref.shape[1] - 1
    column_indices = list(range(num_columns))
    axs.set_xticks(column_indices)
    axs.set_xticklabels(labels = df_ref.columns[1:])
    save_figures(f'./demonstrate index valid @ratio{ratio}', False)

def plot_box_and_whisker_validation_for_change_of_cost_ratio(ratio):

    def calculate_delta_cost_on_scale(ratio, scale, delta_elect_array, delta_gas_array):
    
        cost_gas =  read_from_web_price_gas(year) * scale
        # cost_gas =  cost_elec / ratio
        cost_elec =  cost_gas * ratio

        delta_cost = cost_elec * delta_elect_array - cost_gas * delta_gas_array
        delta_cost = np.sum(delta_cost, axis=1)

        return delta_cost
    
    def calculate_current_cost_on_scale(ratio, scale, elec_consump, gas_consump):
    
        cost_gas =  read_from_web_price_gas(year) * scale
        # cost_gas =  cost_elec / ratio
        cost_elec =  cost_gas * ratio

        cost = cost_elec * elec_consump + cost_gas * gas_consump
        cost = np.sum(cost, axis=1)

        return cost
    
    scaling_factor_list = [0.5, 0.7, 0.9, 1.1, 1.3, 1.5]
    for scaling_factor in scaling_factor_list:
        delta_cost = calculate_delta_cost_on_scale(ratio, scaling_factor, delta_elect_array, delta_gas_array)
        cost = calculate_current_cost_on_scale(ratio, scaling_factor, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        # index = calculate_standarizationed_index(delta_cost)
        df_ref[f'{scaling_factor}'] = cost_ratio * 100
    
    flierprops = dict(markerfacecolor="black", markersize=0.05, linestyle="none", markeredgecolor="black")
    # Initialize the plot
    fig, axs = plt.subplots(1,1,figsize=(5,4))
    plt.tight_layout()
    y_data = df_ref.iloc[:, 1:]
    if ratio == 4:
        ax_box = sb.boxplot(
            x="variable", y="value", data=pd.melt(y_data),
            fliersize=0.05,
            whis=2,
            linewidth=1.2,
            ax=axs,
            color=(253/255, 200/255, 0),
            flierprops=flierprops,
        )
    
    elif ratio == 5:
        ax_box = sb.boxplot(
            x="variable", y="value", data=pd.melt(y_data),
            fliersize=0.05,
            whis=2,
            linewidth=1.2,
            ax=axs,
            color=colors_normalized[0],
            flierprops=flierprops,
        )

    for i, box in enumerate(ax_box.artists):
        box.set_edgecolor("black")
        box.set_facecolor("white")
        # iterate over whiskers and median lines
        for j in range(6 * i, 6 * (i + 1)):
            ax_box.lines[j].set_color("black")
    plt.subplots_adjust(
        left=0.18, bottom=0.12, right=0.967, top=0.949, wspace=0.2, hspace=0.14
        )
    axs.set_xlabel("Scaling factor (-)")
    axs.set_ylabel('Normalised estimated change in\nannual household fuel cost, $\Delta\hat{C}$ (%)')
    axs.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    num_columns = df_ref.shape[1] - 1
    column_indices = list(range(num_columns))
    axs.set_xticks(column_indices)
    axs.set_xticklabels(labels = df_ref.columns[1:])
    save_figures(f'./demonstrate change in cost ratio valid @ratio{ratio}', False)

def geo_plot_mutilple_map_comparision_change_of_cost_ratio():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):

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
            bottom = q1 - 6 * cb_scale * iqr
            top = q3 + 0.8 * cb_scale * iqr
            divnorm = cl.Normalize(vmin=-5, vmax=15)

            return divnorm

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
        mosaic = '''
                ABCD
                EFGH
                '''
        color_theme = custom_cmap
        fig = plt.figure(figsize=(9,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = 'ABCDEFGH'
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            axs[plot_names[it]].set_title('Price Ratio: ' + df.columns[it + 1], loc='left')
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.025,right=0.975, top=0.95)
            
            # for the last plot adding a colorbar
            if plot_names[it] == mosaic[-1]:
                tl = df_geo.plot(column=df_geo.iloc[:, -1],
                        cmap=color_theme,
                        antialiased=False,
                        ax=axs[plot_names[it]],
                        legend=True,
                        norm=divnorm,
                        cax=cax)
            else:
                tl  = df_geo.plot(df_geo.iloc[:, it + 2],\
                    cmap=color_theme,\
                    antialiased=False,\
                    ax = axs[plot_names[it]],\
                    norm = divnorm)

            
            # Create a colorbar for the plot
            cax = fig.add_axes([0.125, 0.1, 0.75, 0.02])
            cbar4 = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1] ,'ho')
            ticks = [-5, 0, 5, 10, 15]
            ticks_label = [str(tick) for tick in ticks[1:-1]]
            ticks_label.insert(0,f'< {ticks[0]}')
            ticks_label.append(f'> {ticks[-1]}')
            cbar4.set_ticks(ticks)  
            cbar4.set_ticklabels(ticks_label)
            axs[plot_names[it]].set_xticks([])
            axs[plot_names[it]].set_yticks([])
            axs[plot_names[it]].spines["top"].set_visible(False)
            axs[plot_names[it]].spines["right"].set_visible(False)
            axs[plot_names[it]].spines["left"].set_visible(False)
            axs[plot_names[it]].spines["bottom"].set_visible(False)
        
        save_figures(f'./{title}', pdf)
    
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        # index = calculate_index_original_tom(fp, delta_cost)
        # index = calculate_compared_index(fp, delta_cost)
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)
        cost_ratio = delta_cost / cost 
        # index = calculate_standarizationed_index(delta_cost)
        df_ref[f'{ratio}'] = cost_ratio * 100

    df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    
    plot_multiple_geodistribution_a('Normalised estimated change in annual household fuel cost, $\Delta\hat{C}$ (%)',
                                  'change in Fuel Cost Ratio geo plot with cities',
                                  df_ref,
                                  1.3,
                                  False)

def geo_plot_mutilple_map_comparision_inequality_index():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):

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
            bottom = q1 - 3 *  iqr
            top = q3 + 1*iqr
            divnorm = cl.Normalize(vmin=-4, vmax=4)

            return divnorm

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
        mosaic = '''
                ABCD
                EFGH
                '''
        color_theme = custom_cmap
        fig = plt.figure(figsize=(9,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = 'ABCDEFGH'
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            axs[plot_names[it]].set_title('Price Ratio: ' + df.columns[it + 1], loc='left')
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.025,right=0.975, top=0.95)
            
            # for the last plot adding a colorbar
            if plot_names[it] == mosaic[-1]:
                tl = df_geo.plot(column=df_geo.iloc[:, -1],
                        cmap=color_theme,
                        antialiased=False,
                        ax=axs[plot_names[it]],
                        legend=True,
                        norm=divnorm,
                        cax=cax)
            else:
                tl  = df_geo.plot(df_geo.iloc[:, it + 2],\
                    cmap=color_theme,\
                    antialiased=False,\
                    ax = axs[plot_names[it]],\
                    norm = divnorm)

            
            # Create a colorbar for the plot
            cax = fig.add_axes([0.125, 0.075, 0.75, 0.02])
            cbar4 = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1], 'ho')
            ticks = [-4,-2,0,2,4]
            ticks_label = [str(tick) for tick in ticks[1:-1]]
            ticks_label.insert(0,f'< {ticks[0]}')
            ticks_label.append(f'> {ticks[-1]}')
            cbar4.set_ticks(ticks)  
            cbar4.set_ticklabels(ticks_label)
            axs[plot_names[it]].set_xticks([])
            axs[plot_names[it]].set_yticks([])
            axs[plot_names[it]].spines["top"].set_visible(False)
            axs[plot_names[it]].spines["right"].set_visible(False)
            axs[plot_names[it]].spines["left"].set_visible(False)
            axs[plot_names[it]].spines["bottom"].set_visible(False)
        
        save_figures(f'./{title}', pdf)
    
    ratio_list = [3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5]
    for ratio in ratio_list:
        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        index = calculate_index_normalization_variation(fp,delta_cost, 'std')
        df_ref[f'{ratio}'] = index

    df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    
    plot_multiple_geodistribution_a('Inequality Index (-)',
                                  'inequality index geoplots',
                                  df_ref,
                                  0,
                                  False)

def geo_single_country_for_jiying():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
        
        def normalization_a(a,b):
            
            divnorm = cl.Normalize(vmin=0.4, vmax=1)

            return divnorm

        df_geo = call_pickle('./Data/pickles/geometry')
        df = copy.deepcopy(df_in)
        print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)

        # Count how many figures need to be plotted based on df
        num_plot = df.shape[1] - 1

        # Initialize the plot
        mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
        color_theme = custom_cmap
        fig = plt.figure(figsize=(15,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = mosaic 
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            if it % 2 == 0:
                axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10, y =-0.2)
            axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10)
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.075,right=0.8)
            
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
                colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
                colorbar.ax.set_position(colorbar.ax.get_position().translated(-0.05, 0))
                # ticks = [ 0.3, 0.5, 0.7, 0.9, 1]
                ticks = [ 0.4, 0.6, 0.8,  1]
                ticks_label =  [str(tick) for tick in ticks]
                ticks_label = [str(tick) for tick in ticks[1:]]
                ticks_label.insert(0,f'< {ticks[0]}')
                # ticks_label.append(f'> {ticks[-1]}')
                colorbar.set_ticks(ticks)  
                colorbar.set_ticklabels(ticks_label)
                # cax.ticklabel_format(axis="y", style="plain", scilimits=(0,0))  
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
        
        save_figures(f'./{title}', pdf)
    
    # df_1 = pd.read_csv('./GWR_GasMedianConsumption_vs_Percentage of dwellings with an EPC rating of C or higher (%).csv')
    # df_2 = pd.read_csv('./GWR_GasMedianConsumption_vs_Average Energy Efficiency Score.csv')
    # df_3 = pd.read_csv('./GWR_ElecMedianConsumption_vs_Percentage of dwellings with an EPC rating of C or higher (%).csv')
    # df_4 = pd.read_csv('./GWR_ElecMedianConsumption_vs_Average Energy Efficiency Score.csv')
    
    
    # df_ref = df_1[['LSOA_code']]
    # df_ref['GasMedianConsumption\nvs_Percentage of dwellings\nwith an EPC rating of C or higher (%)'] = df_1['normalized_gwr_coeff']
    # df_ref['GasMedianConsumption\nvs_Average Energy\nEfficiency Score'] = df_2['normalized_gwr_coeff']
    # df_ref['ElecMedianConsumption\nvs_Percentage of\ndwellings with an\nEPC rating of C\nor higher (%)'] = df_3['normalized_gwr_coeff']
    # df_ref['ElecMedianConsumption\nvs_Average\nEnergy Efficiency Score'] = df_4['normalized_gwr_coeff']
    
    
    df = pd.read_excel(f'./Calculation result_Plot_with_Building Retrofit Priority Index.xlsx')
    df['LSOA_code'] = df['LSOA Code']
    df = df[['LSOA_code', 'Building Retrofit Priority Index', 'Normalized Building Retrofit Priority Index ']]
    df['LSOA_code'] = df['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))

    # plot_multiple_geodistribution_a('Change in domestic energy cost \n (£/year/household)',
    #                               'Change in domestic energy cost compare 2019 vs 2022',
    #                               df_ref,
    #                               0,
    #                               False)
    # plot_multiple_geodistribution_a('Change in domestic energy cost \n (£/year/household)',
    #                               'Change in domestic energy cost compare 2019 vs 2022',
    #                               df_ref,
    #                               0,
    #                               False)
    plot_multiple_geodistribution_a('Index level',
                                  'Building Retrofit Priority Index Normalized Building Retrofit Priority Index ',
                                  df,
                                  0,
                                  False)
    # ('Inequality Index',
    #                               'STANDARD Inequality Index versus ratio geo plot',
    #                               df_ref,
    #                               0)

def generate_csv_for_jiying():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
        
        def normalization_a(val_values, cb_scale):
            iqr = st.iqr(val_values)
            q1,q3 = st.mstats.idealfourths(val_values)
            bottom = q1 - 1 * iqr
            top = q3 + 5 * iqr
            divnorm = cl.Normalize(vmin=0.14, vmax=1)

            return divnorm

        df_geo = call_pickle('./Data/pickles/geometry')
        df = copy.deepcopy(df_in)
        print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)
        # Sort the DataFrame by 'column_name' in ascending order
        df_geo = df_geo.sort_values(by='Proportion of households fuel poor (%)')
        # Reset the index if needed
        df_geo = df_geo.reset_index(drop=True)

        # Count how many figures need to be plotted based on df
        num_plot = df.shape[1] - 1

        # Initialize the plot
        mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
        color_theme = custom_cmap
        fig = plt.figure(figsize=(9,5))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = mosaic 
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10)
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.075,right=0.8)
            
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
                colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
                colorbar.ax.set_position(colorbar.ax.get_position().translated(-0.05, 0))
                ticks = [0.14, 0.2, 0.6, 0.8, 1]
                ticks_label = [str(tick) for tick in ticks[1:-1]]
                ticks_label.insert(0,f'< {ticks[0]}')
                ticks_label.append(f'> {ticks[-1]}')
                colorbar.set_ticks(ticks)  
                colorbar.set_ticklabels(ticks_label)
                # cax.ticklabel_format(axis="y", style="plain", scilimits=(0,0))  
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
        
        save_figures(f'./{title}', pdf)
    global df_fp
    
    df = pd.read_excel(f'./Updated_Plot.xlsx')
    df['LSOA_code'] = df['LSOA Code']
    df = df[['LSOA_code', 'ECO Support Strength']]
    df['LSOA_code'] = df['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    df['ECO Support Strength'] = df['ECO Support Strength'].replace(0, np.nan)

    df_fp['LSOA_code'] = df_fp['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    # plot_multiple_geodistribution_a('Change in domestic energy cost \n (£/year/household)',
    #                               'Change in domestic energy cost compare 2019 vs 2022',
    #                               df_ref,
    #                               0,
    #                               False)
    plot_multiple_geodistribution_a('ECO Support Strength (-)',
                                  'ECO Support Strength',
                                  df_fp,
                                  0,
                                  False)
    # ('Inequality Index',
    #                               'STANDARD Inequality Index versus ratio geo plot',
    #                               df_ref,
    #                               0)

def geo_plot_compare_2019_2022_index():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
        
        def normalization_a(val_values, cb_scale):
            iqr = st.iqr(val_values)
            q1,q3 = st.mstats.idealfourths(val_values)
            bottom = q1 - 1 * iqr
            top = q3 + 5 * iqr
            divnorm = cl.Normalize(vmin=-4, vmax=4)

            return divnorm

        df_geo = call_pickle('./Data/pickles/geometry')
        df = copy.deepcopy(df_in)
        print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)

        # Count how many figures need to be plotted based on df
        num_plot = df.shape[1] - 1

        # Initialize the plot
        mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
        color_theme = custom_cmap
        fig = plt.figure(figsize=(9,5))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = mosaic 
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10)
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.075,right=0.8)
            
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
                colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
                colorbar.ax.set_position(colorbar.ax.get_position().translated(-0.05, 0))
                ticks = [-4, -2, 0, 2, 4]
                ticks_label = [str(tick) for tick in ticks[1:-1]]
                ticks_label.insert(0,f'< {ticks[0]}')
                ticks_label.append(f'> {ticks[-1]}')
                colorbar.set_ticks(ticks)  
                colorbar.set_ticklabels(ticks_label)
                # cax.ticklabel_format(axis="y", style="plain", scilimits=(0,0))  
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
        
        save_figures(f'./{title}', pdf)
    
    delta_cost_2019 = elec_price_2019 * delta_elect_array - gas_price_2019 * delta_gas_array
    delta_cost_2022 = elec_price_2022 * delta_elect_array - gas_price_2022 * delta_gas_array
    delta_cost_2019 = np.sum(delta_cost_2019, axis=1)
    delta_cost_2022 = np.sum(delta_cost_2022, axis=1)
    index_2019 = calculate_index_normalization_variation(fp, delta_cost_2019,'std')
    index_2022 = calculate_index_normalization_variation(fp, delta_cost_2022,'std')
    df_ref[f'2019 prices'] = index_2019
    df_ref[f'2022 prices'] = index_2022

    df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    plot_multiple_geodistribution_a('Inequality index (-)',
                                  'Inequality index compare 2019 vs 2022',
                                  df_ref,
                                  0,
                                  False)

def geo_plot_compare_2019_2022_fp():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
        
        def normalization_a(val_values, cb_scale):
            iqr = st.iqr(val_values)
            q1,q3 = st.mstats.idealfourths(val_values)
            bottom = q1 - 1 * iqr
            top = q3 + 5 * iqr
            divnorm = cl.Normalize(vmin=-1, vmax=0)

            return divnorm

        df_geo = call_pickle('./Data/pickles/geometry')
        df = copy.deepcopy(df_in)
        print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)

        # Count how many figures need to be plotted based on df
        num_plot = df.shape[1] - 1

        # Initialize the plot
        mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
        cmap = mcolors.LinearSegmentedColormap.from_list('grey_white', ['grey', 'white'])
        color_theme = cmap
        fig = plt.figure(figsize=(9,5))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = mosaic 
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10)
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.075,right=0.8)
            
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
                colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
                colorbar.ax.set_position(colorbar.ax.get_position().translated(-0.05, 0))
                ticks = [-4, -2, 0, 2, 4]
                ticks_label = [str(tick) for tick in ticks[1:-1]]
                ticks_label.insert(0,f'< {ticks[0]}')
                ticks_label.append(f'> {ticks[-1]}')
                colorbar.set_ticks(ticks)  
                colorbar.set_ticklabels(ticks_label)
                # cax.ticklabel_format(axis="y", style="plain", scilimits=(0,0))  
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
        
        save_figures(f'./{title}', pdf)
    global df_fp
    
    # Sort the DataFrame by 'column_name' in ascending order
    df_fp = df_fp.sort_values(by='Proportion of households fuel poor (%)')
    # Reset the index if needed
    df_fp = df_fp.reset_index(drop=True)
    # Add a new column 'New_Column_1' and initialize it with None
    df_fp['2019 prices'] = 0

    # Assign 1 to the first 100 rows in 'New_Column_1'
    df_fp.loc[:9999, '2019 prices'] = -1

    # Add another new column 'New_Column_2' and initialize it with None
    df_fp['2022 prices'] = 0

    # Assign 0 to the last 100 rows in 'New_Column_2'
    df_fp.loc[len(df_fp) - 10000:, '2022 prices'] = -1

    df_fp = drop_column(df_fp, 'Proportion of households fuel poor (%)')

    df_fp['LSOA_code'] = df_fp['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    plot_multiple_geodistribution_a('Inequality index (-)',
                                  'Inequality index compare 2019 vs 2022',
                                  df_fp,
                                  0,
                                  False)

def geo_plot_compare_2019_2022_change_of_cost():

    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
        
        def normalization_a(val_values, cb_scale):
            iqr = st.iqr(val_values)
            q1,q3 = st.mstats.idealfourths(val_values)
            bottom = q1 - 1 * iqr
            top = q3 + 7 * iqr
            divnorm = cl.Normalize(vmin=-25, vmax=100)

            return divnorm

        df_geo = call_pickle('./Data/pickles/geometry')
        df = copy.deepcopy(df_in)
        print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)

        # Count how many figures need to be plotted based on df
        num_plot = df.shape[1] - 1

        # Initialize the plot
        mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
        color_theme = custom_cmap
        fig = plt.figure(figsize=(9,5))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = mosaic 
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10, fontsize = 20)
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            # plt.subplots_adjust(left=0.075,right=0.836)
            plt.subplots_adjust(left=0.025,right=0.8)
            
            # for the last plot adding a colorbar
            if plot_names[it] == mosaic[-1]:
                cax = fig.add_axes([0.88, 0.1, 0.02, 0.8])
                tl = df_geo.plot(column=df_geo.iloc[:, -1],
                        cmap=color_theme,
                        antialiased=False,
                        ax=axs[plot_names[it]],
                        legend=True,
                        norm=divnorm,
                        cax=cax)
                
                colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
                colorbar.ax.set_position(colorbar.ax.get_position().translated(-0.05, 0))
                ticks = [-25, 0, 25, 50, 75, 100]
                ticks_label = [str(tick) for tick in ticks[1:-1]]
                ticks_label.insert(0,f'< {ticks[0]}')
                ticks_label.append(f'> {ticks[-1]}')
                colorbar.set_ticks(ticks)  
                colorbar.set_ticklabels(ticks_label, fontsize = 15)
                # cax.ticklabel_format(axis="y", style="plain", scilimits=(0,0))  
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
            # plt.subplots_adjust(right=0.3)
        
        save_figures(f'./{title}', pdf)
    
    delta_cost_2019 = elec_price_2019 * delta_elect_array - gas_price_2019 * delta_gas_array
    delta_cost_2022 = elec_price_2022 * delta_elect_array - gas_price_2022 * delta_gas_array
    cost = calculate_current_cost(ratio_2019, elec_consump, gas_consump)
    delta_cost_2019 = np.sum(delta_cost_2019, axis=1)
    delta_cost_2022 = np.sum(delta_cost_2022, axis=1)
    df_ref[f'2019'] = delta_cost_2019
    df_ref[f'2022'] = delta_cost_2022

    df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
    plot_multiple_geodistribution_a('Change in annual domestic\nfuel cost (£/year/household)',
                                  'Change in domestic energy cost compare 2019 vs 2022',
                                  df_ref,
                                  0,
                                  False)
    # ('Inequality Index',
    #                               'STANDARD Inequality Index versus ratio geo plot',
    #                               df_ref,
    #                               0)

def geo_plot_with_cities():

    def plot_geodistribution_with_cities_a(label: str, title:str, df_in: pd.DataFrame, cb_scale: float = 0, pdf = True):
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
            # colors_rgb = ([(4/255, 127/255, 60/255), 
            #             (43/255, 162/255, 46/255), 
            #             (148/255, 205/255, 62/255), 
            #             (244/255, 244/255, 26/255),
            #             (236/255, 174/255, 41/255), 
            #             (229/255, 100/255, 45/255),
            #             (218/255, 1/255, 44/255)])
            # colors_rgb.reverse()
            # colors_normalized = [(r, g, b) for r, g, b in colors_rgb]
            # positions = np.linspace(0, 1, len(colors_normalized))
            # cmap_dict = {'red': [], 'green': [], 'blue': []}

            # # Populate the dictionary with the normalized RGB values and positions
            # for pos, color in zip(positions, colors_normalized):
            #     r, g, b = color
            #     cmap_dict['red'].append((pos, r, r))
            #     cmap_dict['green'].append((pos, g, g))
            #     cmap_dict['blue'].append((pos, b, b))
            
            # cmap_name = 'custom_cmap'
            # custom_cmap = LinearSegmentedColormap(cmap_name, cmap_dict)

            color_theme = custom_cmap
            mosaic = '''
            A
            A
            '''
            fig = plt.figure(figsize=(15,7))
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

            return axs, cax, color_theme, UK_gdf, fig

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
            divnorm = cl.Normalize(vmin=-2300, vmax=-700)

            return divnorm

        # Get Geospatial shapes:
        df_geo = call_pickle('./Data/pickles/geometry')

        # Make a copy of df_in
        df = copy.deepcopy(df_in)
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, title, df)
        
        # Initilize the graph
        axs, cax, color_theme, UK_gdf, fig = basic_settings(df_geo)
        
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)

        axs_xbounds = [np.array([-2.815E5,-2E5]),np.array([-2.838E5,-1.05E5]),np.array([-3.35E4,9.4E3]),np.array([-6.5E5,-1.957E5])]
        axs_ybounds = [np.array([7.007E6,7.0652E6]),np.array([7.206E6,7.41E6]),np.array([6.656E6,6.6969E6]),np.array([6.39E6,6.78E6])]
        
        
        # Create a colorbar for the plot
        # colorbar.set_label(label, fontsize=18)
        # ticks = [50, 60, 70, 80]
        # ticks_label = [str(tick) for tick in ticks[1:-1]]
        # ticks_label.insert(0,f'< {ticks[0]}')
        # ticks_label.append(f'> {ticks[-1]}')
        # colorbar.set_ticks(ticks)  

        
        colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo[f"{title}"])
        
        # bounds = [1, 21, 39, 55, 69, 81, 92, 100]
        # ticks = [ 0, 20, 40, 60, 80, 100]
        # ticks = [-1,-0.75, -0.5,-0.25, 0, 0.25,0.5, 0.75, 1]
        # ticks = [0.14, 0.4, 0.6, 0.8, 1]
        ticks = [-2300, -1900, -1500, -1100,-700]
        ticks_label = [str(tick) for tick in ticks[1:-1]]
        ticks_label.insert(0,f'<{ticks[0]}')
        ticks_label.append(f'>{ticks[-1]}')
        colorbar.set_ticks(ticks) 
        colorbar.set_ticklabels(ticks_label, fontsize=16)
        # norm = mpl.colors.BoundaryNorm(ticks, color_theme.N)
        # colorbar = fig.colorbar(
        #     mpl.cm.ScalarMappable(cmap=cmap, norm=norm),
        #     cax=cax,
        #     ticks=bounds,
        #     spacing='proportional',
        #     orientation='vertical',
        #     label='Energy Efficiency (%)',
        # )

        
        tl = df_geo.plot(column=f"{title}",
                    cmap=color_theme,
                    antialiased=False,
                    ax=axs['A'],
                    norm=divnorm,
                    cax=cax)
        
        # ticks = [1, 21, 39, 55, 69, 81, 92, 100]
        # ticks_label = [str(tick) for tick in ticks]
        # colorbar.set_ticks(ticks)  
        # colorbar.set_ticklabels(ticks_label, fontsize=16)

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
            axins2.set_title(str(names[f]), fontsize=18)
            axins2.set_yticks([])
            axins2.set_ylim(axs_ybounds[f])
            axins2.set_xlim(axs_xbounds[f])
            df_geo.plot(column=f"{title}",cmap=color_theme,\
                    antialiased=False,\
                    ax = axins2,\
                    norm = divnorm)
            mark_inset(axs['A'],axins2,loc1=loc1[f],loc2=loc2[f],fc='none',ec='0')
        
        # Store the figures
        plt.subplots_adjust(left=0.15, bottom=0.15, top=0.9, right=0.9)
        save_figures(f'./{title}', pdf)
    
    def plot_multiple_geodistribution_a(label: str, title:str, df_in: pd.DataFrame,cb_scale: float = 0, pdf = False):
        
        def normalization_a(a,b):
            
            # divnorm = cl.Normalize(vmin=50, vmax=250)
            divnorm = cl.Normalize(vmin=0.25, vmax=0.75)

            return divnorm

        df_geo = call_pickle('./Data/pickles/geometry')
        df = copy.deepcopy(df_in)
        print(f'Beginning plot for geodistribution (multiple in comparison) of {title}...')
        
        # Revising the data
        df_geo, val_values =data_treatment(df_geo, arg_name=False, arg_value_in=df)

        # Count how many figures need to be plotted based on df
        num_plot = df.shape[1] - 1

        # Initialize the plot
        mosaic = 'ABCDEFGHIJKLMNOPQRST'[:num_plot]
        color_theme = custom_cmap
        fig = plt.figure(figsize=(15,7))
        axs = fig.subplot_mosaic(mosaic)    
        UK_gdf = gpd.read_file("GB_shapefile/GBR_adm2.shp")
        UK_gdf = UK_gdf.to_crs("EPSG:3395")
        plot_names = mosaic 
        
        # Specify the interquartile range (IQR) and the first and third quartiles (q1 and q3)
        divnorm = normalization_a(val_values, cb_scale)
        
        for it in range(num_plot):  # iterate over the number of subplots
            if it % 2 == 0:
                axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10, y =-0.2)
            axs[plot_names[it]].set_title(df.columns[it + 1], loc='center', pad=10)
            UK_gdf.boundary.plot(ax=axs[plot_names[it]],color='k',linewidth=0.5)
            boundary = df_geo.bounds
            boundary = [min(boundary.values[:,0]),min(boundary.values[:,1]),max(boundary.values[:,2]),max(boundary.values[:,3])]
            axs[plot_names[it]].set_ylim([boundary[1]-5E4,boundary[3]+5E4])
            axs[plot_names[it]].set_xlim(([boundary[0]-5E4,boundary[2]]))
            plt.subplots_adjust(left=0.075,right=0.8)
            
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
                colorbar = create_color_bar(color_theme, divnorm, label, axs, cax, df_geo.iloc[:, -1])
                colorbar.ax.set_position(colorbar.ax.get_position().translated(-0.05, 0))
                # ticks = [0, 1, 2, 3, 4]
                # ticks = [0, 100, 200, 300, 400]
                ticks = [0.4, 0.5, 0.6, 0.7]
                # ticks = [0.25, 0.5, 0.75]
                # ticks = [50, 70, 90, 120, 150, 250]
                # ticks_label =  [str(tick) for tick in ticks]
                ticks_label = [str(tick) for tick in ticks[1:]]
                ticks_label.insert(0,f'< {ticks[0]}')
                # ticks_label.append(f'> {ticks[-1]}')
                colorbar.set_ticks(ticks)  
                colorbar.set_ticklabels(ticks_label)
                # cax.ticklabel_format(axis="y", style="plain", scilimits=(0,0))  
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
        
        save_figures(f'./{title}', pdf)
    
    def plot_inequality_index():
        ratio_list = [ratio_2019]
        for ratio in ratio_list:
            delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
            index = calculate_index_normalization_variation(fp, delta_cost,'std')
            # index = calculate_index_original_tom(fp, delta_cost)
            df_ref[f'{ratio}'] = index

        df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        plot_geodistribution_with_cities_a('Inequality index (-)',
                                  'Inequality index geo plot 2019',
                                  df_ref,
                                  1.5,
                                False)
    
    def energy_efficiency_geo_plot():
        data_dict = {}
        csv_file = './Data/MSOA_LSOA.csv'

        # Read the CSV file
        with open(csv_file, 'r') as file:
            csv_reader = csv.reader(file, delimiter=',')
            
            # Iterate through each row in the CSV
            for row in csv_reader:
                msoa_code, lsoa_code = row
                # Check if the MSOA code is already a key in the dictionary
                if msoa_code in data_dict:
                    # If it is, append the LSOA code to the existing list
                    data_dict[msoa_code].append(lsoa_code)
                else:
                    # If it's not, create a new list with the LSOA code
                    data_dict[msoa_code] = [lsoa_code]

        del data_dict['ï»¿msoa']
        energy_file = './Data/MSOA-Regional Building Efficiency.csv'

        # Read the energy efficiency data into a DataFrame
        energy_df = pd.read_csv(energy_file)

        # Initialize an empty list to store LSOA code - energy efficiency pairs
        lsoa_energy_data = []

        # Iterate through the MSOA codes in the data_dict
        for msoa_code, lsoa_codes in data_dict.items():
            # Get the corresponding energy efficiency for the MSOA code
            try:
                energy_efficiency = energy_df.loc[energy_df['MSOA_code'] == msoa_code, 'Energy efficiency'].values[0]
            except:
                print(f"No data for MSOA:{msoa_code}")
            # Iterate through the LSOA codes associated with the MSOA code
            for lsoa_code in lsoa_codes:
                lsoa_energy_data.append({'LSOA_code': lsoa_code, 'Energy efficiency': energy_efficiency})

        # Create a DataFrame from the list of LSOA code - energy efficiency pairs
        df_ref = pd.DataFrame(lsoa_energy_data)
        df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_ref, df_fp = sort_muiltiple_df(df_ref, df_fp)
        df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        plot_geodistribution_with_cities_a('Energy Efficiency (%)',
                                    'Energy Efficiency',
                                    df_ref,
                                    1.5,
                                    False)
    
    def npc():
        # df = pd.read_csv('./TotalEnergyConsumption_MoransI_Results.csv')
        df = pd.read_excel(f'./LSOA-LA-BRPM.xlsx')
        df['LSOA_code'] = df['LSOA Code']
        df = df[['LSOA_code', 'Building Retrofit Prioritization Metric (BRPM)']]
        df['LSOA_code'] = df['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        df['Building Retrofit Prioritization Metric (BRPM)'] = df['Building Retrofit Prioritization Metric (BRPM)'].replace(0, np.nan)
        # plot_geodistribution_with_cities_a('Building Retrofit Prioritization Metric (BRPM) (-)',
        #                             'Building Retrofit Prioritization Metric (BRPM)',
        #                             df,
        #                             1.5,
        #                             False)
        plot_multiple_geodistribution_a('Building Retrofit Prioritization Metric (BRPM)',
                                  'Building Retrofit Prioritization Metric (BRPM)',
                                  df,
                                  0,
                                  False)
    
    def la_code():
        data_dict = {}
        csv_file = './Data/LA_LSOA.csv'

        # Read the CSV file
        with open(csv_file, 'r') as file:
            csv_reader = csv.reader(file, delimiter=',')
            
            # Iterate through each row in the CSV
            for row in csv_reader:
                la_code, lsoa_code = row
                # Check if the MSOA code is already a key in the dictionary
                if la_code in data_dict:
                    # If it is, append the LSOA code to the existing list
                    data_dict[la_code].append(lsoa_code)
                else:
                    # If it's not, create a new list with the LSOA code
                    data_dict[la_code] = [lsoa_code]
        del data_dict['ï»¿la']

        energy_file = './LA-ECO_DENSITY.xlsx'

        # Read the energy efficiency data into a DataFrame
        energy_df = pd.read_excel(energy_file)

        # Initialize an empty list to store LSOA code - energy efficiency pairs
        lsoa_energy_data = []

        # Iterate through the MSOA codes in the data_dict
        for la_code, lsoa_codes in data_dict.items():
            # Get the corresponding energy efficiency for the MSOA code
            try:
                energy_efficiency = energy_df.loc[energy_df['Area Codes'] == la_code, 'ECO Measure Adoption per 1,000 Households'].values[0]
            except:
                print(f"No data for LA:{la_code}")
            # Iterate through the LSOA codes associated with the MSOA code
            for lsoa_code in lsoa_codes:
                lsoa_energy_data.append({'LSOA_code': lsoa_code, 'ECO Measure Adoption per 1,000 Households': energy_efficiency})

        # Create a DataFrame from the list of LSOA code - energy efficiency pairs
        df_ref = pd.DataFrame(lsoa_energy_data)
        df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        df_ref, df_fp = sort_muiltiple_df(df_ref, df_fp)
        df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        
        plot_multiple_geodistribution_a('ECO Measure Adoption per 1,000 Households',
                                  'ECO Measure Adoption per 1,000 Households',
                                  df_ref,
                                  0,
                                  False)
    
    def lsoa_to_la():
        data_dict = {}
        csv_file = './Data/LA_LSOA.csv'

        # Read the CSV file
        with open(csv_file, 'r') as file:
            csv_reader = csv.reader(file, delimiter=',')
            
            # Iterate through each row in the CSV
            for row in csv_reader:
                la_code, lsoa_code = row
                # Check if the MSOA code is already a key in the dictionary
                if la_code in data_dict:
                    # If it is, append the LSOA code to the existing list
                    data_dict[la_code].append(lsoa_code)
                else:
                    # If it's not, create a new list with the LSOA code
                    data_dict[la_code] = [lsoa_code]
        del data_dict['ï»¿la']

        energy_file = './LSOA-LA-BRPM.xlsx'

        # Read the energy efficiency data into a DataFrame
        energy_df = pd.read_excel(energy_file)

        lsoa_to_la = {}
        for la_code, lsoa_list in data_dict.items():
            for lsoa_code in lsoa_list:
                lsoa_to_la[lsoa_code] = la_code

        # Map lsoa_code to la_code in energy_df
        energy_df['la_code'] = energy_df['LSOA Code'].map(lsoa_to_la)

        # Group energy_df by la_code and calculate average BRPM
        avg_brpm_by_la = energy_df.groupby('la_code')['Building Retrofit Prioritization Metric (BRPM)'].mean()
        avg_brpm_by_la = avg_brpm_by_la.reset_index()
        avg_brpm_by_la.columns = ['la_code', 'Averaged BRPM']

        # Merge avg_brpm_by_la back to energy_df
        energy_df = pd.merge(energy_df, avg_brpm_by_la, on='la_code', how='left')
        energy_df['LSOA_code'] = energy_df['LSOA Code']
        energy_df = energy_df[['LSOA_code', 'Averaged BRPM']]
        # df_fp = pd.read_csv(f"./Data/properties_csv/{year}/fuel_poverty.csv")
        # df_ref, df_fp = sort_muiltiple_df(df_ref, df_fp)
        energy_df['LSOA_code'] = energy_df['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        
        plot_multiple_geodistribution_a('Averaged BRPM',
                                  'Averaged BRPM',
                                  energy_df,
                                  0,
                                  False)
    
    def plot_emission_2019():
        ratio_list = [ratio_2019]
        for ratio in ratio_list:
            carbon_intensity_elec = 0.193
            # 0.223 0.193
            carbon_intensity_gas = 0.2023
            # 0.184 0.2023
            delta_elect = np.sum(delta_elect_array, axis=1)
            delta_gas = np.sum(delta_gas_array, axis=1)
            emission = delta_elect*carbon_intensity_elec - delta_gas*carbon_intensity_gas
            df_ref[f'{ratio}'] = emission

        df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        plot_geodistribution_with_cities_a('Change in emissions (kg$CO_2$eq/year/household)',
                                  'Change in emission 2022',
                                  df_ref,
                                  1.5,
                                False)
    
    def plot_emission_2022():
        ratio_list = [ratio_2019]
        for ratio in ratio_list:
            carbon_intensity_elec = 0.223
            # 0.223 0.193
            carbon_intensity_gas = 0.184
            # 0.184 0.2023
            delta_elect = np.sum(delta_elect_array, axis=1)
            delta_gas = np.sum(delta_gas_array, axis=1)
            emission = delta_elect*carbon_intensity_elec - delta_gas*carbon_intensity_gas
            df_ref[f'{ratio}'] = emission

        df_ref['LSOA_code'] = df_ref['LSOA_code'].apply(lambda x: add_prefix(x, prefix = ONS_ID))
        plot_geodistribution_with_cities_a('Change in emissions (kg$CO_2$eq/year/household)',
                                  'Change in emission 2022',
                                  df_ref,
                                  1.5,
                                False)
    
    # la_code()
    # lsoa_to_la()
    # plot_emission_2019()
    plot_emission_2022()

def ridgeline_plot_change_of_cost_ratio():
    
    for ratio in ratio_list:

        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)

        cost_ratio = delta_cost / cost 

        # index = calculate_index_normalization_variation(fp, delta_cost, 'quartile')

        df_ref[f'{ratio}'] = cost_ratio * 100

    # Transpose the dataframe to have columns as rows
    transposed_df = df_ref.iloc[:, 1:].transpose()

    # Reset the index to make the column names accessible
    transposed_df = transposed_df.reset_index()

    # Rename the columns to use as labels
    transposed_df = transposed_df.rename(columns={"index": "label"})

    # Melt the dataframe to convert columns into a single column
    melted_df = pd.melt(transposed_df, id_vars=["label"], value_name="value")
    median_serie = melted_df.groupby('label')['value'].median()
    melted_df['median'] = melted_df['label'].map(median_serie)
    
    colors_normalized[3] = (253/255, 200/255, 0)
    colors_normalized[2] = (135/255,206/255, 235/255)
    # Generate a color palette
    pal = sns.color_palette(palette=colors_normalized, n_colors=len(df_ref.columns) - 1)

    # Create a FacetGrid with hue and row arguments
    g = sns.FacetGrid(melted_df, row='label', hue = 'median', aspect=6, height=2, palette=pal)

    g.fig.set_size_inches(9, 6)

    # Add the densities kdeplots for each ratio
    g.map(sns.kdeplot, 'value', bw_adjust=1, clip_on=False, fill=True, alpha=1, linewidth=1.5)

    # Add a horizontal line for each plot
    g.map(plt.axhline, y=0, lw=2, clip_on=False)

    # Loop over the FacetGrid figure axes and add the label as text with the right color
    i = 1
    for ax in g.axes.flat:
        if i == 1:
            ax.text(-18, 0.18, 'Price Ratio:', fontsize=12, color='black')
        if i == 8:
            ax.annotate(
                ' ',
                xy=(-14.5, 0),
                xytext=(-14.5, 0.35),
                arrowprops=dict(arrowstyle='<-', lw=2, color=g._colors[i-1]),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
            
            ax.text(-14, 0.2, 'Density (-)', fontsize=12, color='black')
            # ax.plot([13, 15], [0.5, 0.5], color=g._colors[i-1], linewidth=2)
            ticks=[-15, -7.5, 0, 7.5, 15]
            for tick in ticks:
                ax.text(tick, -0.15, f'{tick}', fontsize=12, color='black')
        label = f'{df_ref.columns[i]}'
        ax.text(-18, 0.01, label, fontsize=12, color='black')

        # Get the data for the current ridge line
        ridge_data = melted_df[melted_df['label'] == df_ref.columns[i]]

        
        # Find the max and min values and their corresponding positions
        max_val = ridge_data['value'].max()
        min_val = ridge_data['value'].min()
        max_x = ridge_data[ridge_data['value'] == max_val]['value'].values[0]
        min_x = ridge_data[ridge_data['value'] == min_val]['value'].values[0]
        # Add annotations for max and min values
        # Add short sticks for max and min values
        ax.plot([max_x, max_x], [0, 0.05], color=g._colors[i-1], linewidth=3)
        ax.plot([min_x, min_x], [0, 0.05], color=g._colors[i-1], linewidth=3)
        # ax.text(max_x-0.5, 0.035, f'{(max_val):.1f}', fontsize=12, color='black', va='bottom')
        
        # if i ==3:
        #     ax.text(min_x-0.7, 0.035, f'{(min_x):.1f}', fontsize=12, color='black',  va='bottom')
        # else:
        #     ax.text(min_x-0.5, 0.035, f'{(min_x):.1f}', fontsize=12, color='black',  va='bottom')
        kde = gaussian_kde(ridge_data['value'].dropna())
        median_val = melted_df.loc[melted_df['label'] == df_ref.columns[i], 'value'].median()
        density_at_median = kde.evaluate(median_val)
        ax.plot([median_val, median_val], [0, density_at_median], color=g._colors[i-1], linewidth=2, linestyle='dashed')
        i += 1
        ax.set_ylim(0, 0.1)

    # # Adjust the subplots to overlap
    # g.fig.subplots_adjust(hspace=-0.1)

    # Remove axes titles, yticks, and spines
    g.set_titles("")
    g.set(yticks=[])
    g.set(xticks=[])
    g.set(ylabel='')
    g.despine(bottom=True, left=True)

    x_var = 'Normalized \u0394C (%)'
    # change in Cost Ratio
    axes = g.axes.flat
    # axes[7].set_xticks(ticks=[-10, -5, 0, 5, 10, 15])
    # axes[7].tick_params(axis='both', which='both', bottom=False, top=False, left=False, right=False, labelbottom=True, labelleft=True)

    # Set x-axis tick labels properties
    plt.subplots_adjust(top=0.875, left = 0.1)
    # plt.setp(ax.get_xticklabels(), fontsize=12)
    x_axis_label =plt.xlabel(f'{x_var}', fontsize=12, labelpad = 17.5)

    save_figures(f'./Ridge plot Change in Cost @{year}', pdf= False)

def ridgeline_plot_inequality_index():
    
    ratio_list = [3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5]
    for ratio in ratio_list:

        delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
        
        cost = calculate_current_cost(ratio, elec_consump, gas_consump)

        cost_ratio = delta_cost / cost 

        index = calculate_index_normalization_variation(fp, delta_cost, 'std')

        df_ref[f'{ratio}'] = index

    # Transpose the dataframe to have columns as rows
    transposed_df = df_ref.iloc[:, 1:].transpose()

    # Reset the index to make the column names accessible
    transposed_df = transposed_df.reset_index()

    # Rename the columns to use as labels
    transposed_df = transposed_df.rename(columns={"index": "label"})

    # Melt the dataframe to convert columns into a single column
    melted_df = pd.melt(transposed_df, id_vars=["label"], value_name="value")
    median_serie = melted_df.groupby('label')['value'].median()
    melted_df['median'] = melted_df['label'].map(median_serie)
    
    colors_normalized[3] = (253/255, 200/255, 0)
    colors_normalized[2] = (135/255,206/255, 235/255)
    # Generate a color palette
    pal = sns.color_palette(palette=colors_normalized, n_colors=len(df_ref.columns) - 1)

    # Create a FacetGrid with hue and row arguments
    g = sns.FacetGrid(melted_df, row='label', hue = 'median', aspect=6, height=2, palette=pal)

    g.fig.set_size_inches(7, 5)

    # Add the densities kdeplots for each ratio
    g.map(sns.kdeplot, 'value', bw_adjust=1, clip_on=False, fill=True, alpha=1, linewidth=1.5)

    # Add a horizontal line for each plot
    g.map(plt.axhline, y=0, lw=2, clip_on=False)

    # Loop over the FacetGrid figure axes and add the label as text with the right color
    i = 1
    for ax in g.axes.flat:
        if i == 1:
            ax.text(-25, 0.25, 'Price ratio:', fontsize=12, color='black')
        if i == 8:
            ax.annotate(
                ' ',
                xy=(-21, 0),
                xytext=(-21, 0.4),
                arrowprops=dict(arrowstyle='<-', lw=2, color=g._colors[i-1]),
                fontsize=12,
                color='black',
                ha='center',  # Horizontal alignment set to 'center'
                va='center',  # Vertical alignment set to 'center'
            )
            ax.set_xticks([-20, -10, 0, 10, 20])
            ax.text(-20.5, 0.25, 'Density (-)', fontsize=12, color='black')
            ticks=[-20, -10, 0, 10, 20]
            for tick in ticks:
                ax.text(tick, -0.15, f'{tick}', fontsize=12, color='black')
        label = f'{df_ref.columns[i]}'
        ax.text(-25, 0.01, label, fontsize=12, color='black')

        # Get the data for the current ridge line
        ridge_data = melted_df[melted_df['label'] == df_ref.columns[i]]

        
        # Find the max and min values and their corresponding positions
        max_val = ridge_data['value'].max()
        min_val = ridge_data['value'].min()
        max_x = ridge_data[ridge_data['value'] == max_val]['value'].values[0]
        min_x = ridge_data[ridge_data['value'] == min_val]['value'].values[0]
        # Add annotations for max and min values
        # Add short sticks for max and min values
        ax.plot([max_x, max_x], [0, 1], color=g._colors[i-1], linewidth=3)
        ax.plot([min_x, min_x], [0, 1], color=g._colors[i-1], linewidth=3)
        # ax.text(max_x-0.5, 0.035, f'{(max_val):.1f}', fontsize=12, color='black', va='bottom')
        
        # if i ==3:
        #     ax.text(min_x-0.7, 0.035, f'{(min_x):.1f}', fontsize=12, color='black',  va='bottom')
        # else:
        #     ax.text(min_x-0.5, 0.035, f'{(min_x):.1f}', fontsize=12, color='black',  va='bottom')
        kde = gaussian_kde(ridge_data['value'].dropna())
        median_val = melted_df.loc[melted_df['label'] == df_ref.columns[i], 'value'].median()
        density_at_median = kde.evaluate(median_val)
        ax.plot([median_val, median_val], [0, density_at_median], color=g._colors[i-1], linewidth=2, linestyle='dashed')
        i += 1
        ax.set_ylim(0, 0.03)

    # # Adjust the subplots to overlap
    # g.fig.subplots_adjust(hspace=-0.1)

    # Remove axes titles, yticks, and spines
    g.set_titles("")
    g.set(yticks=[])
    g.set(xticks=[])
    g.set(ylabel='')
    g.despine(bottom=True, left=True)
    # Access the axes of the FacetGrid individually
    axes = g.axes.flat
    # axes[5].set_xscale("symlog", base=4, linscale = 0.8, linthresh = 1)
    # # y_ticks = [-16, -4, -1, 0, 1, 4, 16]
    # axes[5].set_xlim(-64,64)
    # # y_ticks = [-1, 0, 1]
    # axes[5].set_xticks(y_ticks)
    # axes[5].set_xticklabels([str(y) if y != 0 else "0" for y in y_ticks])
    g.set(xticks=[])

    g.set(ylabel='')
    g.despine(bottom=True, left=True)

    x_var = 'Inequality index (-)'
    # change in Cost Ratio

    # Set x-axis tick labels properties
    plt.subplots_adjust(top=0.9, left = 0.1)
    # plt.setp(ax.get_xticklabels(), fontsize=12)
    plt.xlabel(f'{x_var}', fontsize=12, labelpad = 14)

    save_figures(f'./Ridge plot Inequality Index @{year}', pdf= False)

def bar_chart_plot():

    def pearson_correlation(x, y):
        # Calculate the mean of each dataset
        mean_x = np.nanmean(x)
        mean_y = np.nanmean(y)

        # Calculate the numerator and denominators for the correlation coefficient
        numerator = np.sum((x - mean_x) * (y - mean_y))
        denominator = np.sqrt(np.sum((x - mean_x) ** 2) * np.sum((y - mean_y) ** 2))

        # Calculate the correlation coefficient
        correlation_coefficient = numerator / denominator

        return correlation_coefficient

    def bar_chart_plot_reverse_fuel_poverty(array):
        
        plt.figure(figsize=(12, 6))  # Set the figure size as per your preference
        
        max = np.nanmax(array)

        array = 1 - array
        # Plot the column chart using Matplotlib's bar() function
        plt.bar(np.arange(len(array)), array, width=0.8, color='blue')

        # Customize the chart
        plt.xlabel('')          # Replace 'Index' with an appropriate label for the x-axis
        plt.ylabel('Fuel Poverty proportion (%)')    # Replace 'Data Values' with an appropriate label for the y-axis
        plt.ylim(0, 1.5)
        save_figures(f'./Column Chart reverse fuel poverty @{year}', pdf= False)

    def bar_chart_plot_ratio():
        
        for ratio in ratio_list:

            delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
            delta_cost = calculate_standarizationed_index(delta_cost)
            # delta_cost = calculate_normalized_index_median_zero(delta_cost)

            cost = calculate_current_cost(ratio, elec_consump, gas_consump)

            cost_ratio = delta_cost / cost 
            # cost_ratio = calculate_normalized_index_median_zero(cost_ratio)
            # cost_ratio = calculate_standarizationed_index(cost_ratio)
            
            array = calculate_normalized_index(delta_cost, np.nanmin(delta_cost), np.nanmax(delta_cost))
            
            plt.figure(figsize=(12, 6))  # Set the figure size as per your preference

            # Plot the column chart using Matplotlib's bar() function
            plt.bar(np.arange(len(array)), array, width=0.8, color='blue')
            plt.ylim(0, 1.5)

            # Customize the chart
            plt.xlabel('')          # Replace 'Index' with an appropriate label for the x-axis
            plt.ylabel('\u0394C (£/year/household)')    # Replace 'Data Values' with an appropriate label for the y-axis
            
            save_figures(f'./change in cost @{year} @ratio {ratio}', pdf= False)
    
    def print_out_results():
        for ratio in ratio_list:

            delta_cost = calculate_delta_cost(ratio, delta_elect_array, delta_gas_array)
            delta_cost = calculate_standarizationed_index(delta_cost)
            # delta_cost = calculate_normalized_index_median_zero(delta_cost)

            # fp_norm = calculate_normalized_index(fp, 0, 20)
            fp_norm = calculate_standarizationed_index(fp)
            # fp_norm = calculate_normalized_index(fp, np.nanmin(fp), np.nanmax(fp))
            
            cost = calculate_current_cost(ratio, elec_consump, gas_consump)

            cost_ratio = delta_cost / cost 
            # cost_ratio = calculate_normalized_index_median_zero(cost_ratio)
            # cost_ratio = calculate_standarizationed_index(cost_ratio)


    fp_norm = calculate_normalized_index(fp, np.nanmin(fp), np.nanmax(fp))
    
    bar_chart_plot_reverse_fuel_poverty(fp_norm)

    bar_chart_plot_ratio()

# Uncomment the function, to generate the figures
# It is recommended to generate figures one by one
# ----- Graphic abstract -----#

# This figure is generated by Powerpoint, go to Dropbox and find the folder CoMo shared/jx309 for the powerpoint. 

# ----- Figure 1 -----#

# -- uncomment the 'elec_and_gas()' inside the following function before start -- #
# line_plot_price_ratio_only_with_prediction()

# ----- Figure 2 -----#

# geo_plot_compare_2019_2022_change_of_cost()
# line_plot_change_of_cost_comparison()

# ----- Figure 3 -----#

# -- uncomment the 'ratio()' inside the following function before start -- #
# line_plot_price_ratio_only_with_prediction()

# ----- Figure 4 -----#

# geo_plot_mutilple_map_comparision_change_of_cost_ratio()
# geo_plot_mutilple_map_comparision_inequality_index()

# ----- Figure 5 -----#

# Figures are jointed in the Powerpoint
# geo_plot_compare_2019_2022_fp()
# line_plot_inequality_20192022_as_xyaxis_red()
# line_plot_inequality_20192022_as_xyaxis_blue()

# ----- Figure 6 -----#

# Figures are jointed in the Powerpoint
# subsidy_plot_2019_bar()
# subsidy_plot_2022_bar()
# subsidy_plot_2019_density()
# subsidy_plot_2022_density()

# ----- Figure A.1 -----#

# -- uncomment the 'plot_emission_2019()' or 'plot_emission_2022()' inside the following function before start -- #
# geo_plot_with_cities()

# ----- Figure A.2 -----#

# -- uncomment the 'elec()' inside the following function before start -- #
# line_plot_change_of_cost()


# ----- Figure A.3 -----#

# -- uncomment the 'ratio()' inside the following function before start -- #
# line_plot_change_of_cost()

# ----- Figure A.4 -----#

# plot_box_and_whisker_validation_for_index(4)
# plot_box_and_whisker_validation_for_index(5)
# plot_box_and_whisker_validation_for_change_of_cost_ratio(4)
# plot_box_and_whisker_validation_for_change_of_cost_ratio(5)

# ----- Figure A.5 -----#

# Figures are jointed in the Lucidchart
# ridgeline_plot_change_of_cost_ratio()
# line_plot_normalized_proportion_of_inequality_region()

# ----- Figure A.6 -----#

# scatter_plot_inequality_20192022_as_xyaxis_fp()
# scatter_plot_inequality_20192022_as_xyaxis_temperature()
# scatter_plot_inequality_20192022_as_xyaxis_change_in_cost()
