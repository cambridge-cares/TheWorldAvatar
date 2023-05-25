###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################




import pandas as pd

from numpy import std, mean
from scipy.special import gamma
from math import log

#################################################################################################################

def averaging_data(timestamps, data_values): # need dictionary with data and timestamps
    '''Returns average hourly data based on the input data along with the
    maximum value from the dataset.
    '''
    hourly_data = {'00': [], '01': [], '02': [], '03': [], '04': [],
                   '05': [], '06': [], '07': [], '08': [], '09': [],
                   '10': [], '11': [], '12': [], '13': [], '14': [],
                   '15': [], '16': [], '17': [], '18': [], '19': [],
                   '20': [], '21': [], '22': [], '23': []}
    # Finding max value, useful to find highest wind speed so a turbine can be chosen that can survive
    max_data_value = 0
    n = 0
    for timestamp in timestamps: # sorting by hour
        hourly_data[timestamp[11:13]].append(data_values[n]) # edit so you can get the right part of the timestamp
        if data_values[n] > max_data_value:
            max_data_value = data_values[n]
        n += 1
    average_by_hour = []
    for hour in hourly_data.keys():
        average_by_hour.append(mean(hourly_data[hour]))
    return average_by_hour, max_data_value

def optimal_windspeed(windspeeds):
    '''Returns optimal rated speed which a turbine should most
    closely match, for a given set of windspeeds.
    '''
    print('Windspeeds:', windspeeds)
    windspeed_std = std(windspeeds) #standard deviation
    windspeed_mean = mean(windspeeds)

    #optimal windspeed factors
    if windspeed_std != 0:
        k = (windspeed_std/windspeed_mean)**(-1.086)
        c = windspeed_mean/gamma(1+1/k)

        v_op = 0.514*c*((k+2)/k)**(1/k) # 0.514 is to convert from knots to m/s
    else:
        v_op = '-'
        k = '-'
    #print(v_op)
    return v_op, k
 
def optimal_turbine(v_op, gust_speeds):
    '''Returns the optimal turbine, given the optimal rated
    speed and the gust speeds at that location.
    '''

    if v_op == '-':
        return '-'


    turbine_data = pd.read_csv('./Data/turbine_data/turbine_data.csv')
    rated_speeds = turbine_data['Rated speed']
    turbine_heights = turbine_data['Turbine height']
    survival_speeds = turbine_data['Survival speed']
    cut_in_speeds = turbine_data['Cut-in speed']
    cut_off_speeds = turbine_data['Cut-off speed']

    z_0 = 0.1 # Very rough estimate, but it's probably hard to be more accurate. This is a typical value for countryside
    z_ref = 10 # Height of most MIDAS wind sensors, some may not be 10m but we have no way of knowing actual height

    n = 0 # Counter
    min_difference = 10000

    best_turbine = 1
    for z in turbine_heights:
        HAspeed = v_op*((log(float(z)/z_0))/(log(z_ref/z_0)))
        HAmaxgustspeed = (0.514*max(gust_speeds))*((log(float(z)/z_0))/(log(z_ref/z_0)))

        rated_speed = rated_speeds.iloc[n]
        if rated_speed == ' -':
            continue

        survival_speed = survival_speeds.iloc[n]
        if str(survival_speed) == ' -': # Some turbines don't give this value, so it's assumed to be very high
            survival_speed = 1000

        cut_in_speed = cut_in_speeds.iloc[n]
        if cut_in_speed == ' -': # Some turbines don't give this value, so it's assumed to be zero
            cut_in_speed = 0
        
        cut_off_speed = cut_off_speeds.iloc[n]
        if cut_off_speed == ' -': # Some turbines don't give this value, so it's assumed to be very high
            cut_off_speed = 0
        # Finding turbine with rated speed closest to height adjusted windspeed, which
        # can survive the maximum gust speed (with a safety factor of 1.5)
        if abs(HAspeed-float(rated_speed)) < min_difference and float(survival_speed) > 1.5*HAmaxgustspeed and float(cut_off_speed) > float(cut_in_speed):
            min_difference = abs(HAspeed-float(rated_speeds.iloc[n]))
            best_turbine = n
        n += 1
    turbine = turbine_data.iloc[best_turbine]
    #print(min_difference)
    #print(turbine)
    return turbine

def turbine_power(turbine, windspeeds, k):
    '''Returns the power output of a given turbine, for given windspeeds.'''
    wind_powers = []

    if k == '-':
        for windspeed in windspeeds:
            wind_powers.append(0)
        return wind_powers

    rated_speed = float(turbine['Rated speed']) # may be list, may need to fix
    cut_in_speed = float(turbine['Cut-in speed'])
    rated_power = float(turbine['Rated power'])
    
    cut_off_speed = turbine['Cut-off speed']
    if str(cut_off_speed) == ' -': # Some turbines don't give this value, so it's assumed to be very high
        cut_off_speed = 1000
    cut_off_speed = float(cut_off_speed)

    turbine_diameter = float(turbine['Turbine diameter'])
    z = float(turbine['Turbine height'])
    z_0 = 0.1 # Very rough estimate, but it's probably hard to be more accurate. This is a typical value for countryside
    z_ref = 10 # Height of most MIDAS wind sensors, some may not be 10m but we have no way of knowing actual height
    
    for windspeed in windspeeds:
        HAspeed = windspeed*((log(z/z_0))/(log(z_ref/z_0)))
        #print(HAspeed)
        if HAspeed > cut_off_speed or HAspeed < cut_in_speed:
            wind_powers.append(0)
        elif HAspeed > cut_in_speed and HAspeed < rated_speed:
            wind_power = rated_power*((HAspeed**k - cut_in_speed**k)/(rated_speed**k - cut_in_speed**k))
            wind_powers.append(wind_power)
        else:
            wind_powers.append(rated_power)
    power_per_area = mean(wind_powers)/(((3**0.5)/4)*((5*turbine_diameter)**2)) # Must be seperated by at least 5x turbine diameter. Assumed triangle formation
    wind_powers.append(power_per_area)
    #print(wind_powers)
    #print('----------------------')
    return wind_powers

def solar_power(solar_radiation):
    '''Returns the solar panel output for given global irradiation values.'''
    solar_powers = []
    for rad in solar_radiation:
        solar_powers.append(rad*0.26*0.75)
    return solar_powers
