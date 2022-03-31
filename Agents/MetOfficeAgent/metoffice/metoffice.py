###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 09 Feb 2022                           #
###############################################

""" 
This module investigates the capabilities of the MetOffer package
as simple wrapper for the Met Office DataPoint API
https://github.com/sludgedesk/metoffer

How to install MetOffer:
https://github.com/sludgedesk/metoffer/issues/4

Further documentation for function and arguments
https://github.com/sludgedesk/metoffer/blob/master/metoffer.py
"""

import metoffer
import pprint

# Specify API key
api_key = '781fcade-8eaa-44c7-84e8-014f92857aa1'
# Location of King's Lynn
lat = 52.751
lon = 0.392

# Create MetOffice client
metclient = metoffer.MetOffer(api_key)

#
# OBSERVATIONS
#

# Get closest weather observation for King's Lynn
obs = metclient.nearest_loc_obs(lat, lon)
observation = metoffer.Weather(obs)
print('\nClosest observation station to King\'s Lynn: {}\n'.format(observation.name))
pprint.pprint(observation.data[0])

# Get all observations sites
sites = metclient.loc_observations(metoffer.SITELIST)
sites = sites['Locations']['Location']
print('\nTotal number of observation stations: {}\n'.format(len(sites)))
i=0
for site in sites:
    i+=1
    id = site['id']
    if i<10:
        obs = metclient.loc_observations(id)

        try:
            observation = metoffer.Weather(obs)
            print('Station name: {:>50}, station ID: {:>10}'.format(observation.name, id))
        except:
            print('Error while retrieving data for station ID: {:>10}'.format(id))

#
# FORECASTS
#

# Get closest weather forecast for King's Lynn
fc = metclient.nearest_loc_forecast(lat, lon, metoffer.THREE_HOURLY)
forecast = metoffer.Weather(fc)
print('\nClosest forecast station to King\'s Lynn: {}\n'.format(forecast.name))
pprint.pprint(forecast.data[0])

# Get all forecasts sites
sites = metclient.loc_forecast(metoffer.SITELIST, step="")
sites = sites['Locations']['Location']
print('\nTotal number of forecast stations: {}\n'.format(len(sites)))
i=0
for site in sites:
    i+=1
    id = site['id']
    if i<10:
        fc = metclient.loc_forecast(id, metoffer.THREE_HOURLY)
    
        try:
            forecast = metoffer.Weather(fc)
            print('Station name: {:>50}, station ID: {:>10}'.format(forecast.name, id))
        except:
            print('Error while retrieving data for station ID: {:>10}'.format(id))