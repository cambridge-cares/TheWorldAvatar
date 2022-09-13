###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 27 Jan 2022                           #
###############################################

""" 
This module investigates whether a relevant percentage of domestic buildings 
in King's Lynn area has renewable energy generation by wind and solar
https://epc.opendatacommunities.org/docs/api/domestic
"""

import json

import pandas as pd
import requests


# Personal authentification token
auth = ''

# Local authority code for King's Lynn and West Norfolk
loc_auth = 'E07000146'

# Note: The API is only designed to support result sets of up to 10,000 records at a time, 
# with a maximum page size of 5,000. Hence, if more than 10,000 records need to be processed,
# various search filter combinations need to be used 
# --> energy band & floor area used as additional filters
eband = 'abcdefg'
areas = ['s', 'm', 'l', 'xl', 'xxl']

# Specify API endpoints
url_epc_base = 'https://epc.opendatacommunities.org/api/v1/domestic/search?' +\
               'local-authority={}&energy-band={}&floor-area={}&size={}&from={}'
headers = {'Authorization': 'Basic {}'.format(auth),
           'Accept': 'application/json'}

# API has maximum page size of 5000 --> pagination required
size = 5000

#####################################################################################

# Initialise epc list
all_epcs = []

# Loop over all energy bands and floor area ranges
for band in eband:
    for area in areas:
        offset = 0
        further_results = True

        while further_results:
            url_epc = url_epc_base.format(str(loc_auth), str(band), str(area), str(size), str(offset))
            r = requests.get(url=url_epc, headers=headers)
            if r.status_code == 200:
                epcs = r.json()['rows']
                all_epcs.extend(epcs)
                if (len(epcs) < size) or ((offset + size) >= 10000):
                    further_results = False
                offset += size
            else:
                further_results = False

# Create DataFrame with all EPCs
epc_data = pd.DataFrame(all_epcs)
epcs_consolidated = epc_data.sort_index(axis=1)
epcs_consolidated = epcs_consolidated.sort_values(by=['inspection-date'], ascending=True)
epcs_consolidated.drop_duplicates(subset=['uprn'], keep='last', inplace=True)
# Convert relevant columns to numeric
epcs_consolidated[['wind-turbine-count', 'photo-supply']] = epcs_consolidated[['wind-turbine-count', 'photo-supply']].apply(pd.to_numeric) 
epcs_consolidated[['wind-turbine-count', 'photo-supply']] = epcs_consolidated[['wind-turbine-count', 'photo-supply']].fillna(0)
# Extract only entries in relevant postcode
epcs_consolidated_subset = epcs_consolidated[epcs_consolidated['postcode'].str.contains('PE30')]
wind = epcs_consolidated_subset[epcs_consolidated_subset['wind-turbine-count'] > 0]
wind.to_csv('./data/properties_with_wind.csv')
pv = epcs_consolidated_subset[epcs_consolidated_subset['photo-supply'] > 0]
pv.to_csv('./data/properties_with_pv.csv')
