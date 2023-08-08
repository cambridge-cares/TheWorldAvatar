###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 18 Feb 2022                           #
###############################################

""" 
This module investigates the possibility to enrich downloaded flood 
monitoring stations with information about up-/downstream stations

https://environment.data.gov.uk/flood-monitoring/doc/reference
https://check-for-flooding.service.gov.uk/station/{station_id}
"""

import json
import time
import os
import re
import pandas as pd
import requests
from bs4 import BeautifulSoup
from pathlib import Path
from selenium import webdriver


# Initialise selenium webdriver, which is required to load dynamic 
# "check for floodrisk" website to scrape related up- & downstream stations
p = os.path.join(Path(__file__).parent.absolute(), r'..\resources\chromedriver.exe')
driver = webdriver.Chrome(executable_path=p)

# Get all flood monitoring stations (from Flood-Monitoring API)
url = 'https://environment.data.gov.uk/flood-monitoring/id/stations'
resp = requests.get(url).json()
n = len(resp['items'])
print(f'Total number of retrieved stations: {n}')

# Create output DataFrame and initialise dict for new rows to add
cols=['RLOIid', 'riverName', 'RLOIid_upstream', 'RLOIid_downstream']
df = pd.DataFrame(index=range(n), columns=cols)
row = {c: None for c in cols}

# Loop over all stations
i = 0
for s in resp['items']:
    # Only consider river water level stations 
    if not s.get('riverName') :
        continue
    id = s['RLOIid']
    # Get URL to "check for floodrisk" website for current station
    # to extract RLOIid for potential up- & downstream stations
    url = f'https://check-for-flooding.service.gov.uk/station/{id}'

    # Create dynamic website with JavaScript in selenium driver
    driver.get(url)
    # Get content of dynamically loaded website 
    html = driver.page_source
    soup = BeautifulSoup(html, "html.parser")

    # Get dynamically created JSON content from website
    tag_text = soup.find('script', text = re.compile('window.flood.model'))
    # Skip all station websites without relevant information
    if not tag_text:
        continue
    tag_text = tag_text.get_text()
    # Condition text to be JSON parsable
    t = tag_text.strip()
    t = t.replace('\n', '')
    t = t[t.find('{'):]
    # Load JSON as dictionary
    d = json.loads(t)

    # Get station details
    details = d.get('station').get('riverNavigation')
    # Skip all station websites without relevant riverNavigation data
    if not details:
        continue
    row['RLOIid'] = id
    row['riverName'] = details['river_name']
    row['RLOIid_upstream'] = details['up']
    row['RLOIid_downstream'] = details['down']
    
    # Add data to DataFrame
    df.iloc[i, :] = row
    i += 1

# Drop empty rows and write DataFrame
df_cleaned = df.dropna(axis=0, how='all')
df_cleaned.to_csv(os.path.join(Path(__file__).parent.absolute(), r'..\data\river_stations.csv'))

