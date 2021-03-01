from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
import json 
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd
import io
from tabulate import tabulate
import os
import numpy as np 
import bs4 as bs 
from requests_html import HTMLSession
import datetime
import uuid
from datetime import  timezone


def real_time_intakes():
    '''
    DESCRIPTION:
    Calls the National Grid online publication of incoming flows to the NTS and produces
    two numpy tables, one with zonal intakes and one with terminal intakes.
    Units are mcm/day.
    '''
    #--- opening intakes webpage ---#
    os.system('cls' if os.name == 'nt' else 'clear')
    # reading html from webpage 
    url = 'https://mip-prd-web.azurewebsites.net/InstantaneousView'
    session = HTMLSession()
    r = session.get(url)
    r.html.render()
    #--- converting all the information to a table ---#
    soup = bs.BeautifulSoup(r.html.html,'lxml') 
    

    # CREATION OF TABLE FROM HTML TABLE
    table = []
    for tr in soup.find_all('tr')[1:]:
        tds = tr.find_all('td')
        row = []
        for i in tds:
            row.append(i.text)
        table.append(row)
    # TABLE TO DATAFRAME
    table = pd.DataFrame(table)
    #--- ontaining only the required values ---#
    table = table.to_numpy()[4:,1:]
    # zone_names = table[1:29,0]
    # latest_zone_value = table[1:29,6]
    # latest_zone_value[6] = latest_zone_value[6][1:]
    # latest_zone_value = latest_zone_value.astype(np.float)
    # zone_supply = np.concatenate(([zone_names],[latest_zone_value]),axis=0).T

    # zone_supply_pd = pd.DataFrame(zone_supply)

    terminal_names = table[43:52,0]
    data = []
    for i in range(1,7):
        latest_terminal_value = table[43:52,i].astype(np.float)
        time = table[42,i]
        now = datetime.datetime.now()
        time = [now.replace(hour=int(time[:2]), minute=int(time[-2:]),second = 0,microsecond=0) for i in range(len(latest_terminal_value))]
        terminal_supply = np.concatenate(([terminal_names],[time],[latest_terminal_value]),axis=0).T

        terminal_supply_pd = pd.DataFrame(terminal_supply)

        # overall_df = pd.concat((zone_supply_pd,terminal_supply_pd),axis=1,ignore_index=True)
        overall_df = terminal_supply_pd

        data.append(terminal_supply_pd)

    return data


def update_triple_store():
    data = real_time_intakes()
    for terminal_supply in data:
        print('Updating Triple Store...')

        component_namespace = "http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/"
        BIPS = "<"+component_namespace+"BactonIPsTerminal>"
        BUKS = "<"+component_namespace+"BactonUKCSTerminal>"
        BAR  = "<"+component_namespace+"BarrowTerminal>"
        EAS  = "<"+component_namespace+"EasingtonTerminal>"
        IOG  = "<"+component_namespace+"IsleofGrainTerminal>"
        MH   = "<"+component_namespace+"MilfordHavenTerminal>"	
        SF   = "<"+component_namespace+"StFergusTerminal>"
        TEES = "<"+component_namespace+"TeessideTerminal>"
        THED = "<"+component_namespace+"TheddlethorpeTermin>"

        term_uris = [BIPS,BUKS,BAR,EAS,IOG,MH,SF,TEES,THED]

        for i in range(len(term_uris)):
            time_UTC = str(terminal_supply.values[i,1].strftime("%Y-%m-%dT%H:%M:%S"))
            gas_volume = str(terminal_supply.values[i,2])
            gas_uuid = uuid.uuid1()
            query = '''PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
            PREFIX compa:   <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>

            INSERT DATA
            { compa:%s rdf:type comp:IntakenGas.
            %s comp:hasTaken compa:%s.
            compa:%s comp:hasGasVolume %s.
            compa:%s comp:atUTC "%s"^^xsd:dateTime .} '''%(gas_uuid,term_uris[i],gas_uuid,gas_uuid,gas_volume,gas_uuid,time_UTC)
            sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
            sparql.setMethod(POST)
            sparql.setQuery(query)
            ret = sparql.query()

        os.system('cls' if os.name == 'nt' else 'clear')
        print('Done!')
    return 

'''
TODO:

- Delete taken gas if older than X or have no date.

'''
update_triple_store()
