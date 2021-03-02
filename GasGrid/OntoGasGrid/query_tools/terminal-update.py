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
    Calls the National Grid online publication of incoming flows to the NTS
    Produces table with Terminals, times, and values.
    '''
    # clearing terminal
    os.system('cls' if os.name == 'nt' else 'clear')
    # opening and rendering HTML
    url = 'https://mip-prd-web.azurewebsites.net/InstantaneousView'
    session = HTMLSession()
    r = session.get(url)
    r.html.render()
    # convert to soup to parse as table
    soup = bs.BeautifulSoup(r.html.html,'lxml') 

    # parse table from HTML
    table = []
    for tr in soup.find_all('tr')[1:]:
        tds = tr.find_all('td')
        row = []
        for i in tds:
            row.append(i.text)
        table.append(row)
    # converting to dataframe
    table = pd.DataFrame(table)
    # converting to numpy with only desired bits
    table = table.to_numpy()[4:,1:]

    # terminal names from tables
    terminal_names = table[43:52,0]
    data = []
    # iterating over temporal values
    for i in range(1,7):
        latest_terminal_value = table[43:52,i].astype(np.float)
        time = table[42,i]
        # converting time to datetime format
        now = datetime.datetime.now()
        time = [now.replace(hour=int(time[:2]), minute=int(time[-2:]),second = 0,microsecond=0) for i in range(len(latest_terminal_value))]
        # creating row in table 
        terminal_supply = np.concatenate(([terminal_names],[time],[latest_terminal_value]),axis=0).T

        terminal_supply_pd = pd.DataFrame(terminal_supply)

        overall_df = terminal_supply_pd

        data.append(terminal_supply_pd)
    # return data table
    return data


def update_triple_store():
    # calling function to get most recent values of terminal gas rate
    data = real_time_intakes()
    for terminal_supply in data:
        print('Updating Terminal Values for ',terminal_supply.values[0,1],' ...')

        # defining namespaces of each terminal
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

        # iterating over terminals
        for i in range(len(term_uris)):
            # convert to proper datetime format
            time_UTC = str(terminal_supply.values[i,1].strftime("%Y-%m-%dT%H:%M:%S"))
            # get gas volume
            gas_volume = str(terminal_supply.values[i,2])
            # create UUID for IntakenGas
            gas_uuid = uuid.uuid1()
            query = '''PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
            PREFIX compa:   <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>

            INSERT DATA
            { compa:%s rdf:type comp:IntakenGas.
            %s comp:hasTaken compa:%s.
            compa:%s comp:atGasVolumeRate %s.
            compa:%s comp:atUTC "%s"^^xsd:dateTime .} '''%(gas_uuid,term_uris[i],gas_uuid,gas_uuid,gas_volume,gas_uuid,time_UTC)
            sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
            sparql.setMethod(POST) # POST query, not GET
            sparql.setQuery(query)
            ret = sparql.query()
    # clear terminal
    os.system('cls' if os.name == 'nt' else 'clear')
    return 

def continuous_update():
    while True:
        start = time.time()
        update_triple_store()
        end = time.time()
        print('waiting for update...')
        # wait for 12 minutes taking into account time to update queries
        for i in tqdm(range(60*12-int((end-start)))):
            time.sleep(1)
    return 

def single_update():
        update_triple_store()
        return 