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
        print('Updating Terminal Values for ',terminal_supply.values[0,1],' ...')

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
            compa:%s comp:atGasVolumeRate %s.
            compa:%s comp:atUTC "%s"^^xsd:dateTime .} '''%(gas_uuid,term_uris[i],gas_uuid,gas_uuid,gas_volume,gas_uuid,time_UTC)
            sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
            sparql.setMethod(POST)
            sparql.setQuery(query)
            ret = sparql.query()

    os.system('cls' if os.name == 'nt' else 'clear')
    return 


def delete_gas_history():
    
    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete
    where {
    ?gas comp:atGasVolumeRate ?p.
    ?term comp:hasTaken ?gas .
    ?gas rdf:type comp:IntakenGas.
    ?gas comp:atUTC ?s .}'''

    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST)
    sparql.setQuery(query)
    ret = sparql.query()
    return

delete_gas_history()
 
while True:
    start = time.time()
    update_triple_store()
    end = time.time()
    print('waiting for update...')
    for i in tqdm(range(60*12-int((end-start)))):
        time.sleep(1)
