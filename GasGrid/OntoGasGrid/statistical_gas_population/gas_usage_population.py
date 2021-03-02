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



data = pd.read_excel('Copy of LSOA_domestic_gas_2010-19.xlsx',sheet_name='2019',skiprows=1)
LSOA_codes = data['Lower Layer Super Output Area (LSOA) Code'].values
met_num = data['Number of consuming meters'].values
consump = data['Consumption (kWh)'].values
non_met_num = data['Number of non-consuming meters'].values

start_time = "2019-01-01T12:00:00"
end_time = "2019-12-31T12:00:00"
for i in tqdm(range(len(consump))):
    region = LSOA_codes[i]
    meters = met_num[i]
    non_meters = non_met_num[i]
    cons = consump[i]

    used_uuid = uuid.uuid1()

    query = '''PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX gas:    <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#>
    PREFIX compa:   <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>
    PREFIX ons:     <http://statistics.data.gov.uk/id/statistical-geography/>

    INSERT DATA
    { compa:%s rdf:type comp:OfftakenGas.
      ons:%s comp:hasUsed compa:%s.
      ons:%s gas:hasConsumingGasMeters %s.
      compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
      compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .
      ons:%s gas:hasNonConsumingGasMeters %s. 
      compa:%s comp:hasGasEnergy %s.
      } '''%(used_uuid,region,used_uuid,region,meters,used_uuid,start_time,used_uuid,end_time,region,non_meters,used_uuid,cons)

    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    ret = sparql.query()


