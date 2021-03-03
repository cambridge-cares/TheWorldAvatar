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

total = len(met_num)
n_compile = total / 100
remainder = total % 100 
n_compile = int(n_compile)
len_query = np.zeros(n_compile+2)
for i in range(1,len(len_query)-1):
    len_query[i] = len_query[i-1] + 100
len_query[-1] = len_query[-2] + remainder

for g in tqdm(range(len(len_query)-1)):
    i = int(len_query[g])
    region = LSOA_codes[i]
    meters = met_num[i]
    non_meters = non_met_num[i]
    cons = consump[i]

    used_uuid = uuid.uuid1()
    met_uuid = uuid.uuid1()

    query = '''PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX gas:    <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#>
    PREFIX compa:   <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>
    PREFIX ons:     <http://statistics.data.gov.uk/id/statistical-geography/>

    INSERT DATA
    { compa:%s rdf:type comp:OfftakenGas.
      ons:%s comp:hasUsed compa:%s.
      compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
      compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .
      compa:%s comp:hasGasEnergy %s.
      compa:%s rdf:type gas:GasMeters.
      ons:%s gas:hasGasMeters compa:%s.
      compa:%s gas:hasConsumingGasMeters %s.
      compa:%s gas:hasNonConsumingGasMeters %s. 
      compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
      compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .
       '''%(used_uuid,region,used_uuid,used_uuid,start_time,\
         used_uuid,end_time,used_uuid,cons,met_uuid,region,\
           met_uuid,met_uuid,meters,met_uuid,non_meters,met_uuid,start_time,met_uuid,end_time)
    
    middle_num = int(len_query[g+1]-len_query[g])-2
    for j in range(middle_num):
        region = LSOA_codes[i+j+1]
        meters = met_num[i+j+1]
        non_meters = non_met_num[i+j+1]
        cons = consump[i+j+1]

        used_uuid = uuid.uuid1()
        met_uuid = uuid.uuid1()

        query += '''
        compa:%s rdf:type comp:OfftakenGas.
        ons:%s comp:hasUsed compa:%s.
        compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
        compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .
        compa:%s comp:hasGasEnergy %s.
        compa:%s rdf:type gas:GasMeters.
        ons:%s gas:hasGasMeters compa:%s.
        compa:%s gas:hasConsumingGasMeters %s.
        compa:%s gas:hasNonConsumingGasMeters %s. 
        compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
        compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .
        '''%(used_uuid,region,used_uuid,used_uuid,start_time,\
          used_uuid,end_time,used_uuid,cons,met_uuid,region,\
            met_uuid,met_uuid,meters,met_uuid,non_meters,met_uuid,start_time,met_uuid,end_time)
      
        
    region = LSOA_codes[int(len_query[g+1])-1]
    meters = met_num[int(len_query[g+1])-1]
    non_meters = non_met_num[int(len_query[g+1])-1]
    cons = consump[int(len_query[g+1])-1]

    used_uuid = uuid.uuid1()
    met_uuid = uuid.uuid1()

    query += '''
        compa:%s rdf:type comp:OfftakenGas.
        ons:%s comp:hasUsed compa:%s.
        compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
        compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .
        compa:%s comp:hasGasEnergy %s.
        compa:%s rdf:type gas:GasMeters.
        ons:%s gas:hasGasMeters compa:%s.
        compa:%s gas:hasConsumingGasMeters %s.
        compa:%s gas:hasNonConsumingGasMeters %s. 
        compa:%s comp:hasStartUTC "%s"^^xsd:dateTime .
        compa:%s comp:hasEndUTC "%s"^^xsd:dateTime .}
        '''%(used_uuid,region,used_uuid,used_uuid,start_time,\
          used_uuid,end_time,used_uuid,cons,met_uuid,region,\
            met_uuid,met_uuid,meters,met_uuid,non_meters,met_uuid,start_time,met_uuid,end_time)

    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    ret = sparql.query()


