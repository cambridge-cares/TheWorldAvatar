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
import datetime
import uuid
from datetime import  timezone



data = pd.read_excel('data/2021-sub-regional-fuel-poverty-tables.xlsx',sheet_name='Table 3',skiprows=2,skipfooter=9)

print(data)

LSOA_codes = data['LSOA Code'].values
house_num = data['Number of households1'].values
poor_num = data['Number of households in fuel poverty1'].values

start_time = "2019-01-01T12:00:00"
end_time = "2019-12-31T12:00:00"

total = len(house_num)
n_compile = total / 10
remainder = total % 10
n_compile = int(n_compile)
len_query = np.zeros(n_compile+2)
for i in range(1,len(len_query)-1):
        len_query[i] = len_query[i-1] + 10
        len_query[-1] = len_query[-2] + remainder

for g in tqdm(range(len(len_query)-1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        houses = house_num[i]
        poor = poor_num[i]

        house_uuid = uuid.uuid1()

        
        query = '''
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ofp:    <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#>
        PREFIX ofpt:   <http://www.theworldavatar.com/kb/ontofuelpoverty/abox/>
        PREFIX ons:     <http://statistics.data.gov.uk/id/statistical-geography/>
        PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>

        INSERT DATA
        { 
        ons:%s ofp:hasHouseholds  ofpt:%s.
        ofpt:%s ofp:validFrom "%s"^^xsd:dateTime;
                ofp:validTo  "%s"^^xsd:dateTime;
                rdf:type ofp:households;
                ofp:numberofhouseholds %s;
                ofp:fuelpoorhouseholds %s.
        '''%(region,
        house_uuid, 
        house_uuid, 
        start_time,
        end_time,
        houses,
        poor)

        middle_num = int(len_query[g+1]-len_query[g])-2
        for j in range(middle_num):
                region = LSOA_codes[i+j+1]
                houses = house_num[i+j+1]
                poor = poor_num[i+j+1]

                house_uuid = uuid.uuid1()

                query += '''
                ons:%s ofp:hasHouseholds  ofpt:%s.
                ofpt:%s ofp:validFrom "%s"^^xsd:dateTime;
                ofp:validTo  "%s"^^xsd:dateTime;
                rdf:type ofp:households;
                ofp:numberofhouseholds %s;
                ofp:fuelpoorhouseholds %s.
                '''%(region,
                house_uuid, 
                house_uuid, 
                start_time,
                end_time,
                houses,
                poor)

        region = LSOA_codes[int(len_query[g+1])-1]
        houses = house_num[int(len_query[g+1])-1]
        poor = poor_num[int(len_query[g+1])-1]

        house_uuid = uuid.uuid1()

        query += '''
                ons:%s ofp:hasHouseholds  ofpt:%s.
                ofpt:%s ofp:validFrom "%s"^^xsd:dateTime;
                ofp:validTo  "%s"^^xsd:dateTime;
                rdf:type ofp:households;
                ofp:numberofhouseholds %s;
                ofp:fuelpoorhouseholds %s.}
        '''%(region,
                house_uuid, 
                house_uuid, 
                start_time,
                end_time,
                houses,
                poor)


        DEF_NAMESPACE = 'ontogasgrid'
        LOCAL_KG = "http://localhost:9999/blazegraph"
        LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'

        sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
        sparql.setMethod(POST) # POST query, not GET
        sparql.setQuery(query)
        ret = sparql.query()

