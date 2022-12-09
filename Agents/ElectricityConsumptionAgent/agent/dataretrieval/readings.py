
import json
from tqdm import tqdm
import time
import numpy as np
import pandas as pd
from tabulate import tabulate
import uuid
from datetime import timezone

import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import input_query_template
#from agent.kgutils.tsclient import TSClient
from agent.errorhandling.exceptions import *
from agent.kgutils.querytemplates import input_query_template

# Initialise logger
logger = agentlogging.get_logger("prod")
DEF_NAMESPACE = "ontogasgrid"
LOCAL_KG = "http://localhost:8080/blazegraph"
QUERY_ENDPOINT= UPDATE_ENDPOINT = LOCAL_KG + "/namespace/" + DEF_NAMESPACE + "/sparql"

def read_from_excel(year:str,
                    keyword:str):
 '''
     Return list of readings from Excel
     
     Arguments:
     year: the number of year of which the data you may want to read
     keyword: 'Electricity'/'Gas'/'Fuel Poverty'
             the content of the data of which you may want to read
 '''
 if keyword == 'Electricity':
    try:
        #./Data/LSOA_domestic_elec_2010-20.xlsx
        data = pd.read_excel('./Data/LSOA_domestic_elec_2010-20.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
        logger.error("Excel file can not be read")
        raise InvalidInput("Excel file can not be read -- try fixing by using absolute path") from ex

    logger.info('Retrieving Electricity consumption data from Excel ...')
    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values
    logger.info('Electricity consumption data succesfully retrieved')

# Remove the 'null' data
    for i in LSOA_codes, met_num, consump:
     i[np.where(i=='')] = 0

    logger.info('Create triples to instantiate Electrical Consumption data ...')
    
 return LSOA_codes, met_num, consump

def upload_data_to_KG(year:str,
                keyword:str,
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):

# Read the Excel and extract relavent data
# Initialisation 
    LSOA_codes = []
    met_num = []
    consump = []
    LSOA_codes, met_num, consump = read_from_excel(year, keyword)

# Define the reference time
    start_time = year + "-01-01T12:00:00"
    end_time = year + "-12-31T12:00:00"

# Split the queries into Batches
# Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile + 2)

    for i in range(1, len(len_query) - 1):
        len_query[i] = len_query[i - 1] + 10
        len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query) - 1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        meters = met_num[i]
        cons = consump[i]

        used_uuid = uuid.uuid1()
        met_uuid = uuid.uuid1()
        kw_uuid = uuid.uuid1()
        mes_uuid = uuid.uuid1()

        # Initialise update query
        query = f"INSERT DATA" + "{"

        triples = input_query_template(
                mes_uuid,
                used_uuid,
                start_time,
                end_time,
                region,
                kw_uuid,
                cons,
                met_uuid,
                meters) 

        middle_num = int(len_query[g + 1] - len_query[g]) - 2
        for j in range(middle_num):
            region = LSOA_codes[i + j + 1]
            meters = met_num[i + j + 1]
            cons = consump[i + j + 1]

            used_uuid = uuid.uuid1()
            met_uuid = uuid.uuid1()
            kw_uuid = uuid.uuid1()
            mes_uuid = uuid.uuid1()

        triples += input_query_template(
                mes_uuid,
                used_uuid,
                start_time,
                end_time,
                region,
                kw_uuid,
                cons,
                met_uuid,
                meters) 

        region = LSOA_codes[int(len_query[g + 1]) - 1]
        meters = met_num[int(len_query[g + 1]) - 1]
        cons = consump[int(len_query[g + 1]) - 1]

        used_uuid = uuid.uuid1()
        met_uuid = uuid.uuid1()
        kw_uuid = uuid.uuid1()
        mes_uuid = uuid.uuid1()

        triples += input_query_template(
                mes_uuid,
                used_uuid,
                start_time,
                end_time,
                region,
                kw_uuid,
                cons,
                met_uuid,
                meters) 

        query +=triples
        query += "}"

        # Instantiate all non-time series triples
        f = open("query.txt", "w")
        f.write(query)
        f.close()

        kg_client = KGClient(query_endpoint, update_endpoint)
        query_endpoint = kg_client.kg_client.setUpdateEndpoint(update_endpoint)
        kg_client.performUpdate(query)

    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query for Electricity Consumption successfully performed.')
    return len(len_query) - 1

if __name__ == '__main__':
# years = ['2018','2017','2016','2015']
# for year in years:
#         upload_year(year)
 print("\nUploading the Electricity consumption data")
 logger.info("Uploading the Electricity consumption data...")

 t1= time.time()
 len_query=upload_data_to_KG("2020",'Electricity')
 print(f"Number of instantiated Electricity consumption data per LOSA output area :{len_query}")
 t2= time.time()
 diff = t2 - t1
 print(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
 logger.info(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')



