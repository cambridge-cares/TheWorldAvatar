################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to read the Electricity consumption data, 
# and instantiate both the data along with timeseries
# into the Knowledge graph

from tqdm import tqdm
import time
import numpy as np
import pandas as pd
import uuid

import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.utils.readings_mapping import DATACLASS, TIME_FORMAT
from agent.errorhandling.exceptions import *
from agent.datamodel.spec import *

#from agent.kgutils.tsclient import jpsBaseLibView
from agent.kgutils.tsclient import TSClient

# Initialise logger
logger = agentlogging.get_logger("prod")

def read_from_excel(year:str) -> list:
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel('./Data/LSOA_domestic_elec_2010-20.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            logger.error("Excel file can not be read")
            raise InvalidInput("Excel file can not be read -- try fixing by using absolute path") from ex

    logger.info('Retrieving Electricity consumption data from Excel ...')
    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values
    logger.info('Electricity consumption data succesfully retrieved')
    
    # Replace the 'null' data to 'NaN'
    met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(met_num) else met_num for met_num in met_num]  
    consump =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(consump) else consump for consump in consump]   

    return LSOA_codes, met_num, consump

def upload_data_to_KG(year:str = '2020',
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the data into Blazegraph
        
        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''

# Initialisation 
    LSOA_codes = []
    met_num = []
    consump = []
    datairi = []
    value = []

# Read the Excel and extract relavent data
    LSOA_codes, met_num, consump = read_from_excel(year)

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
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    logger.info('Create triples to instantiate Electrical Consumption data ...')
    for i in range(1, len(len_query) - 1):
        len_query[i] = len_query[i - 1] + 10
        len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query) - 1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        meters = met_num[i]
        cons = consump[i]

        # Initialise update query
        query = f"INSERT DATA" + "{"

        used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
        met_uuid = COMPA + 'ElectricityMeter_' + str(uuid.uuid4())
        kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
        mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
        datairi.append(mes_uuid)
        value.append(cons)
        datairi.append(met_uuid)
        value.append(meters)

        triples = electricity_update_template(
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

            used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
            met_uuid = COMPA + 'ElectricityMeter_' + str(uuid.uuid4())
            kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
            mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
            datairi.append(mes_uuid)
            value.append(cons)
            datairi.append(met_uuid)
            value.append(meters)

            triples += electricity_update_template(
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

        used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
        met_uuid = COMPA + 'ElectricityMeter_' + str(uuid.uuid4())
        kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
        mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
        datairi.append(mes_uuid)
        value.append(cons)
        datairi.append(met_uuid)
        value.append(meters)

        triples += electricity_update_template(
                mes_uuid,
                used_uuid,
                start_time,
                end_time,
                region,
                kw_uuid,
                cons,
                met_uuid,
                meters) 

        query +=triples + "}"

        # Instantiate all non-time series triples
        kg_client = KGClient(query_endpoint, update_endpoint)
        kg_client.performUpdate(query)

    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query for Electricity Consumption successfully performed.')
    return total, datairi, value

def upload_timeseries_to_KG(datairi:list, value:list, year:str, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT) -> int:
    '''
            Adds time series data to instantiated time series IRIs
        
        Arguments:
            datairi - list of IRIs of instantiated time series data
            value - respective value of instantiated time series data
            year - specify the year of time
            query_endpoint: str = QUERY_ENDPOINT,
            update_endpoint: str = UPDATE_ENDPOINT
            '''
    # Initialise time list for TimeSeriesClient's bulkInit function
    times = [f"{year}-01-01T12:00:00" for _ in datairi]

    # Initialise TimeSeries Clients
    kg_client = KGClient(query_endpoint,update_endpoint)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)
    with ts_client.connect() as conn:
            ts_client.tsclient.bulkInitTimeSeries(datairi, [DATACLASS]*len(datairi), TIME_FORMAT, conn)
    logger.info('Time series triples for electricity consumption/meters per LSOA via Java TimeSeriesClient successfully instantiated.')

    # Upload TimeSeries data
    added_ts = 0
    ts_list = []
    for i in range(len(times)):
        added_ts += 1
        ts = TSClient.create_timeseries(times[i],datairi[i],value[i])
        ts_list.appened(ts)

    with ts_client.connect() as conn:
        ts_client.tsclient.bulkaddTimeSeriesData(ts_list, conn)
    logger.info(f'Time series data for electricity consumption for {added_ts} LSOA area successfully added.')

    return added_ts

def upload_all(year: str = '2020',query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
    
    # instantiate the consumption data
    print("\nUploading the Electricity consumption data:")
    logger.info("Uploading the Electricity consumption data...")
    t1 = time.time()
    len_query,  datairi, value = upload_data_to_KG(year, query_endpoint, update_endpoint)
    print(f"Number of instantiated Electricity consumption data per LOSA output area :{len_query}")
    t2= time.time()
    diff = t2 - t1
    print(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    # upload the timeseries data
    print("\nUploading the Electricity consumption time series data:")
    logger.info("Uploading the Electricity consumption time series data...")
    t1 = time.time()
    added_ts = upload_timeseries_to_KG(datairi, value, year, query_endpoint, update_endpoint)
    t2= time.time()
    diff = t2 - t1
    print(f'Electricity consumption timeseries data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Electricity consumption timeseries data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    return len_query, added_ts

if __name__ == '__main__':
# years = ['2018','2017','2016','2015']
# for year in years:
#         upload_year(year)
    len_query, added_ts = upload_all()
    print(f'Number of instantiated LSOA area:{len_query}')
    print(f'Number of updated time series readings (i.e. dataIRIs):{added_ts}')