
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

def parse_to_file(query):
    #ONLY for testing purpose
  f = open("demofile3.txt", "w")
  f.write(query)
  f.close()

  #open and read the file after the appending:
  f = open("demofile3.txt", "r")

def read_from_excel(year:str):
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

    # Replace the 'null' data to zero
    for i in LSOA_codes, met_num, consump:
        i[np.where(i=='')] = 0

    logger.info('Create triples to instantiate Electrical Consumption data ...')

    return LSOA_codes, met_num, consump

def upload_data_to_KG(year:str,
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
    return len(len_query) - 1

def retrieve_data_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
              's'  'usage'  'meter'  'usageiri'  'meteriri'
        0
        1
        2
        ...

        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
    # Get query string
    query = output_query_template('Electricity',iris= True)

    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s','usage','meter','usageiri','meteriri'])
    df = df.append(result)

    return df

def upload_timeseries_to_KG(year:str, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):

    logger.info('Retrieving available electricity consumption/meters per LSOA from Knowledge Graph ...')
    df = retrieve_data_from_KG(query_endpoint, update_endpoint)
    logger.info('Available electricity consumption/meters per LSOA successfully retrieved.')

    # Initialise lists for TimeSeriesClient's bulkInit function
    dataIRIs = []
    times = []
    values = []
    time = year + "-01-01T12:00:00"

    for lsoa in list(df['s'].unique()):
        # Extract relevant datairi to consumption      
        dataIRI = df[df['s'] == lsoa]['usageiri'].to_string(index=False)
        value = df[df['s'] == lsoa]['usage'].values[0]
        dataIRIs.append(dataIRI)
        times.append(time)
        values.append(value)
    
    for lsoa in list(df['s'].unique()):
        # Extract relevant datairi to meters      
        dataIRI = df[df['s'] == lsoa]['meteriri'].to_string(index=False)
        value = df[df['s'] == lsoa]['meter'].values[0]
        dataIRIs.append(dataIRI)
        times.append(time)
        values.append(value)
    
    # Initialise TimeSeries Clients
    kg_client = KGClient(QUERY_ENDPOINT,UPDATE_ENDPOINT)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)
    with ts_client.connect() as conn:
            ts_client.tsclient.bulkInitTimeSeries(dataIRIs, [DATACLASS]*len(dataIRIs), TIME_FORMAT, conn)
    logger.info('Time series triples for electricity consumption/meters per LSOA via Java TimeSeriesClient successfully instantiated.')

    # Upload TimeSeries data
    added_ts = 0
    ts_list = []
    for i in range(len(times)):
        added_ts += 1
        ts = TSClient.create_timeseries(times[i],dataIRIs[i],values[i])
        ts_list.appened(ts)

    with ts_client.connect() as conn:
        ts_client.tsclient.bulkaddTimeSeriesData(ts_list, conn)
    logger.info(f'Time series data for electricity consumption for {added_ts} LSOA area successfully added.')

    return added_ts

if __name__ == '__main__':
# years = ['2018','2017','2016','2015']
# for year in years:
#         upload_year(year)
 '''
 print("\nUploading the Electricity consumption data")
 logger.info("Uploading the Electricity consumption data...")

 t1= time.time()
 len_query=upload_data_to_KG("2020",QUERY_ENDPOINT,UPDATE_ENDPOINT)
 print(f"Number of instantiated Electricity consumption data per LOSA output area :{len_query}")
 t2= time.time()
 diff = t2 - t1
 print(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
 logger.info(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
'''

upload_timeseries_to_KG("2020", QUERY_ENDPOINT,UPDATE_ENDPOINT)
