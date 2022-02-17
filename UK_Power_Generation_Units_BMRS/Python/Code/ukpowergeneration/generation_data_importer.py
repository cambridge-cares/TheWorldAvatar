"""
    Script to (periodically) gather instantaneous gas power data for UK's gas supply generators (from National Grid)

    Local deployment requires:
        - Blazegraph running in local Tomcat server with port 9999 exposed
        - Triple store endpoint to use for data assimilation (namespace in Blazegraph) needs to be created beforehand
          and match namespace provided in timeseries.properties file in resource folder
        - PostgreSQL database set up locally (with URL and credentials provided in timeseries.properties file)

    Authors: trs53<@>cam.ac.uk, mh807<@>cam.ac.uk
"""

from numpy import power
from tqdm import tqdm
import time
import pytz
import os
import datetime
import uuid
import sys
import traceback
#import wget
import csv
import pandas as pd

# get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module
from jpsSingletons import jpsBaseLibView
# get settings and functions from kg_utils_generation module
import kg_utils_generation as kg
# get the BMRS API Dataframe generating and CSV writing script. 
from ScriptMapQuery import BMRS_API_Input_JA_7 as bmrs


def instantiate_generator(query_endpoint, update_endpoint, generator_name):
    """
        Instantiates new generator in knowledge graph to enable power data assimilation.
        (solely creates new generator instance with respective name, but no further relationships)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            generator_name - name of generator to be instantiated.
    """
    print("Instantiate new generator: " + generator_name)

    # Create unique IRI for new generator based on generator name
    generatorIRI = kg.PREFIXES['ontoenergysystem_kb'] + generator_name.replace(' ', '')
    n = 1
    # Add number suffix in case pure name based IRI already exists
    while generatorIRI in kg.get_instantiated_generators(query_endpoint).values():
        generatorIRI = kg.PREFIXES['ontoenergysystem_kb'] + generator_name.replace(' ', '') + str(n)
        n += 1

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)
    
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    if generatorIRI != "" and generator_name != "":
        query = kg.create_sparql_prefix('ontoenergysystem') + \
                kg.create_sparql_prefix('rdf') + \
                kg.create_sparql_prefix('rdfs') + \
                kg.create_sparql_prefix('xsd') + \
                '''INSERT DATA { <%s> rdf:type ontoenergysystem:PowerGenerator . \
                                <%s> rdfs:label "%s"^^xsd:string . }''' % \
                (generatorIRI, generatorIRI, generator_name.replace("PowerGenerator_",""))
        KGClient.executeUpdate(query)


def instantiate_powerplant(query_endpoint, update_endpoint, powerplant_name):
    """
        Instantiates new powerplant in knowledge graph to enable power data assimilation.
        (solely creates new powerplant instance with respective name, but no further relationships)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            powerplant_name - name of powerplant to be instantiated.
    """
    print("Instantiate new powerplant: " + powerplant_name)

    # Create unique IRI for new powerplant based on powerplant name
    powerplantIRI = kg.PREFIXES['ontoenergysystem_kb'] + powerplant_name.replace(' ', '')
    n = 1
    # Add number suffix in case pure name based IRI already exists
    while powerplantIRI in kg.get_instantiated_powerplants(query_endpoint).values():
        powerplantIRI = kg.PREFIXES['ontoenergysystem_kb'] + powerplant_name.replace(' ', '') + str(n)
        n += 1

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)

    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    if powerplantIRI != "" and powerplant_name != "":
        query = kg.create_sparql_prefix('ontoenergysystem') + \
                kg.create_sparql_prefix('rdf') + \
                kg.create_sparql_prefix('rdfs') + \
                kg.create_sparql_prefix('xsd') + \
                kg.create_sparql_prefix('ontoeip') + \
                '''INSERT DATA { <%s> rdf:type ontoenergysystem:PowerPlant . \
                                <%s> rdfs:label "%s"^^xsd:string . }''' % \
                (powerplantIRI, powerplantIRI, powerplant_name.replace("PowerPlant_",""))
        KGClient.executeUpdate(query)


def instantiate_timeseries(query_endpoint, update_endpoint, generatorIRI, generator_name=''):
    """
        Instantiates all relevant triples for time series storage in KG and initialises RDB tables for generator.
        (raises exception if time series association is already initialised)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            generatorIRI - full gas generator IRI incl. namespace (without trailing '<' or '>').
            generator_name - gas generator name (optional).
    """
    if generator_name != '':
        print("Instantiate time series association for: " + generator_name)
    else:
        print("Instantiate time series association.")

    # Create UUIDs for IntakenGas, VolumetricpowerRate, and Measure instances
    gas = 'GasAmount_' + str(uuid.uuid4())
    quantity = 'Quantity_' + str(uuid.uuid4())
    measurement = 'Measurement_' + str(uuid.uuid4())

    # Ensure that newly created IRIs are not already present in knowledge graph --> if so, re-create
    # !! ASSUMED TO BE UNNECESSARY DUE TO random UUID --> currently left out due to long query times !!
    #
    # gases_existing = kg.get_instantiated_gas_amounts(query_endpoint)
    # while any(existing.endswith(gas) for existing in gases_existing):
    #     gas = 'GasAmount_' + str(uuid.uuid4())
    #
    # quantities_existing = kg.get_instantiated_quantities(query_endpoint)
    # while any(existing.endswith(quantity) for existing in quantities_existing):
    #     quantity = 'Quantity_' + str(uuid.uuid4())
    #
    # measurements_existing = kg.get_instantiated_measurements(query_endpoint)
    # while any(existing.endswith(measurement) for existing in measurements_existing):
    #     measurement = 'Measurement_' + str(uuid.uuid4())
    
    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)

    # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('ontoenergysystem') + \
            kg.create_sparql_prefix('ontoenergysystem_kb') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> ontoenergysystem:hasTaken ontoenergysystem_kb:%s . \
            ontoenergysystem_kb:%s rdf:type ontoenergysystem:IntakenGas . \
            ontoenergysystem_kb:%s rdf:type om:VolumetricpowerRate; \
                     om:hasPhenomenon ontoenergysystem_kb:%s; \
                     om:hasValue ontoenergysystem_kb:%s . \
            ontoenergysystem_kb:%s rdf:type om:Measure; \
                     om:hasUnit om:cubicMetrePerSecond-Time. }''' % (
                generatorIRI, gas, gas, quantity, gas, measurement, measurement)

    KGClient.executeUpdate(query)
    print("Time series triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Derive MeasurementIRI to which time series is actually connected to
    measurement_iri = kg.PREFIXES['ontoenergysystem_kb'] + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)

    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries([measurement_iri], [double_class], kg.FORMAT)

    print("Time series triples via Java TimeSeriesClient successfully instantiated.")


def add_time_series(instance_IRI, timestamps, values, units): 
    """
        Directly adds time series information. 

        For a single generator/powerplant, for a day (so expecting 48 time periods). 
    """
    ###
    # 1) Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT)

    # Retrieve Java classes for time entries (Instant) and data (ALL Double)
    # (required for time series client instantiation)
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE
    ###

    dataIRIs = []
    activepowergenerated_IRI = kg.PREFIXES['ontoenergysystem_kb'] + 'ActivePowerGenerated_' + str(uuid.uuid4())
    dataIRIs.append(activepowergenerated_IRI)

    #Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    ###
    query = kg.create_sparql_prefix('ontopowsys') + \
            kg.create_sparql_prefix('ontoenergysystem') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontopowsys:ActivePowerGenerated . \
            <%s> om:hasUnit <%s> . }''' % (instance_IRI, activepowergenerated_IRI, activepowergenerated_IRI, activepowergenerated_IRI, units)
    ###

    #print('insert query:', query)

    KGClient.executeUpdate(query)
    #print("Triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Add actual time series data
    # Create Java TimeSeries object with data to attach
    times = timestamps
    variables = dataIRIs #Could just be a single variable in the current application. 
    
    #######
    #Currently have the information in a different format (eg. dataframes, string), need to have it in the below format. 
    #Array(list) of Timestamps
    #times = ['2021-12-09T00:00:00Z', '2021-12-09T00:30:00Z', '2021-12-09T01:00:00Z', '2021-12-09T01:30:00Z', '2021-12-09T02:00:00Z', '2021-12-09T02:30:00Z', '2021-12-09T03:00:00Z', '2021-12-09T03:30:00Z', '2021-12-09T04:00:00Z', '2021-12-09T04:30:00Z', '2021-12-09T05:00:00Z', '2021-12-09T05:30:00Z', '2021-12-09T06:00:00Z', '2021-12-09T06:30:00Z', '2021-12-09T07:00:00Z', '2021-12-09T07:30:00Z', '2021-12-09T08:00:00Z', '2021-12-09T08:30:00Z', '2021-12-09T09:00:00Z', '2021-12-09T09:30:00Z', '2021-12-09T10:00:00Z', '2021-12-09T10:30:00Z', '2021-12-09T11:00:00Z', '2021-12-09T11:30:00Z', '2021-12-09T12:00:00Z', '2021-12-09T12:30:00Z', '2021-12-09T13:00:00Z', '2021-12-09T13:30:00Z', '2021-12-09T14:00:00Z', '2021-12-09T14:30:00Z', '2021-12-09T15:00:00Z', '2021-12-09T15:30:00Z', '2021-12-09T16:00:00Z', '2021-12-09T16:30:00Z', '2021-12-09T17:00:00Z', '2021-12-09T17:30:00Z', '2021-12-09T18:00:00Z', '2021-12-09T18:30:00Z', '2021-12-09T19:00:00Z', '2021-12-09T19:30:00Z', '2021-12-09T20:00:00Z', '2021-12-09T20:30:00Z', '2021-12-09T21:00:00Z', '2021-12-09T21:30:00Z', '2021-12-09T22:00:00Z', '2021-12-09T22:30:00Z', '2021-12-09T23:00:00Z', '2021-12-09T23:30:00Z']
    #Single element array(list) with string
    #variables = ['http://www.theworldavatar.com/kb/ontoenergysystem/ActivePowerGenerated_ad7e55af-0c71-4adf-9d7f-39ba9ce18ffd']
    #Array(list) containing a single, other array(list) of power values
    #values = [[848.186, 983.106, 992.36, 994.12, 885.554, 993.444, 976.76, 978.47, 953.792, 870.35, 990.672, 1049.056, 1370.106, 1706.562, 1938.362, 1973.972, 1977.078, 1976.586, 1975.902, 1976.792, 1976.412, 1975.942, 1976.336, 1976.588, 1884.476, 1846.298, 1805.98, 1863.346, 1881.686, 1926.882, 1967.512, 1969.906, 1971.362, 1970.718, 1971.742, 1968.02, 1965.662, 1968.954, 1962.82, 1960.606, 1877.356, 1670.484, 1080.696, 656.936, 179.12, 0, 0, 0]]
    #dataIRIs = variables
    
    #Reformat times. 
    times = times.values.reshape(-1,).tolist()

    #Reformat variables (or just a single variable), could already be in this form. 
    if type(variables) == str:
        a = variables #a is just a temporary variable for this. 
        variables = []
        variables.append(a)
    
    #Reformat values. 
    values = values.values.reshape(-1,).tolist() #tolist is noted in some older posts online to convert to float, but it does not seem to here, so conversion from float64 (numpy) to float. 
    a = [] #a is just a temporary variable for this. 
    for value in values:
        a.append(float(value))
    values = []
    values.append(a)
    """
    print("COMPARE:")
    print("Code Version:")
    print(values)
    values1 = [[848.186, 983.106, 992.36, 994.12, 885.554, 993.444, 976.76, 978.47, 953.792, 870.35, 990.672, 1049.056, 1370.106, 1706.562, 1938.362, 1973.972, 1977.078, 1976.586, 1975.902, 1976.792, 1976.412, 1975.942, 1976.336, 1976.588, 1884.476, 1846.298, 1805.98, 1863.346, 1881.686, 1926.882, 1967.512, 1969.906, 1971.362, 1970.718, 1971.742, 1968.02, 1965.662, 1968.954, 1962.82, 1960.606, 1877.356, 1670.484, 1080.696, 656.936, 179.12, 0, 0, 0]]
    print("Hard Coded Version:")
    print(values1)
    print("Compare Overall:")
    print(values == values1)
    print(values[0] == values1[0])
    print(type(values) == type(values1))
    print(type(values[0]) == type(values1[0]))
    print("Compare Parts:")
    print(type(values[0][0]))
    print(type(values1[0][0]))
    for i in range(0,47):
        print(type(values1[0][i]))
        print(type(values[0][i]))
        print(type(values[0][i]) == type(values1[0][i]))
    """
    #######
    """
    print('---times starts---')
    print(times)
    print('---times ends---')
    print('---variables starts---')
    print(variables)
    print('---variables ends---')
    print('---values starts---')
    print(values)
    print('---values ends---')
    """
    # 3) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), kg.FORMAT)

    #print("Time series triples via Java TimeSeriesClient successfully instantiated.")
    timeseries = jpsBaseLibView.TimeSeries(times, variables, values)

    # Add data
    TSClient.addTimeSeriesData(timeseries)
    #print("Time series data successfully added.\n") #Note that this would run for each upload, so if you are uploading multiple times it is reccomended to have the comment in location after this function is called (however many times). 


def get_power_data_from_api():
    """
        Gathers instantaneous power rate data for each generator from national grid website.

        Returns:
            DataFrame with gas power rate data and columns 'generator', 'time', 'power'.
            (all generator names are capitalised for naming consistency reasons)
    """

    # Get current UK timezone to properly convert reported local times into UTC
    # (i.e. account for daylight saving time)
    #tz = pytz.timezone('Europe/London')
    #####DO THIS LATER#####

    print("Querying API")
    #url = "https://mip-prd-web.azurewebsites.net/InstantaneousViewFileDownload/DownloadFile"
    #filename = wget.download(url)
    #Run the Query Script. If the inputs are valid it will update the CSV. 
    #csvName = 'Input-Template.csv'
    #Key = ''
    #Year = 2021 #This should be a week ago. 
    #Month = 11 #This should be a week ago. 
    #Day = 14 #This should be a week ago. 
    #Period = 24 #Note this doesn't matter if Search is 2, as it goes from 1 - 48 regardless. 
    #Search = 2 #This script have multiple run options, for a day we want '2'. 

    #####BACK LATER#####
    #Local Key#
    with open("LocalOnlyBMRSKey.txt", "r") as keyFile:
        Key = keyFile.readline()
    #Or just paste it below directly#
    #Key = '' #####NEED THIS#####
    AutoFile = 'Input-Template-Auto.csv'
    powerplant_df, generator_df = bmrs.Auto_Call(Key, AutoFile)
    #Read the Input-Template.csv file from a URL. 
    #eg. 'https://www.dropbox.com/s/mmmcto232y4q3or/Input-Template.csv?dl=1' for 'Dropbox (Cambridge CARES)\CoMo shared\ja685\BMRS\Script-BMRS-API/Input-Template.csv'
    #Simplified Data Link
    #powerplant_df, generator_df = bmrs.convert_csv_to_triple_dfs('https://www.dropbox.com/s/o6b0m1qozb356u6/Input-Template%20-%20Simple.csv?dl=1')
    #Standardised Day Link
    #powerplant_df, generator_df = bmrs.convert_csv_to_triple_dfs('https://www.dropbox.com/s/qi3no1kbwr4idus/Input-Template%20-%20All.csv?dl=1')
    
    #Note, will want to call the overall funtion, rather than convert_csv_to_triple_dfs longer term. 

    # print("PowerPlants Dataframe: ")
    # print(powerplant_df)
    # print("Generators Dataframe: ")
    # print(generator_df)
    #####BACK LATER#####
    print("Finished preparing dataframes.")

    # 2D array of data (triples [generatorName, time, power])
    #data = []

    #print("Reading power data CSV...")
    #data = pd.read_csv(csvName) #Dataframe including DUKES stations. 
    #These should both be EIC of (station or generator), time, power.  
    #powerplant_df, generator_df = bmrs.convert_csv_to_triple_dfs(csvName)
    
    '''
    with open(filename, newline='') as csvfile:
        reader = csv.reader(csvfile)

        currentRow = -1
        generatorStartLine = 9999
        generatorEndLine = 9999

        for row in reader:
            currentRow = currentRow + 1

            if "generator Totals" in row[0]:
                generatorStartLine = currentRow
                print("generator data starts on CSV row", currentRow)
            elif "Total System Supply" in row[0]:
                generatorEndLine = currentRow
                print("generator data ends on CSV row", currentRow)

            if (currentRow > generatorStartLine) and (currentRow < generatorEndLine):
                # Parse the CSV rows
                generatorName = row[0]

                # Times from CSV file are in local UK time
                dateTimeObj = datetime.datetime.strptime(row[3], "%d/%m/%Y %H:%M:%S")
                # is_dst=False is used to determine correct timezone in the ambiguous period
                # at the end of daylight saving time
                dateTimeObjUTC = tz.localize(dateTimeObj, False).astimezone(pytz.utc)
                dateTimeStr = dateTimeObjUTC.strftime("%Y-%m-%dT%H:%M:%SZ")

                powerValue = row[2]
                data.append([generatorName, dateTimeStr, powerValue])
    '''

    #print("Finished reading power data CSV, removing file...\n")
    #print("Finished reading power data CSV")
    #os.remove(filename)

    '''
    # Create DataFrame
    df = pd.DataFrame(data, columns=['generator', 'time', 'power'])
    # Convert power from MCM/Day to M^3/S
    df['power'] = (df['power'].astype(float) * 1000000) / (24 * 60 * 60)
    # Capitalise generator names (for consistent comparisons by name)
    df['generator'] = df['generator'].str.upper()
    '''
    
    return powerplant_df, generator_df


def add_time_series_data(assetIRI, power_data, asset_name=''):
    """
        Adds given gas power data (DataFrame) to time series of respective powerplant/generator (asset).

        Arguments:
            assetIRI - IRI of asset to which power data shall be added.
            power_data - power data to add to time series (as DataFrame with columns ['time', 'power']).
            asset_name - asset name (optional).
    """
    print("Adding time series data for: " + asset_name)

    # Extract data to create Java TimeSeries object
    measurementIRI = kg.get_measurementIRI(kg.QUERY_ENDPOINT, assetIRI)
    times = list(power_data['time'].values)
    powers = list(power_data['power'].values)

    # Create Java TimeSeries object
    timeseries = jpsBaseLibView.TimeSeries(times, [measurementIRI], [powers])

    # Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java class for time entries (Instant) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()

    # Add time series data to existing time series association in KG using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.addTimeSeriesData(timeseries)
    print("Time series data added successfully for: " + asset_name + ").\n")


def check_df(df, periods):
    #Checks if there exists data in a dataframe. 
    #Does so at row = 0 and row = (periods - 1)
    #if both contain a (time) value, then returns True, otherwise, returns False. 
    if((df['time'][0] != "") and (df['time'][periods - 1] != "")):
        more = True
    elif((df['time'][0] != "") or (df['time'][periods - 1] != "")):
        print("Error: Dataframe contains an incomplete day as the number of rows is not a multiple of the number of periods in a day. Not including data from here-on out. ")
        more = False
    else:
        more = False
    return more


def take_day(df, starting, periods):
    #Takes a dataframe of multiple instances of multiple instances (eg. multiple generators), and returns a one's dataframe. 
    #df is the underlying dataframe, starting is the row it begins at, and periods is the number of periods in a day (eg. 48 for 30 minute incriments). 
    ending = starting + periods
    dfSlice = df.iloc[starting:ending]
    if((df['time'][ending] != "") and (df['time'][ending + periods - 1] != "")):
        more = True
    elif((df['time'][ending] != "") or (df['time'][ending + periods - 1] != "")):
        print("Error: Dataframe contains an incomplete day as the number of rows is not a multiple of the number of periods in a day. Not including data from here-on out. ")
        more = False
    else:
        more = False
    
    #Returns the dfslice (slice of the original dataframe (eg. a single generator for a day)), ending (the index which could be used to restart this process (eg. if it is 48 instances, and starting was 0, then this would be 49)), and more (are there more generators/powerplants after this which could be extracted if the process was restarted at 'ending'). 
    return dfSlice, ending, more


def update_triple_store():
    """
        Main function to assimilate power time series data into KG, incl. instantiation
        of relevant relationships and new generators in case new data becomes available
    """

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)
    print("Connected to KG SPARQL endpoints:")
    print("Queries: ", kg.QUERY_ENDPOINT)
    print("Updates: ", kg.UPDATE_ENDPOINT)

    # Get the power data from National Grid csv as DataFrame
    #####Note that there are two now#####
    #power_data = get_power_data_from_api()
    #THIS IS THE BIG, IMPORTANT QUERY CALL. 
    powerplant_power_data, generator_power_data = get_power_data_from_api()

    #BMRS uses MW
    units = "http://www.ontology-of-units-of-measure.org/resource/om-2/megawatt"
    
    #Now do the same for powerplants as will be done for generators. 
    # Retrieve all powerplants with available power data (powerplant names are capitalised)
    powerplants_with_data = powerplant_power_data['powerplanteic'].unique()

    # Retrieve all instantiated powerplants in KG
    powerplants = kg.get_instantiated_powerplants(kg.QUERY_ENDPOINT)
    powerplants_instantiated = {k.upper(): v for k, v in powerplants.items()}

    # Potentially create new powerplant instances for powerplants with available power data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    new_powerplants = False
    for gt in powerplants_with_data:
        if gt not in powerplants_instantiated.keys():
            instantiate_powerplant(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt)
            new_powerplants = True

    # Retrieve update of instantiated powerplants in KG (in case any new powerplants were added)
    if new_powerplants:
        powerplants = kg.get_instantiated_powerplants(kg.QUERY_ENDPOINT)
        powerplants_instantiated = {k.upper(): v for k, v in powerplants.items()}

    """
    # Assimilate power data for instantiated gas powerplants
    for gt in powerplants_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if kg.get_measurementIRI(kg.QUERY_ENDPOINT, powerplants_instantiated[gt]) is None:
            print("No instantiated timeseries detected for: ", gt)
            instantiate_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, powerplants_instantiated[gt], gt)
        else:
            print("Instantiated time series detected!")

        # Retrieve power time series data for respective powerplant from overall DataFrame
        new_data = powerplant_power_data[powerplant_power_data['powerplanteic'] == gt][['time', 'power']]

        # Add time series data using Java TimeSeriesClient
        add_time_series_data(powerplants_instantiated[gt], new_data, gt)
    """
    #Loop for powerplants (which we have data)
    daily_loop = check_df(powerplant_power_data, 48)
    placement = 0
    while(daily_loop):
        dfSlice, placement, daily_loop = take_day(powerplant_power_data, placement, 48)
        """
        print("placement")
        print(placement)
        print("daily_loop")
        print(daily_loop)
        print("SLICE")
        print(dfSlice)
        print(kg.PREFIXES['ontoenergysystem_kb'] + dfSlice['powerplanteic'].iloc[0])
        """
        #Add daily slice (dfSlice) here. 
        add_time_series((kg.PREFIXES['ontoenergysystem_kb'] + dfSlice['powerplanteic'].iloc[0]), dfSlice['time'], dfSlice['power'], units)
    #Now do the same for generators. 
    # Retrieve all generators with available power data (generator names are capitalised)
    generators_with_data = generator_power_data['generatoreic'].unique()

    # Retrieve all instantiated generators in KG
    generators = kg.get_instantiated_generators(kg.QUERY_ENDPOINT)
    generators_instantiated = {k.upper(): v for k, v in generators.items()}

    # Potentially create new generator instances for generators with available power data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    new_generators = False
    for gt in generators_with_data:
        if gt not in generators_instantiated.keys():
            instantiate_generator(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt)
            new_generators = True

    # Retrieve update of instantiated generators in KG (in case any new generators were added)
    if new_generators:
        generators = kg.get_instantiated_generators(kg.QUERY_ENDPOINT)
        generators_instantiated = {k.upper(): v for k, v in generators.items()}

    """
    # Assimilate power data for instantiated gas generators
    for gt in generators_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if kg.get_measurementIRI(kg.QUERY_ENDPOINT, generators_instantiated[gt]) is None:
            print("No instantiated timeseries detected for: ", gt)
            instantiate_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, generators_instantiated[gt], gt)
        else:
            print("Instantiated time series detected!")

        # Retrieve power time series data for respective generator from overall DataFrame
        new_data = generator_power_data[generator_power_data['generatoreic'] == gt][['time', 'power']]

        # Add time series data using Java TimeSeriesClient
        add_time_series_data(generators_instantiated[gt], new_data, gt)
    """
    #Loop for generators (for which we have data)
    daily_loop = check_df(generator_power_data, 48)
    placement = 0
    while(daily_loop):
        dfSlice, placement, daily_loop = take_day(generator_power_data, placement, 48)
        """
        print("placement")
        print(placement)
        print("daily_loop")
        print(daily_loop)
        print("SLICE")
        print(dfSlice)
        if placement == 192:
            for w in dfSlice['time']:
                print(w)
            print("MAINTEST")
            print(dfSlice['generatoreic'].iloc[0])
        """
        #Add daily slice (dfSlice) here. 
        add_time_series((kg.PREFIXES['ontoenergysystem_kb'] + dfSlice['generatoreic'].iloc[0]), dfSlice['time'], dfSlice['power'], units)
    
    print("Time series data successfully added.\n")


def continuous_update():
    """
        Ensures that the update_triple_store() function runs once per 12 minutes (as this is the
        interval at which new power data is published).
    """
    emailed = False                                                                      
    while True:
        start = time.time()

        try:
            update_triple_store()
        except Exception:
            print("Encountered exception, will try again in 12 minutes...")
            print(traceback.format_exc())
            
            # Send a notification email
            if not emailed:
                sender = jpsBaseLibView.EmailSender()
                sender.sendEmail(
                    "GasGridAgent - Exception when gathering live power data.",
                     """
                        The 'input_power_data.py' script of the GasGridAgent has encountered an Exception. This script will continue to loop (attempting to gather data every 12 minutes), as the issue may be temporary.
                        \n\n
                        It is recommended that a developer logs into the relevant VM and checks the logs for the gas-grid-agent Docker container.
                    """
                )
                emailed = True

        end = time.time()

        print('\n')
        # Wait for 12 minutes taking into account time to update queries
        for i in tqdm(range(60 * 12 - int((end - start)))):
            time.sleep(1)
    return


def single_update():
    print('Time before (UNIX): {}'.format(int(time.time())))
    update_triple_store()
    print('Time after (UNIX): {}'.format(int(time.time())))
    return


def main():
    
    # Create specified PostgreSQL database
    kg.create_postgres_db()
    # Create specified Blazegraph namespace
    kg.create_blazegraph_namespace()

    """
        Main method, parses arguments.
    """                                                                                          
    # Try to detect (command-line) arguments passed to the script and launch update method
    if len(sys.argv) <= 1:
        single_update()
    elif sys.argv[1] == '-single':
        print('Detected \'-single\' argument, running single update...')
        single_update()
    elif sys.argv[1] == '-continuous':
        print('Detected \'-continuous\' argument, running continuous updates...')
        continuous_update()
    else:
        single_update()


# Entry point, calls main function
if __name__ == '__main__':
    main()
