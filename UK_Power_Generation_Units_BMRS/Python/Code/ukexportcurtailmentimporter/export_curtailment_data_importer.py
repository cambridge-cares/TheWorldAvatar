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
import py4j.protocol as pyprotocol

#New Supplementary Components
import ExcelToTimeseries
import TimeStampFunctions

# get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module
from jpsSingletons import jpsBaseLibView
# get settings and functions from kg_utils_generation module
import kg_utils_generation as kg
# get the BMRS API Dataframe generating and CSV writing script. 
#from ScriptMapQuery import BMRS_API_Input_JA_7 as bmrs

units = "http://www.ontology-of-units-of-measure.org/resource/om-2/megawatt"
powerplant_class_prefix = 'PowerPlant_'
powerGenerator_class_prefix = 'PowerGenerator_'

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


def instantiate_powerplant_timeseries(query_endpoint, update_endpoint, powerplantIRI, powerplant_name=''):
    """
        Instantiates all relevant triples for time series storage in KG and initialises RDB tables for generator.
        (raises exception if time series association is already initialised)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            generatorIRI - full gas generator IRI incl. namespace (without trailing '<' or '>').
            generator_name - gas generator name (optional).
    """
    if powerplant_name != '':
        print("Instantiate time series association for: " + powerplant_name)
    else:
        print("Instantiate time series association.")

    # Create UUIDs for IntakenGas, VolumetricpowerRate, and Measure instances
    powerplant = "PowerPlant" + "_" + powerplant_name

    # quantity = 'Quantity_' + str(uuid.uuid4())
    measurement = 'Measurement_' + str(uuid.uuid4())
    # Derive MeasurementIRI to which time series is actually connected to
    measurement_iri = kg.PREFIXES['ontoenergysystem_kb'] + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)
    
    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)

    query = kg.create_sparql_prefix('ontopowsys') + \
            kg.create_sparql_prefix('ontoenergysystem') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontoenergysystem:ExportedActivePower ; \
                 om:hasUnit <%s> . }''' % (powerplantIRI, measurement_iri, measurement_iri, units)

    KGClient.executeUpdate(query)
    print("Time series triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries([measurement_iri], [double_class], kg.FORMAT)

    print("Time series triples via Java TimeSeriesClient successfully instantiated.")


def instantiate_powerplant_curtailment_timeseries(query_endpoint, update_endpoint, powerplantIRI, powerplant_name=''):
    """
        Instantiates all relevant triples for time series storage in KG and initialises RDB tables for generator.
        (raises exception if time series association is already initialised)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            generatorIRI - full gas generator IRI incl. namespace (without trailing '<' or '>').
            generator_name - gas generator name (optional).
    """
    if powerplant_name != '':
        print("Instantiate time series association for: " + powerplant_name)
    else:
        print("Instantiate time series association.")

    # Create UUIDs for IntakenGas, VolumetricpowerRate, and Measure instances
    powerplant = "PowerPlant" + "_" + powerplant_name

    # quantity = 'Quantity_' + str(uuid.uuid4())
    measurement = 'Measurement_' + str(uuid.uuid4())
    # Derive MeasurementIRI to which time series is actually connected to
    measurement_iri = kg.PREFIXES['ontoenergysystem_kb'] + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)
    
    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)

    query = kg.create_sparql_prefix('ontopowsys') + \
            kg.create_sparql_prefix('ontoenergysystem') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontoenergysystem:CurtailedActiveEnergy ; \
                 om:hasUnit <%s> . }''' % (powerplantIRI, measurement_iri, measurement_iri, units)

    KGClient.executeUpdate(query)
    print("Time series curtailment triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries([measurement_iri], [double_class], kg.FORMAT)

    print("Time series triples via Java TimeSeriesClient successfully instantiated.")


def instantiate_generator_timeseries(query_endpoint, update_endpoint, generatorIRI, generator_name=''):
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

    # Create UUIDs for the measured power contributed by the generator
    generator = generator_name
    measurement = 'Measurement_' + str(uuid.uuid4())
    # Derive MeasurementIRI to which time series is actually connected to
    measurement_iri = kg.PREFIXES['ontoenergysystem_kb'] + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)
    
    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)

    # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('ontopowsys') + \
            kg.create_sparql_prefix('ontoenergysystem') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontopowsys:GeneratedActivePower ; \
                 om:hasUnit <%s> . }''' % (generatorIRI, measurement_iri, measurement_iri, units)

    KGClient.executeUpdate(query)
    print("Time series triples independent of Java TimeSeriesClient successfully instantiated.")
    print('measurement_iri 1:', measurement_iri)
    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE
    print('measurement_iri 2:', measurement_iri)
    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    print('measurement_iri 3:', measurement_iri)
    try:
        TSClient.initTimeSeries([measurement_iri], [double_class], kg.FORMAT)
        print("Time series has been  successfully initialised.")
    except pyprotocol.Py4JJavaError as err:
        print("Could not initialise a time-series for the following generator:"+generator_name+" that has\n' \
        'the following measurement IRI:" + generatorIRI)
        print('The error message is:', str(err.java_exception))
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
            <%s> rdf:type ontopowsys:GeneratedActivePower . \
            <%s> om:hasUnit <%s> . }''' % (instance_IRI, activepowergenerated_IRI, activepowergenerated_IRI, activepowergenerated_IRI, units)
    ###

    KGClient.executeUpdate(query)
    print("Triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Add actual time series data
    # Create Java TimeSeries object with data to attach
    times = timestamps
    variables = dataIRIs #Could just be a single variable in the current application. 
    
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

    # 3) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries(dataIRIs, [double_class]*len(dataIRIs), kg.FORMAT)

    #print("Time series triples via Java TimeSeriesClient successfully instantiated.")
    timeseries = jpsBaseLibView.TimeSeries(times, variables, values)

    # Add data
    TSClient.addTimeSeriesData(timeseries)
    #print("Time series data successfully added.\n")
    # #Note that this would run for each upload, so if you are uploading
    # multiple times it is reccomended to have the comment in location
    # after this function is called (however many times).

def get_power_data_from_api():
    """
        Gathers instantaneous power rate data for each generator from national grid website.

        Returns:
            DataFrame with gas power rate data and columns 'generator', 'time', 'power'.
            (all generator names are capitalised for naming consistency reasons)
    """
    print("Querying API")

    #Local Key
    with open("LocalOnlyBMRSKey.txt", "r") as keyFile:
        Key = keyFile.readline()
    #Or just paste it below directly#
    #Key = '' #####NEED THIS#####
    AutoFile = 'Input-Template-Auto.csv'
    powerplant_df, generator_df = bmrs.Auto_Call(Key, AutoFile)
    #Read the Input-Template.csv file from a URL. 
    #Simplified Data Link
    # powerplant_df, generator_df = bmrs.convert_csv_to_triple_dfs('https://www.dropbox.com/s/o6b0m1qozb356u6/Input-Template%20-%20Simple.csv?dl=1')
    #Standardised Day Link
    #powerplant_df, generator_df = bmrs.convert_csv_to_triple_dfs('https://www.dropbox.com/s/qi3no1kbwr4idus/Input-Template%20-%20All.csv?dl=1')
    
    #Note, will want to call the overall funtion, rather than convert_csv_to_triple_dfs longer term. 
    print("Finished preparing dataframes.")
    return powerplant_df, generator_df


def add_time_series_data(assetIRI, power_data, asset_name='', plantEIC='', curtailment_bool=False):
    """
        Adds given gas power data (DataFrame) to time series of respective powerplant/generator (asset).

        Arguments:
            assetIRI - IRI of asset to which power data shall be added.
            power_data - power data to add to time series (as DataFrame with columns ['time', 'power']).
            asset_name - asset name (optional).
    """
    print("Adding time series data for: " + plantEIC)

    # Extract data to create Java TimeSeries object
    if curtailment_bool:
         measurementIRI = kg.get_curtailment_measurementIRI(kg.QUERY_ENDPOINT, assetIRI)
    else:
        measurementIRI = kg.get_measurementIRI(kg.QUERY_ENDPOINT, assetIRI)
    variables = measurementIRI
    if variables is None:
        print('Time series could not be created for the following asset:', assetIRI)
        return
    times, values = ExcelToTimeseries.excelToKG(power_data, plantEIC) #excelToKG takes a file containing export, curtailment (or any other data of the same format), and returns timestamps and values in lists. 
    #Reformat times. 
    #times = times.values.reshape(-1,).tolist()
    #New (not required)

    #Reformat variables (or just a single variable), could already be in this form. 
    if type(variables) == str:
        a = measurementIRI #a is just a temporary variable for this. 
        variables = []
        variables.append(a)
    
    #Reformat values. 
    #values = powers.values.reshape(-1,).tolist() #tolist is noted in some older posts online to convert to float, but it does not seem to here, so conversion from float64 (numpy) to float. 
    #New (not required)
    
    a = [] #a is just a temporary variable for this. 
    for value in values:
        a.append(float(value))
    values = []
    values.append(a)
    # Create Java TimeSeries object
    timeseries = jpsBaseLibView.TimeSeries(times, variables, values)

    # Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java class for time entries (Instant) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()

    # Add time series data to existing time series association in KG using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    try:
        TSClient.addTimeSeriesData(timeseries)
    except pyprotocol.Py4JJavaError as err:
        print("Could not add a time-series to the following asset:"+asset_name+" that has\n' \
        'the following measurement IRI:"+ assetIRI)
        print('The error message is:', str(err.java_exception))
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


def EIC_check(IRI, EIC_list, name_list):
    """
        For each powerplant, add an EIC if one exists and is not added already. 
        The IRI should be the name. 
        The EIC_list and name_list should be of equal length, with corresponding positions (i.e. 0, 1, 2,...,n) refering to the same powerplant. 

        EIC_list example: plants_with_data_full_EIC
        name_list example: plants_with_data_full_name
    """
    url = IRI 

    #If the IRI is not the name, but instead is a URL, then maybe add a split here for it. This is optional based on what the input ends up being. 
    IRI = IRI.replace('\\', ' ')
    IRI = IRI.replace('/', ' ')
    IRI = IRI.split()
    if type(IRI) == list:
        IRI = IRI[-1]
        IRI = IRI.replace("PowerPlant_", "")
    
    #Is there already an EIC, if so, then return. 
    ###ADD QUERY HERE, FOLLOWED BY IF WHICH RETURNS IF ONE EXISTS###

    #If there is not an EIC, does one exist? 
    if IRI not in name_list:
        return
    
    #If there is, then add the EIC from the list. 
    else:
        EIC = EIC_list[name_list.index(IRI)]
        ###THEN ADD THE EIC###


def setup_df_to_list(ColName, ExcelName):
    #Reads a column from an excel sheet and makes it a list.
    bl = pd.read_excel(ExcelName)

    out_list = []

    for i in bl[ColName]:
        if not str(i) == "nan":
            out_list.append(i)
        #print(i)

    return out_list


def update_triple_store():
    """
        Main function to assimilate power time series data into KG, incl. instantiation
        of relevant relationships and new generators in case new data becomes available
    """

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Get the power data from National Grid csv as DataFrame
    #powerplant_power_data, generator_power_data = get_power_data_from_api()
    #New (just get the files)
    export_path = "data/Template-Powerplant-Export.xlsx"
    curtailment_path = "data/Template-Powerplant-Curtailment.xlsx"
    ##########################################
    #Note: Read excel file here once for all and use the variable every time you call the add time series function
    ##########################################
    export_data = pd.read_excel(export_path)
    curtailment_data = pd.read_excel(curtailment_path)

    base_lists = "data/BaseLists.xlsx"
    
    #Now do the same for powerplants as will be done for generators. 
    # Retrieve all powerplants with available power data (powerplant names are capitalised)
    #powerplants_with_data = powerplant_power_data['powerplanteic'].unique()
    #New (EICs)
    #plants_with_data_full_EIC = ["48WSTN0000ABRBON", "48WSTN0000ABRTWR", "48WSTN0000ACHRWV", "48WSTN0000ANSUWY", "48WSTN0000ARCHW6", "48WSTN0000ASHWWA", "48WSTN1000BABAWQ", "48WSTN0000BEATOG", "48WSTN0000BEINWN", "48WSTN0000BETHWY", "48WSTN0000BHLAWZ", "48WSTN0000BLKWWR", "48WSTN00000BLLAV", "48WSTN00000BLLXM", "48WSTN0000BNAKWJ", "48WSTN0000BNWKW5", "48WSTN0000BOWLWY", "48WSTN0000BRBEOT", "48WSTN0000BRDUWV", "48WSTN0000BRYBW4", "48WSTN0000BTUIWQ", "48WSTN0000BURBWH", "48WSTN0000CAUSWB", "48WSTN0000CGTHWI", "48WSTN0000CLDCWZ", "48WSTN0000CLDNW2", "48WSTN0000CLDRWR", "48WSTN0000CLDSWO", "48WSTN0000COUWW3", "48WSTN0000CRMLWG", "48WSTN0000CRYRBT", "48WSTN0000CRYRWO", "48WSTN0000DALSW4", "48WSTN0000DDGNO3", "48WSTN0000DEUCWX", "PLACEHOLDER1", "48WSTN0000DRDGWO", "48WSTN0000DRSLWN", "48WSTN0000DUNGW6", "48WSTN00000EAAOS", "48WSTN0000EARBWP", "48WSTN0000EDINWA", "48WSTN1000EWHLWQ", "48WSTN0000FAARW2", "48WSTN0000FALGWS", "48WSTN1000FDUNTQ", "48WSTN0000FSDLWT", "48WSTN0000GAOFOT", "48WSTN0000GDSTWE", "48WSTN0000GFLDW6", "48WSTN0000GLOFWW", "48WSTN0000GLWSWZ", "48WSTN0000GNFSWJ", "48WSTN0000GRGBW9", "48WSTN0000GRIFWQ", "PLACEHOLDER2", "48WSTN0000HADHW8", "48WSTN0000HLTWWT", "48WSTN0000HMGTOR", "48WSTN0000HOWAOA", "48WSTN0000HRSTWC", "48WSTN0000HYWDW9", "48WSTN0000KILBWA", "48WSTN0000KLGLWM", "48WSTN0000LARYWP", "48WSTN0000LCLTWH", "48WSTN0000MDHLW9", "48WSTN0000MILWW9", "48WSTN0000MINSWD", "48WSTN0000MKHLWB", "48WSTN0000MOWEO5", "48WSTN0000NHOYWR", "48WSTN1000NOVAWQ", "48WSTN0000OMNDWQ", "48WSTN0000PAUHW3", "48WSTN0000RCBKOV", "48WSTN0000RHYFWK", "48WSTN0000RMPNON", "48WSTN00000RREWF", "48WSTN00000RRWWZ", "48WSTN0000RSHLWF", "48WSTN0000SHRSW3", "48WSTN0000STLGW3", "48WSTN0000STRNWW", "48WSTN0000TDBNWM", "48WSTN0000THNTWA", "48WSTN0000TULWBN", "48WSTN0000TULWWI", "48WSTN0000WDNSWF", "48WSTN1000WHILWQ", "48WSTN0000WLNYWV", "48WSTN0000WLNY3F", "48WSTN0000WLNY4D", "48WSTN0000WTMSOT"]
    #plants_with_data_full_name = ["Aberdeen", "Auchrobert", "AChruach", "An_Suidhe", "Arecleoch", "Andershaw", "Baillie", "Beatrice", "Beinneun", "Beinn_Tharsuinn", "Bhlaraidh", "Blackcraig", "Black_Law", "Black_Law_II", "Ben_Aketil", "Burn_of_Whilk", "Barrow", "Burbo_Extension", "Braes_of_Doune", "Berry_Burn", "Beinn_an_Tuirc_II", "Burbo", "Causeymire", "Corriegarth", "Clyde_(Central)", "Clyde_(North)", "Clashindarroch", "Clyde_(South)", "Cour", "Corriemoillie", "Crystal_Rig_II", "Crystal_Rig_I", "Dalswinton", "Dudgeon_1", "Deucheran_Hill", "Dorenell", "Drumderg", "Dersalloch", "Dunmaglass", "East_Anglia_One", "Earlsburn", "Edinbane", "Ewe_Hill_II", "Farr", "Fallago_Rig", "Lynn", "Freasdail", "Galloper", "Gordonstown_Hill", "Goole_Fields_A", "Glens_of_Foudland", "Galawhistle", "Gunfleet_Sands_1_2", "Greater_Gabbard", "Griffin", "Gwynt_y_Mor", "Hadyard_Hill", "Hill_of_Towie", "Humber_Gateway", "Hornsea_1", "Harestanes", "Hywind_1", "Kilbraur", "Kilgallioch", "London_Array", "Lochluichart", "Mid_Hill", "Millennium", "Minsca", "Mark_Hill", "Moray_East", "North_Hoyle", "Novar", "Ormonde", "Pauls_Hill", "Race_Bank", "Rhyl_Flats", "Rampion", "Robin_Rigg_East", "Robin_Rigg_West", "Rosehall", "Sheringham_Shoal_1", "Stronelairg", "Strathy_North", "Toddleburn", "Thanet", "Twinshiels", "Tullo", "West_of_Duddon_Sands", "Whitelee", "Walney_1_2", "Walney_3", "Walney_4", "Westermost_Rough"]
    #plants_with_data_checked_EIC = ["48WSTN0000ABRBON", "48WSTN0000ARCHW6", "48WSTN1000BABAWQ", "48WSTN0000BEATOG", "48WSTN0000BEINWN", "48WSTN0000BHLAWZ", "48WSTN0000BLKWWR", "48WSTN00000BLLAV", "48WSTN00000BLLXM", "48WSTN0000BOWLWY", "48WSTN0000BRBEOT", "48WSTN0000BRDUWV", "48WSTN0000BRYBW4", "48WSTN0000CGTHWI", "48WSTN0000CLDCWZ", "48WSTN0000CLDNW2", "48WSTN0000CLDSWO", "48WSTN0000CRYRBT", "48WSTN0000DDGNO3", "48WSTN0000DRSLWN", "48WSTN0000DUNGW6", "48WSTN00000EAAOS", "48WSTN0000FALGWS", "48WSTN0000GLWSWZ", "48WSTN0000GNFSWJ", "48WSTN0000GRGBW9", "48WSTN0000GRIFWQ", "48WSTN0000HADHW8", "48WSTN0000HMGTOR", "48WSTN0000HOWAOA", "48WSTN0000HRSTWC", "48WSTN0000KILBWA", "48WSTN0000KLGLWM", "48WSTN0000LCLTWH", "48WSTN0000MILWW9", "48WSTN0000MKHLWB", "48WSTN0000RCBKOV", "48WSTN0000RMPNON", "48WSTN00000RREWF", "48WSTN00000RRWWZ", "48WSTN0000STLGW3", "48WSTN0000STRNWW", "48WSTN1000WHILWQ", "48WSTN0000WLNYWV", "48WSTN0000WLNY3F", "48WSTN0000WLNY4D", "48WSTN0000WTMSOT"]
    #plants_with_data_checked_name = ["Aberdeen", "Arecleoch", "Baillie", "Beatrice", "Beinneun", "Bhlaraidh", "Blackcraig", "Black_Law", "Black_Law_II", "Barrow", "Burbo_Extension", "Braes_of_Doune", "Berry_Burn", "Corriegarth", "Clyde_(Central)", "Clyde_(North)", "Clyde_(South)", "Crystal_Rig_II", "Dudgeon_1", "Dersalloch", "Dunmaglass", "East_Anglia_One", "Fallago_Rig", "Galawhistle", "Gunfleet_Sands_1_2", "Greater_Gabbard", "Griffin", "Hadyard_Hill", "Humber_Gateway", "Hornsea_1", "Harestanes", "Kilbraur", "Kilgallioch", "Lochluichart", "Millennium", "Mark_Hill", "Race_Bank", "Rampion", "Robin_Rigg_East", "Robin_Rigg_West", "Stronelairg", "Strathy_North", "Whitelee", "Walney_1_2", "Walney_3", "Walney_4", "Westermost_Rough"]

    plants_with_data_full_EIC = setup_df_to_list("plants_with_data_full_EIC", base_lists)
    plants_with_data_full_name = setup_df_to_list("plants_with_data_full_name", base_lists)
    plants_with_data_checked_EIC = setup_df_to_list("plants_with_data_checked_EIC", base_lists)
    plants_with_data_checked_name = setup_df_to_list("plants_with_data_checked_name", base_lists)
    
    # Retrieve all instantiated powerplants in KG
    powerplants = kg.get_instantiated_powerplants(kg.QUERY_ENDPOINT)
    powerplants_instantiated = {powerplant_class_prefix + k: v for k, v in powerplants.items()}

    # Potentially create new powerplant instances for powerplants with available power data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    #new_powerplants = False
    #for gt in powerplants_with_data:
    #    if (gt not in powerplants_instantiated.keys()) and (gt != ""):
    #        instantiate_powerplant(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt)
    #        new_powerplants = True
    #New (update)
    new_powerplants = False
    for gt in plants_with_data_full_EIC:
        if (gt not in powerplants_instantiated.keys()) and (gt != ""):
            instantiate_powerplant(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt)
            new_powerplants = True
    
    # Retrieve update of instantiated powerplants in KG (in case any new powerplants were added)
    if new_powerplants:
        powerplants = kg.get_instantiated_powerplants(kg.QUERY_ENDPOINT)
        powerplants_instantiated = {powerplant_class_prefix + k: v for k, v in powerplants.items()}
    # Assimilate power data for instantiated  powerplants

    for pp in powerplants_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if kg.get_measurementIRI(kg.QUERY_ENDPOINT, powerplants_instantiated[pp]) is None:
            print("No instantiated timeseries detected for: ", pp)
            instantiate_powerplant_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, powerplants_instantiated[pp], pp)
        else:
            print("Instantiated time series detected!")
        if kg.get_curtailment_measurementIRI(kg.QUERY_ENDPOINT, powerplants_instantiated[pp]) is None:
            print("No instantiated curtailment timeseries detected for: ", pp)
            instantiate_powerplant_curtailment_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, powerplants_instantiated[pp], pp)
        else:
            print("Instantiated curtailment time series detected!")
        # Retrieve power time series data for respective powerplant from overall DataFrame
        #new_data = powerplant_power_data[powerplant_power_data['powerplanteic'] == pp][['time', 'power']]
        #New (not required)

        # Add time series data using Java TimeSeriesClient
        #add_time_series_data(powerplants_instantiated[pp], new_data, pp)
        #New (file name), also using the EIC as pp. 
        if "/" in powerplants_instantiated[pp]:
            tokens = powerplants_instantiated[pp].split("/")
            plantName = tokens[len(tokens)-1]
            print("powerplants_instantiated[pp]:", powerplants_instantiated[pp])
            print("plantName:", plantName)
            if plantName in plants_with_data_checked_name:
                print("plants_with_data_checked_name.index(plantName):", plants_with_data_checked_name.index(plantName))
                plant_index = plants_with_data_checked_name.index(plantName)
                print("plant_index:", plant_index)
                plantEIC = plants_with_data_full_EIC[plant_index]
                print("plantEIC:", plantEIC)
                add_time_series_data(powerplants_instantiated[pp], export_data, pp, plantEIC, False)
                add_time_series_data(powerplants_instantiated[pp], curtailment_data, pp, plantEIC, True)
                ###
                #REPEAT ABOVE LINE, BUT FOR CURTAILMENT (redoing functions / queries / have a setting as required). 
                #So something like the below line, but where the called code puts the data into curtailment rather than exports: 
                #add_time_series_data(powerplants_instantiated[pp], curtailment_data, pp, plantEIC)
                ###
            else:
                print("The following plant is not in the list being processed:", plantName)
    # # Retrieve all generators with available power data (generator names are capitalised)
    # generators_with_data = generator_power_data['generatoreic'].unique()
    # # Retrieve all instantiated generators in KG
    # generators = kg.get_instantiated_generators(kg.QUERY_ENDPOINT)
    # generators_instantiated = {powerGenerator_class_prefix + k: v for k, v in generators.items()}
    # # Potentially create new generator instances for generators with available power data,
    # # which are not yet instantiated in KG (only create instance to enable data assimilation)
    # new_generators = False
    # for gt in generators_with_data:
    #     if (gt not in generators_instantiated.keys()) and (gt != ""):
    #         instantiate_generator(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt)
    #         new_generators = True

    # # Retrieve update of instantiated generators in KG (in case any new generators were added)
    # if new_generators:
    #     generators = kg.get_instantiated_generators(kg.QUERY_ENDPOINT)
    #     generators_instantiated = {powerGenerator_class_prefix + k: v for k, v in generators.items()}

    # # Assimilate power data for instantiated generators
    # for gt in generators_instantiated:
    #     # Potentially instantiate time series association (if not already instantiated)
    #     if (kg.get_measurementIRI(kg.QUERY_ENDPOINT, generators_instantiated[gt]) is None) and (gt != ""):
    #         print("No instantiated timeseries detected for: ", gt)
    #         print('gt:', gt)
    #         print('generators_instantiated[gt]:', generators_instantiated[gt])
    #         instantiate_generator_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, generators_instantiated[gt], gt)
    #         # Retrieve power time series data for respective generator from overall DataFrame
    #         new_data = generator_power_data[generator_power_data['generatoreic'] == gt][['time', 'power']]
    #         # Add time series data using Java TimeSeriesClient
    #         add_time_series_data(generators_instantiated[gt], new_data, gt)
    #     else:
    #         print("Instantiated time series detected!")
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
