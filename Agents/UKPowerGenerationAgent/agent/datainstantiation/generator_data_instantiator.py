##############################################################################
# Authors: John Atherton (ja685@cam.ac.uk), Feroz Farazi (msff2@cam.ac.uk)   #
# Date: 27 June 2023                                                         #
##############################################################################


# This module is developed to instantiate the UK power generation
# data in the Knowledge Graph (kG) of the World Avatar project.

from py4jps import agentlogging
from tqdm import tqdm
import time
import uuid
import sys
import traceback
import py4j.protocol as pyprotocol

from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.kgutils.tsclient import add_timeseries
from agent.utils.stack_configs import (DB_PASSWORD, DB_URL, DB_USER,
                                       QUERY_ENDPOINT, UPDATE_ENDPOINT)
from agent.utils.timeseries import (TIME_FORMAT, TIMECLASS, TS_TIMESERIES)
from agent.dataretrieval.generators import (get_instantiated_generators, 
                                            get_instantiated_powerplants, 
                                            get_measurementIRI)
import agent.datamodel.iris as iris
# get the BMRS API Dataframe generating and CSV writing script. 
from agent.scriptmapquery import BMRS_API_Input_JA_7 as bmrs

# Initialises logger
logger = agentlogging.get_logger("prod")

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
    generatorIRI = iris.ONTO_ENERGY_SYSTEM_KB + generator_name.replace(' ', '')
    

    n = 1
    # Add number suffix in case pure name based IRI already exists
    while generatorIRI in get_instantiated_generators(query_endpoint, update_endpoint).values():
        generatorIRI = iris.ONTO_ENERGY_SYSTEM_KB + generator_name.replace(' ', '') + str(n)
        n += 1
    
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    if generatorIRI != "" and generator_name != "":
        query = iris.ONTO_ENERGY_SYSTEM + \
                iris.RDF + \
                iris.RDFS + \
                iris.XSD + \
                '''INSERT DATA { <%s> rdf:type ontoenergysystem:PowerGenerator . \
                                <%s> rdfs:label "%s"^^xsd:string . }''' % \
                (generatorIRI, generatorIRI, generator_name.replace("PowerGenerator_",""))
        # Initialises the KGClient query
        kg_client = KGClient(query_endpoint, update_endpoint)
        # Executes the query
        kg_client.performUpdate(query)


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
    powerplantIRI = iris.ONTO_ENERGY_SYSTEM_KB + powerplant_name.replace(' ', '')
    n = 1
    # Add number suffix in case pure name based IRI already exists
    while powerplantIRI in get_instantiated_powerplants(query_endpoint, update_endpoint).values():
        powerplantIRI = iris.ONTO_ENERGY_SYSTEM_KB + powerplant_name.replace(' ', '') + str(n)
        n += 1

    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    if powerplantIRI != "" and powerplant_name != "":
        query = iris.ONTO_ENERGY_SYSTEM + \
                iris.RDF + \
                iris.RDFS + \
                iris.XSD + \
                iris.ONTO_EIP + \
                '''INSERT DATA { <%s> rdf:type ontoenergysystem:PowerPlant . \
                                <%s> rdfs:label "%s"^^xsd:string . }''' % \
                (powerplantIRI, powerplantIRI, powerplant_name.replace("PowerPlant_",""))
        # Initialises the KGClient query
        kg_client = KGClient(query_endpoint, update_endpoint)
        # Executes the query
        kg_client.performUpdate(query)


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
    measurement_iri = iris.ONTO_ENERGY_SYSTEM_KB + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)
    
    # Initialises remote KG client with query AND update endpoints specified
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Initialises TS client
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    query = iris.ONTO_POW_SYS + \
            iris.ONTO_ENERGY_SYSTEM + \
            iris.OM + \
            iris.RDF + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontopowsys:GeneratedActivePower ; \
                 om:hasUnit <%s> . }''' % (powerplantIRI, measurement_iri, measurement_iri, units)

    # Executes the query
    kg_client.performUpdate(query)
    print("Time series triples independent of Java TimeSeriesClient successfully instantiated.")

    # # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # # ".class" does not work as this command also exists in Python
    # Instant = jpsBaseLibView.java.time.Instant
    # instant_class = Instant.now().getClass()
    # double_class = jpsBaseLibView.java.lang.Double.TYPE

    # # Initialise time series in both KG and RDB using TimeSeriesClass
    # TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    # TSClient.initTimeSeries([measurement_iri], [double_class], TIME_FORMAT)

    # print("Time series triples via Java TimeSeriesClient successfully instantiated.")


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
    measurement_iri = iris.ONTO_ENERGY_SYSTEM_KB + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)
    
    # Initialises remote KG client with query AND update endpoints specified
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Initialises TS client
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = iris.ONTO_POW_SYS + \
            iris.ONTO_ENERGY_SYSTEM + \
            iris.OM + \
            iris.RDF + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontopowsys:GeneratedActivePower ; \
                 om:hasUnit <%s> . }''' % (generatorIRI, measurement_iri, measurement_iri, units)

    # Executes the query
    kg_client.performUpdate(query)

def add_time_series(instance_IRI, timestamps, values, units, query_endpoint: str = QUERY_ENDPOINT,
                            update_endpoint: str = UPDATE_ENDPOINT): 
    """
        Directly adds time series information. 

        For a single generator/powerplant, for a day (so expecting 48 time periods). 
    """
    ###
    # 1) Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint, update_endpoint)
        # Initialises TS client
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    dataIRIs = []
    activepowergenerated_IRI = iris.ONTO_ENERGY_SYSTEM_KB + 'ActivePowerGenerated_' + str(uuid.uuid4())
    dataIRIs.append(activepowergenerated_IRI)

    #Performs SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = iris.ONTO_POW_SYS + \
            iris.ONTO_ENERGY_SYSTEM + \
            iris.OM + \
            iris.RDF + \
            '''INSERT DATA { \
            <%s> ontopowsys:hasActivePowerGenerated <%s> . \
            <%s> rdf:type ontopowsys:GeneratedActivePower . \
            <%s> om:hasUnit <%s> . }''' % (instance_IRI, activepowergenerated_IRI, activepowergenerated_IRI, activepowergenerated_IRI, units)
    ###

    kg_client.executeUpdate(query)
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

    ts_client.add_timeseries(times, variables, values)
    logger.info("Time-series data has been successfully added to the KG")

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


def add_time_series_data(assetIRI, power_data, asset_name='', query_endpoint: str = QUERY_ENDPOINT,
                            update_endpoint: str = UPDATE_ENDPOINT):
    """
        Adds given gas power data (DataFrame) to time series of respective powerplant/generator (asset).

        Arguments:
            assetIRI - IRI of asset to which power data shall be added.
            power_data - power data to add to time series (as DataFrame with columns ['time', 'power']).
            asset_name - asset name (optional).
    """
    print("Adding time series data for: " + asset_name)

    # Extract data to create Java TimeSeries object
    measurementIRI = get_measurementIRI(query_endpoint, update_endpoint, assetIRI)
    variables = measurementIRI
    if variables is None:
        print('Time series could not be created for the following asset:', assetIRI)
        return
    times = power_data['time']
    powers = power_data['power']
    #Reformat times. 
    times = times.values.reshape(-1,).tolist()
    #Reformat variables (or just a single variable), could already be in this form. 
    if type(variables) == str:
        a = measurementIRI #a is just a temporary variable for this. 
        variables = []
        variables.append(a)
    
    #Reformat values. 
    values = powers.values.reshape(-1,).tolist() #tolist is noted in some older posts online to convert to float, but it does not seem to here, so conversion from float64 (numpy) to float. 
    a = [] #a is just a temporary variable for this. 
    for value in values:
        a.append(float(value))
    values = []
    values.append(a)
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint, update_endpoint)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)
    ts_client.add_timeseries(times, variables, values)
    # Creates time series data
    # ts_list = []
    # for i in range(len(times)):
    #     ts = TSClient.create_timeseries(times[i], variables[i], values[i])
    #     ts_list.append(ts)
    # with ts_client.connect() as conn:
    #     ts_client.tsclient.bulkaddTimeSeriesData(ts_list, conn)
    logger.info(f'Time series data for the asset {asset_name} successfully added to KG.')

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


def update_triple_store(query_endpoint: str = QUERY_ENDPOINT,
                            update_endpoint: str = UPDATE_ENDPOINT):
    """
        Main function to assimilate power time series data into KG, incl. instantiation
        of relevant relationships and new generators in case new data becomes available
    """

    # Get the power data from National Grid csv as DataFrame
    powerplant_power_data, generator_power_data = get_power_data_from_api()

    #Now do the same for powerplants as will be done for generators. 
    # Retrieve all powerplants with available power data (powerplant names are capitalised)
    powerplants_with_data = powerplant_power_data['powerplanteic'].unique()

    # Retrieve all instantiated powerplants in KG
    powerplants = get_instantiated_powerplants(query_endpoint, update_endpoint)
    powerplants_instantiated = {powerplant_class_prefix + k: v for k, v in powerplants.items()}

    # Potentially create new powerplant instances for powerplants with available power data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    new_powerplants = False
    for gt in powerplants_with_data:
        if (gt not in powerplants_instantiated.keys()) and (gt != ""):
            instantiate_powerplant(query_endpoint, update_endpoint, gt)
            new_powerplants = True

    # Retrieve update of instantiated powerplants in KG (in case any new powerplants were added)
    if new_powerplants:
        powerplants = get_instantiated_powerplants(query_endpoint, update_endpoint)
        powerplants_instantiated = {powerplant_class_prefix + k: v for k, v in powerplants.items()}
    # Assimilate power data for instantiated  powerplants
    for pp in powerplants_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if get_measurementIRI(query_endpoint, update_endpoint, powerplants_instantiated[pp]) is None:
            print("No instantiated timeseries detected for: ", pp)
            instantiate_powerplant_timeseries(query_endpoint, update_endpoint, powerplants_instantiated[pp], pp)
        else:
            print("Instantiated time series detected!")
        # Retrieve power time series data for respective powerplant from overall DataFrame
        new_data = powerplant_power_data[powerplant_power_data['powerplanteic'] == pp][['time', 'power']]
        # Add time series data using Java TimeSeriesClient
        add_time_series_data(powerplants_instantiated[pp], new_data, pp)

    # Retrieve all generators with available power data (generator names are capitalised)
    generators_with_data = generator_power_data['generatoreic'].unique()
    # Retrieve all instantiated generators in KG
    generators = get_instantiated_generators(query_endpoint, update_endpoint)
    generators_instantiated = {powerGenerator_class_prefix + k: v for k, v in generators.items()}
    # Potentially create new generator instances for generators with available power data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    new_generators = False
    for gt in generators_with_data:
        if (gt not in generators_instantiated.keys()) and (gt != ""):
            instantiate_generator(query_endpoint, update_endpoint, gt)
            new_generators = True

    # Retrieve update of instantiated generators in KG (in case any new generators were added)
    if new_generators:
        generators = get_instantiated_generators(query_endpoint, update_endpoint)
        generators_instantiated = {powerGenerator_class_prefix + k: v for k, v in generators.items()}

    # Assimilate power data for instantiated generators
    for gt in generators_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if (get_measurementIRI(query_endpoint, generators_instantiated[gt]) is None) and (gt != ""):
            print("No instantiated timeseries detected for: ", gt)
            print('gt:', gt)
            print('generators_instantiated[gt]:', generators_instantiated[gt])
            instantiate_generator_timeseries(query_endpoint, update_endpoint, generators_instantiated[gt], gt)
            # Retrieve power time series data for respective generator from overall DataFrame
            new_data = generator_power_data[generator_power_data['generatoreic'] == gt][['time', 'power']]
            # Add time series data using Java TimeSeriesClient
            add_time_series_data(generators_instantiated[gt], new_data, gt)
        else:
            print("Instantiated time series detected!")
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
            # if not emailed:
            #     sender = TSClient.jpsBaseLibView.EmailSender()
            #     sender.sendEmail(
            #         "GasGridAgent - Exception when gathering live power data.",
            #          """
            #             The 'input_power_data.py' script of the GasGridAgent has encountered an Exception. This script will continue to loop (attempting to gather data every 12 minutes), as the issue may be temporary.
            #             \n\n
            #             It is recommended that a developer logs into the relevant VM and checks the logs for the gas-grid-agent Docker container.
            #         """
            #     )
            #     emailed = True

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


def instantiate_all_generators():
    
    # # Create specified PostgreSQL database
    # kg.create_postgres_db()
    # # Create specified Blazegraph namespace
    # kg.create_blazegraph_namespace()

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
    instantiate_all_generators()
