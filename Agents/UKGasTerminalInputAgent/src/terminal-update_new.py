from tqdm import tqdm
import time
import pytz
from SPARQLWrapper import SPARQLWrapper, POST
import os
import datetime
import uuid
import sys
import traceback
import wget
import csv
import re
import json
from configobj import ConfigObj

# get the jpsBaseLibGW instance from the jpsSingletons module
from jpsSingletons import jpsBaseLibGW

"""
Script to (periodically) gather instantaneous gas flow data for UK's gas supply terminals (from National Grid)

Local deployment requires:
    - Blazegraph running in local Tomcat server with port 9999 exposed
    - Triple store endpoint to use for data assimilation (namespace in Blazegraph) needs to be created beforehand
    - PostgreSQL database set up locally (with URL and credentials provided in properties file)
"""

# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(os.getcwd(), "..", "resources", "timeseries.properties"))

# Define time series properties
# Define format of time series time entries: Year-Day-Month T hour:minute:second:millisecond Z
FORMAT = 'YYYY-dd-mmTHH:MM:SS.msZ'

# Define global variables
global FALLBACK_KG, NAMESPACE, QUERY_ENDPOINT, UPDATE_ENDPOINT

# Define PREFIXES for SPARQL queries (WITHOUT trailing '<' and '>')
PREFIXES = {
    'comp':  'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#',
    'compa': 'http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/',
    'om':    'http://www.ontology-of-units-of-measure.org/resource/om-2/',
    'rdf':   'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs':  'http://www.w3.org/2000/01/rdf-schema#',
    'ts':    'http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#',
    'xsd':   'http://www.w3.org/2001/XMLSchema#',
}

# Defining IRI of each terminal
TERMINAL_DICTIONARY = {
    "BACTON IPs Terminal": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/BactonIPsTerminal>",
    "BACTON UKCS Terminal": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/BactonUKCSTerminal>",
    "BARROW TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/BarrowTerminal>",
    "EASINGTON TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/EasingtonTerminal>",
    "ISLE OF GRAIN TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/IsleofGrainTerminal>",
    "MILFORD HAVEN TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/MilfordHavenTerminal>",
    "ST FERGUS TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/StFergusTerminal>",
    "TEESSIDE TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/TeessideTerminal>",
    "THEDDLETHORPE TERMINAL": "<http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/TheddlethorpeTermin>"
}


def read_properties_file(filepath):
    """
        Reads local KG location and namespace from properties file (as global variables).

        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global variables as scope
    global FALLBACK_KG, NAMESPACE

    # Read properties file
    props = ConfigObj(filepath)

    # Extract local KG location and set as fallback
    try:
        FALLBACK_KG = props['fallback_kg']
    except KeyError:
        raise KeyError('Key "fallback_kg" is missing in properties file: ' + filepath)
    if FALLBACK_KG == '':
        raise KeyError('No "fallback_kg" value has been provided in properties file: ' + filepath)

    # Extract KG's SPARQL endpoint to use (namespace in Blazegraph)
    try:
        NAMESPACE = props['namespace']
    except KeyError:
        raise KeyError('Key "namespace" is missing in properties file: ' + filepath)
    if NAMESPACE == '':
        raise KeyError('No "namespace" value has been provided in properties file: ' + filepath)


def setKGEndpoints(filepath):
    """
        Sets the correct URLs for KG's SPARQL Query and Update endpoints (as global variables).

        Arguments:
             filepath - absolute file path to properties file.
    """

    # Define global variables as scope
    global FALLBACK_KG, NAMESPACE, QUERY_ENDPOINT, UPDATE_ENDPOINT

    # Check for the KG_LOCATION environment variable, using local fallback
    kgRoot = os.getenv('KG_LOCATION', FALLBACK_KG)

    # Construct full URLs for SPARQL endpoints
    if kgRoot.endswith("/"):
        QUERY_ENDPOINT = kgRoot + "namespace/" + NAMESPACE + "/sparql"
        UPDATE_ENDPOINT = QUERY_ENDPOINT
    else:
        QUERY_ENDPOINT = kgRoot + "/namespace/" + NAMESPACE + "/sparql"
        UPDATE_ENDPOINT = QUERY_ENDPOINT

    # Read properties file
    props = ConfigObj(filepath)
    # Write SPARQL endpoints to properties file (overwrite potentially existing entries)
    props['sparql.query.endpoint'] = QUERY_ENDPOINT
    props['sparql.update.endpoint'] = UPDATE_ENDPOINT
    props.write()


def create_sparql_prefix(abbreviation, prefixes):
    """
        Constructs proper SPARQL Prefix String for given namespace abbreviation.

        Arguments:
            abbreviation - namespace abbreviation to construct SPARQL PREFIX string for.
            prefixes - dictionary of pre-specified prefixes and respective full IRIs.

        Returns:
            SPARQL query prefix string in the form "PREFIX ns: <full IRI>".
    """

    # Raise key error if given namespace abbreviation has not been specified
    if abbreviation not in prefixes.keys():
        raise KeyError('Prefix: "' + abbreviation + '" has not been specified')

    # Get full IRI from pre-specified prefixes dictionary
    iri = prefixes[abbreviation]

    if not iri.startswith('<'):
        iri = '<' + iri
    if not iri.endswith('>'):
        iri = iri + '>'

    return 'PREFIX ' + abbreviation + ': ' + iri + ' '


def get_instantiated_terminals(endpoint):
    """
        Retrieves names and IRIs of all instantiated GasTerminals in the knowledge graph.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.

        Returns:
            Dictionary of all instantiated gas terminals (name as key, IRI as value)
            (empty dictionary in case no terminals are instantiated)
    """

    # Initialise SPARQL query variables for gas terminal IRIs and names
    var1, var2 = 'iri', 'name'

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp', PREFIXES) + \
            create_sparql_prefix('rdf', PREFIXES) + \
            create_sparql_prefix('rdfs', PREFIXES) + \
            'SELECT ?' + var1 + ' ?' + var2 + ' ' \
            'WHERE { ?' + var1 + ' rdf:type comp:GasTerminal; \
                                   rdfs:label ?' + var2 + '. }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    # Create dictionary of query results with gas terminal name as key and IRI as value
    res = dict()
    for r in response:
        res[r[var2]] = r[var1]

    return res


def check_timeseries_instantiation(endpoint, terminalIRI):
    """
        Check whether terminal already has an instantiated gas flow time series attached to it.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').

        Returns:
            True - if gas terminal is associated with a time series via the specified path.
            False - otherwise.
    """

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp', PREFIXES) + \
            create_sparql_prefix('om', PREFIXES) + \
            create_sparql_prefix('ts', PREFIXES) + \
            'SELECT * ' \
            'WHERE { <' + terminalIRI + '> comp:hasTaken/^om:hasPhenomenon/om:hasValue/ts:hasTimeSeries ?a }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    # Return True if any TimeSeries is associated with gas terminal via specified property path
    if len(response) > 0:
        return True
    else:
        return False


def get_measurementIRI(endpoint, terminalIRI):
    """
        Retrieves gas flow MeasurementIRI for terminal, which is actually connected to time series.

        Arguments:
            endpoint - SPARQL Query endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').

        Returns:
            Full gas flow MeasurementIRI incl. namespace (without trailing '<' or '>').
    """

    # Initialise SPARQL query variable
    var = 'iri'

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(endpoint)
    # Perform SPARQL query (see StoreRouter in jps-base-lib for further details)
    query = create_sparql_prefix('comp', PREFIXES) + \
            create_sparql_prefix('om', PREFIXES) + \
            create_sparql_prefix('ts', PREFIXES) + \
            'SELECT ?' + var + ' '\
            'WHERE { <' + terminalIRI + '> comp:hasTaken/^om:hasPhenomenon/om:hasValue ?' + var + '. }'
    response = KGClient.execute(query)
    # Convert JSONArray String back to list
    response = json.loads(response)

    if len(response) == 0:
        return None
    else:
        return response[0][var]


def instantiate_timeseries(query_endpoint, update_endpoint, terminalIRI):
    """
        Instantiates all relevant triples for time series storage in KG and initialises RDB tables for terminal.
        (raises exception if time series association is already initialised)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').
    """

    # Create UUIDs for IntakenGas, VolumetricFlowRate, and Measure instances
    gas = 'GasAmount_' + str(uuid.uuid4())
    quantity = 'Quantity_' + str(uuid.uuid4())
    measurement = 'Measurement_' + str(uuid.uuid4())

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.timeseries.*")

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)
    # 1) Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = create_sparql_prefix('comp', PREFIXES) + \
            create_sparql_prefix('compa', PREFIXES) + \
            create_sparql_prefix('om', PREFIXES) + \
            create_sparql_prefix('rdf', PREFIXES) + \
            create_sparql_prefix('ts', PREFIXES) + \
            create_sparql_prefix('xsd', PREFIXES) + \
            '''INSERT DATA { \
            <%s> comp:hasTaken compa:%s. \
            compa:%s rdf:type comp:IntakenGas. \
            compa:%s rdf:type om:VolumetricFlowRate; \
                     om:hasPhenomenon compa:%s; \
                     om:hasValue compa:%s. \
            compa:%s rdf:type om:Measure; \
                     om:hasUnit om:cubicMetrePerSecond-Time. }'''%(
            terminalIRI, gas, gas, quantity, gas, measurement, measurement)
    KGClient.executeUpdate(query)

    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLib_view.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLib_view.java.lang.Double.TYPE

    # Get MeasurementIRI to which time series is actually connected to
    measurement_iri = get_measurementIRI(QUERY_ENDPOINT, terminalIRI)

    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLib_view.TimeSeriesClient(instant_class, PROPERTIES_FILE)
    TSClient.initTimeSeries([measurement_iri], [double_class], FORMAT)


def get_flow_data_from_csv():
    """
        Gathers instantaneous flow rate data for each terminal from national grid website.

        Returns:
            2D array of gas flow rates (triples [terminalName, time, flow]).
    """

    # Get current UK timezone to properly convert reported local times into UTC
    # (i.e. account for daylight saving time)
    tz = pytz.timezone('Europe/London')

    print("Downloading latest flow data CSV...")
    url = "https://mip-prd-web.azurewebsites.net/InstantaneousViewFileDownload/DownloadFile"
    filename = wget.download(url)
    
    # 2D array of data (triples [terminalName, time, flow])
    data = []
    
    print("Reading flow data CSV...")
    with open(filename, newline='') as csvfile:
        reader = csv.reader(csvfile)
        
        currentRow = -1
        terminalStartLine = 9999
        terminalEndLine = 9999
        
        for row in reader:  
            currentRow = currentRow + 1
            
            if "Terminal Totals" in row[0] :
                terminalStartLine = currentRow
                print("Terminal data starts on CSV row", currentRow)
            elif "Total System Supply" in row[0]:
                terminalEndLine = currentRow
                print("Terminal data ends on CSV row", currentRow)
                
            if (currentRow > terminalStartLine) and (currentRow < terminalEndLine):
            
                # Parse the CSV rows
                terminalName = row[0]

                # Times from CSV file are in local time
                dateTimeObj = datetime.datetime.strptime(row[3], "%d/%m/%Y %H:%M:%S")
                # is_dst=False is used to determine correct timezone in the ambigous period
                # at the end of daylight saving time
                dateTimeObjUTC = tz.localize(dateTimeObj, False).astimezone(pytz.utc)
                dateTimeStr = dateTimeObjUTC.strftime("%Y-%m-%dT%H:%M:%S.000Z")

                flowValue = row[2]
                data.append([terminalName, dateTimeStr, flowValue])

    print("Finished reading flow data CSV, removing file...")
    os.remove(filename)
    return data


def update_triple_store():

    today = datetime.datetime.now()
    print("\nPerforming update at: ", today)

    # Build the correct KG URL
    kgURL = getKGLocation(NS)
    print("Determined KG URL as", kgURL)

    # Get the flow data from CSV
    # 2D array of tiples ([terminal-name, time, flow])
    data = get_flow_data_from_csv()

    for i in range(0, len(data)):
        dataRow = data[i]
        terminalURI = TERMINAL_DICTIONARY[dataRow[0]]
        time = dataRow[1]

        # Convert flow from MCM/Day to M^3/S
        flow = dataRow[2]
        gasVolume = str( (float(flow) * 1000000) / (24*60*60) )

        # Create UUID for IntakenGas, quantity and measurement.
        gas_uuid = uuid.uuid1()
        quan_uuid = uuid.uuid1()
        mes_uuid = uuid.uuid1()

        query = '''PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
        PREFIX compa:   <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>
        PREFIX om:      <http://www.ontology-of-units-of-measure.org/resource/om-2/>

        INSERT DATA
        { compa:%s rdf:type comp:IntakenGas.
        %s comp:hasTaken compa:%s.
        compa:%s rdf:type om:Measure;
                 om:hasNumericalValue %s;
                om:hasUnit om:cubicMetrePerSecond-Time.
        compa:%s rdf:type om:VolumetricFlowRate;
              om:hasPhenomenon compa:%s;
              om:hasValue compa:%s.
        compa:%s comp:atUTC "%s"^^xsd:dateTime .} '''%(gas_uuid,
                                                       terminalURI,
                                                       gas_uuid,
                                                       mes_uuid,
                                                       gasVolume,
                                                       quan_uuid,
                                                       gas_uuid,
                                                       mes_uuid,
                                                       gas_uuid,
                                                       time)



        sparql = SPARQLWrapper(kgURL)
        sparql.setMethod(POST) # POST query, not GET
        sparql.setQuery(query)

        print("Running SPARQL update " + str(i + 1) + " of " + str(len(data)) + "...")
        ret = sparql.query()


    print("All SPARQL updates finished.")
    return


def continuous_update():
    while True:
        start = time.time()
        
        try:
            update_triple_store()
        except Exception:
            print("Encountered exception, will try again in 15 minutes...")
            print(traceback.format_exc())
               
        end = time.time()
        
        # wait for 12 minutes taking into account time to update queries
        for i in tqdm(range(60*12-int((end-start)))):
            time.sleep(1)
    return 


def single_update():
    update_triple_store()
    return 


# # Try to detect (command-line) arguments passed to the script and launch update method
# if len(sys.argv) <= 1:
#     single_update()
# elif sys.argv[1] == '-single':
#     print('Detected \'-single\' argument, running single update...')
#     single_update()
# elif sys.argv[1] == '-continuous':
#     print('Detected \'-continuous\' argument, running continuous updates...')
#     continuous_update()
# else:
#     single_update()

if __name__ == '__main__':

    # read properties file
    read_properties_file(PROPERTIES_FILE)

    # set URL to KG SPARQL endpoint
    setKGEndpoints(PROPERTIES_FILE)

    # retrieve all instantiated gas terminals in KG
    terminals = get_instantiated_terminals(QUERY_ENDPOINT)

    for gt in terminals:

        if gt == 'Bacton IPs Terminal':

            # check if associated time series is already instantiated
            instantiated = check_timeseries_instantiation(QUERY_ENDPOINT, terminals[gt])

            if not instantiated:
                instantiate_timeseries(QUERY_ENDPOINT, UPDATE_ENDPOINT, terminals[gt])
