"""
    Script to (periodically) gather instantaneous gas power data for UK's gas supply generators (from National Grid)

    Local deployment requires:
        - Blazegraph running in local Tomcat server with port 9999 exposed
        - Triple store endpoint to use for data assimilation (namespace in Blazegraph) needs to be created beforehand
          and match namespace provided in timeseries.properties file in resource folder
        - PostgreSQL database set up locally (with URL and credentials provided in timeseries.properties file)

    Authors: trs53<@>cam.ac.uk, mh807<@>cam.ac.uk
"""

from tqdm import tqdm
import time
import pytz
import os
import datetime
import uuid
import sys
import traceback
import wget
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
        Instantiates new gas generator in knowledge graph to enable gas power data assimilation.
        (solely creates new Gasgenerator instance with respective name, but no further relationships)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            generator_name - name of gas generator to be instantiated.
    """
    print("Instantiate new gas generator: " + generator_name)

    # Create unique IRI for new gas generator based on generator name
    generatorIRI = kg.PREFIXES['ontoenergyststem_kb'] + generator_name.replace(' ', '')
    n = 1
    # Add number suffix in case pure name based IRI already exists
    while generatorIRI in kg.get_instantiated_generators(query_endpoint).values():
        generatorIRI = kg.PREFIXES['ontoenergyststem_kb'] + generator_name.replace(' ', '') + str(n)
        n += 1

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)
    
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('ontoenergyststem') + \
            kg.create_sparql_prefix('rdf') + \
            kg.create_sparql_prefix('rdfs') + \
            kg.create_sparql_prefix('xsd') + \
            '''INSERT DATA { <%s> rdf:type ontoenergyststem:Gasgenerator . \
                             <%s> rdfs:label "%s"^^xsd:string . }''' % \
            (generatorIRI, generatorIRI, generator_name)
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
    query = kg.create_sparql_prefix('ontoenergyststem') + \
            kg.create_sparql_prefix('ontoenergyststem_kb') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> ontoenergyststem:hasTaken ontoenergyststem_kb:%s . \
            ontoenergyststem_kb:%s rdf:type ontoenergyststem:IntakenGas . \
            ontoenergyststem_kb:%s rdf:type om:VolumetricpowerRate; \
                     om:hasPhenomenon ontoenergyststem_kb:%s; \
                     om:hasValue ontoenergyststem_kb:%s . \
            ontoenergyststem_kb:%s rdf:type om:Measure; \
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
    measurement_iri = kg.PREFIXES['ontoenergyststem_kb'] + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)

    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries([measurement_iri], [double_class], kg.FORMAT)

    print("Time series triples via Java TimeSeriesClient successfully instantiated.")


def get_power_data_from_api():
    """
        Gathers instantaneous power rate data for each generator from national grid website.

        Returns:
            DataFrame with gas power rate data and columns 'generator', 'time (utc)', 'powerrate (m3/s)'.
            (all generator names are capitalised for naming consistency reasons)
    """

    # Get current UK timezone to properly convert reported local times into UTC
    # (i.e. account for daylight saving time)
    tz = pytz.timezone('Europe/London')
    #####DO THIS LATER#####

    print("Querying API")
    #url = "https://mip-prd-web.azurewebsites.net/InstantaneousViewFileDownload/DownloadFile"
    #filename = wget.download(url)
    #Run the Query Script. If the inputs are valid it will update the CSV. 
    csvName = 'Input-Template.csv'
    Key = ''
    Year = 2021 #This should be a week ago. 
    Month = 11 #This should be a week ago. 
    Day = 14 #This should be a week ago. 
    Period = 24 #Note this doesn't matter if Search is 2, as it goes from 1 - 48 regardless. 
    Search = 2 #This script have multiple run options, for a day we want '2'. 
    bmrs.live_power(csvName, Key, Year, Month, Day, Period, Search)

    # 2D array of data (triples [generatorName, time, power])
    #data = []

    print("Reading power data CSV...")
    #data = pd.read_csv(csvName) #Dataframe including DUKES stations. 
    #These should both be EIC of (station or generator), time, power.  
    powerplant_df, generator_df = bmrs.convert_csv_to_tripple_dfs(csvName)
    
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
    print("Finished reading power data CSV")
    #os.remove(filename)

    '''
    # Create DataFrame
    df = pd.DataFrame(data, columns=['generator', 'time (utc)', 'powerrate (m3/s)'])
    # Convert power from MCM/Day to M^3/S
    df['powerrate (m3/s)'] = (df['powerrate (m3/s)'].astype(float) * 1000000) / (24 * 60 * 60)
    # Capitalise generator names (for consistent comparisons by name)
    df['generator'] = df['generator'].str.upper()
    '''
    return powerplant_df, generator_df


def add_time_series_data(generatorIRI, power_data, generator_name=''):
    """
        Adds given gas power data (DataFrame) to time series of respective generator.

        Arguments:
            generatorIRI - IRI of gas generator to which power data shall be added.
            power_data - gas power data to add to time series (as DataFrame with columns
                        ['time (utc)', 'powerrate (m3/s)'].
            generator_name - gas generator name (optional).
    """
    print("Adding time series data for: " + generator_name)

    # Extract data to create Java TimeSeries object
    measurementIRI = kg.get_measurementIRI(kg.QUERY_ENDPOINT, generatorIRI)
    times = list(power_data['time (utc)'].values)
    powers = list(power_data['powerrate (m3/s)'].values)

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
    print("Time series data successfully added.\n")


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
    powerplant_power_data, generator_power_data = get_power_data_from_api()

    # Retrieve all generators with available power data (generator names are capitalised)
    #generators_with_data = power_data['generator'].unique()

    # Retrieve all instantiated generators in KG
    generators = kg.get_instantiated_generators(kg.QUERY_ENDPOINT)
    generators_instantiated = {k.upper(): v for k, v in generators.items()}

    # Potentially create new generator instances for generators with available power data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    new_generators = False
    for gt in generators_with_data:
        if gt not in generators_instantiated.keys():
            instantiate_generator(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt.title())
            new_generators = True

    # Retrieve update of instantiated generators in KG (in case any new generators were added)
    if new_generators:
        generators = kg.get_instantiated_generators(kg.QUERY_ENDPOINT)
        generators_instantiated = {k.upper(): v for k, v in generators.items()}

    # Assimilate power data for instantiated gas generators
    for gt in generators_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if kg.get_measurementIRI(kg.QUERY_ENDPOINT, generators_instantiated[gt]) is None:
            print("No instantiated timeseries detected for: ", gt)
            instantiate_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, generators_instantiated[gt], gt)
        else:
            print("Instantiated time series detected!")

        # Retrieve power time series data for respective generator from overall DataFrame
        new_data = power_data[power_data['generator'] == gt][['time (utc)', 'powerrate (m3/s)']]

        # Add time series data using Java TimeSeriesClient
        add_time_series_data(generators_instantiated[gt], new_data, gt)


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
