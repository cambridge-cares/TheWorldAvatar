"""
    Script to (periodically) gather instantaneous gas flow data for UK's gas supply terminals (from National Grid)

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
# get settings and functions from kg_utils module
import kg_utils as kg


def instantiate_terminal(query_endpoint, update_endpoint, terminal_name):
    """
        Instantiates new gas terminal in knowledge graph to enable gas flow data assimilation.
        (solely creates new GasTerminal instance with respective name, but no further relationships)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            terminal_name - name of gas terminal to be instantiated.
    """
    print("Instantiate new gas terminal: " + terminal_name)

    # Create unique IRI for new gas terminal based on terminal name
    terminalIRI = kg.PREFIXES['compa'] + terminal_name.replace(' ', '')
    n = 1
    # Add number suffix in case pure name based IRI already exists
    while terminalIRI in kg.get_instantiated_terminals(query_endpoint).values():
        terminalIRI = kg.PREFIXES['compa'] + terminal_name.replace(' ', '') + str(n)
        n += 1

    # Initialise remote KG client with query AND update endpoints specified
    KGClient = jpsBaseLibView.RemoteStoreClient(query_endpoint, update_endpoint)
    
    # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
    query = kg.create_sparql_prefix('comp') + \
            kg.create_sparql_prefix('rdf') + \
            kg.create_sparql_prefix('rdfs') + \
            kg.create_sparql_prefix('xsd') + \
            '''INSERT DATA { <%s> rdf:type comp:GasTerminal . \
                             <%s> rdfs:label "%s"^^xsd:string . }''' % \
            (terminalIRI, terminalIRI, terminal_name)
    KGClient.executeUpdate(query)


def instantiate_timeseries(query_endpoint, update_endpoint, terminalIRI, terminal_name=''):
    """
        Instantiates all relevant triples for time series storage in KG and initialises RDB tables for terminal.
        (raises exception if time series association is already initialised)

        Arguments:
            query_endpoint - SPARQL Query endpoint for knowledge graph.
            update_endpoint - SPARQL Update endpoint for knowledge graph.
            terminalIRI - full gas terminal IRI incl. namespace (without trailing '<' or '>').
            terminal_name - gas terminal name (optional).
    """
    if terminal_name != '':
        print("Instantiate time series association for: " + terminal_name)
    else:
        print("Instantiate time series association.")

    # Create UUIDs for IntakenGas, VolumetricFlowRate, and Measure instances
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
    query = kg.create_sparql_prefix('comp') + \
            kg.create_sparql_prefix('compa') + \
            kg.create_sparql_prefix('om') + \
            kg.create_sparql_prefix('rdf') + \
            '''INSERT DATA { \
            <%s> comp:hasTaken compa:%s . \
            compa:%s rdf:type comp:IntakenGas . \
            compa:%s rdf:type om:VolumetricFlowRate; \
                     om:hasPhenomenon compa:%s; \
                     om:hasValue compa:%s . \
            compa:%s rdf:type om:Measure; \
                     om:hasUnit om:cubicMetrePerSecond-Time. }''' % (
                terminalIRI, gas, gas, quantity, gas, measurement, measurement)

    KGClient.executeUpdate(query)
    print("Time series triples independent of Java TimeSeriesClient successfully instantiated.")

    # 2) Perform SPARQL update for time series related triples (i.e. via TimeSeriesClient)
    # Retrieve Java classes for time entries (Instant) and data (Double) - to get class simply via Java's
    # ".class" does not work as this command also exists in Python
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    double_class = jpsBaseLibView.java.lang.Double.TYPE

    # Derive MeasurementIRI to which time series is actually connected to
    measurement_iri = kg.PREFIXES['compa'] + measurement
    print("Measurement IRI for actual time series: ", measurement_iri)

    # Initialise time series in both KG and RDB using TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)
    TSClient.initTimeSeries([measurement_iri], [double_class], kg.FORMAT)

    print("Time series triples via Java TimeSeriesClient successfully instantiated.")


def get_flow_data_from_csv():
    """
        Gathers instantaneous flow rate data for each terminal from national grid website.

        Returns:
            DataFrame with gas flow rate data and columns 'terminal', 'time (utc)', 'flowrate (m3/s)'.
            (all terminal names are capitalised for naming consistency reasons)
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

            if "Terminal Totals" in row[0]:
                terminalStartLine = currentRow
                print("Terminal data starts on CSV row", currentRow)
            elif "Total System Supply" in row[0]:
                terminalEndLine = currentRow
                print("Terminal data ends on CSV row", currentRow)

            if (currentRow > terminalStartLine) and (currentRow < terminalEndLine):
                # Parse the CSV rows
                terminalName = row[0]

                # Times from CSV file are in local UK time
                dateTimeObj = datetime.datetime.strptime(row[3], "%d/%m/%Y %H:%M:%S")
                # is_dst=False is used to determine correct timezone in the ambiguous period
                # at the end of daylight saving time
                dateTimeObjUTC = tz.localize(dateTimeObj, False).astimezone(pytz.utc)
                dateTimeStr = dateTimeObjUTC.strftime("%Y-%m-%dT%H:%M:%SZ")

                flowValue = row[2]
                data.append([terminalName, dateTimeStr, flowValue])

    print("Finished reading flow data CSV, removing file...\n")
    # os.remove(filename)

    # Create DataFrame
    df = pd.DataFrame(data, columns=['terminal', 'time (utc)', 'flowrate (m3/s)'])
    # Convert flow from MCM/Day to M^3/S
    df['flowrate (m3/s)'] = (df['flowrate (m3/s)'].astype(float) * 1000000) / (24 * 60 * 60)
    # Capitalise terminal names (for consistent comparisons by name)
    df['terminal'] = df['terminal'].str.upper()

    return df


def add_time_series_data(terminalIRI, flow_data, terminal_name=''):
    """
        Adds given gas flow data (DataFrame) to time series of respective terminal.

        Arguments:
            terminalIRI - IRI of gas terminal to which flow data shall be added.
            flow_data - gas flow data to add to time series (as DataFrame with columns
                        ['time (utc)', 'flowrate (m3/s)'].
            terminal_name - gas terminal name (optional).
    """
    print("Adding time series data for: " + terminal_name)

    # Extract data to create Java TimeSeries object
    measurementIRI = kg.get_measurementIRI(kg.QUERY_ENDPOINT, terminalIRI)
    times = list(flow_data['time (utc)'].values)
    flows = list(flow_data['flowrate (m3/s)'].values)

    # Create Java TimeSeries object
    timeseries = jpsBaseLibView.TimeSeries(times, [measurementIRI], [flows])

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
        Main function to assimilate gas flow time series data into KG, incl. instantiation
        of relevant relationships and new gas terminals in case new data becomes available
    """

    # Read properties file incl. SPARQL endpoints
    kg.read_properties_file(kg.PROPERTIES_FILE)
    print("Connected to KG SPARQL endpoints:")
    print("Queries: ", kg.QUERY_ENDPOINT)
    print("Updates: ", kg.UPDATE_ENDPOINT)

    # Get the gas flow data from National Grid csv as DataFrame
    flow_data = get_flow_data_from_csv()   

    # Retrieve all terminals with available gas flow data (terminal names are capitalised)
    terminals_with_data = flow_data['terminal'].unique()

    # Retrieve all instantiated gas terminals in KG
    terminals = kg.get_instantiated_terminals(kg.QUERY_ENDPOINT)
    terminals_instantiated = {k.upper(): v for k, v in terminals.items()}

    # Potentially create new GasTerminal instances for terminals with available gas flow data,
    # which are not yet instantiated in KG (only create instance to enable data assimilation)
    new_terminals = False
    for gt in terminals_with_data:
        if gt not in terminals_instantiated.keys():
            instantiate_terminal(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, gt.title())
            new_terminals = True

    # Retrieve update of instantiated gas terminals in KG (in case any new terminals were added)
    if new_terminals:
        terminals = kg.get_instantiated_terminals(kg.QUERY_ENDPOINT)
        terminals_instantiated = {k.upper(): v for k, v in terminals.items()}

    # Assimilate gas flow data for instantiated gas terminals
    for gt in terminals_instantiated:
        # Potentially instantiate time series association (if not already instantiated)
        if kg.get_measurementIRI(kg.QUERY_ENDPOINT, terminals_instantiated[gt]) is None:
            print("No instantiated timeseries detected for: ", gt)
            instantiate_timeseries(kg.QUERY_ENDPOINT, kg.UPDATE_ENDPOINT, terminals_instantiated[gt], gt)
        else:
            print("Instantiated time series detected!")

        # Retrieve gas flow time series data for respective terminal from overall DataFrame
        new_data = flow_data[flow_data['terminal'] == gt][['time (utc)', 'flowrate (m3/s)']]

        # Add time series data using Java TimeSeriesClient
        add_time_series_data(terminals_instantiated[gt], new_data, gt)


def continuous_update():
    """
        Ensures that the update_triple_store() function runs once per 12 minutes (as this is the
        interval at which new flow data is published).
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
                    "GasGridAgent - Exception when gathering live flow data.",
                     """
                        The 'input_flow_data.py' script of the GasGridAgent has encountered an Exception. This script will continue to loop (attempting to gather data every 12 minutes), as the issue may be temporary.
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
