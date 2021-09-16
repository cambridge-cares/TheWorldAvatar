"""
    This script queries the KG for the last day of flow data for
    every terminal and offtake, this data is then written to a
    local JSON file.
    
    It comprises part of the Gas Grid Agent tool and should be 
    executed daily via crontab.
    
    Author: support<@>cmclinnovations.com
"""

import os 
import sys
import shutil
from datetime import datetime as dt
from datetime import timedelta as td

# get the jpsBaseLibGateWay instance from the jpsSingletons module
from jpsSingletons import jpsBaseLibGW
# get settings and functions from kg_utils module
import kg_utils as kg


def get_gasflow_history(callbackSuccess, callbackFailure):
    """
        Submits the input query to the KG asynchronously (so a timeout can be added).
        Once the request is complete, one of the input callback functions
        will be called.
        
        Arguments:
            namespace         - KG namespace
            callbackSuccess - function to call on success
            callbackFailure - function to call on failure
    """
    
    # Read properties file (to retrieve namespace and fallback_kg)
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Set URLs to KG SPARQL endpoints (and update properties file accordingly)
    kg.setKGEndpoints(kg.PROPERTIES_FILE)
    print("INFO: Determined KG endpoint as '" + kg.QUERY_ENDPOINT + "'.")

    # Create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.timeseries.*")

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    # Get class simply via Java's ".class" does not work as this command also exists in Python
    Instant = jpsBaseLib_view.java.time.Instant
    instant_class = Instant.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLib_view.TimeSeriesClient(instant_class, kg.PROPERTIES_FILE)

    # Initialise timestamps for gas flow time series retrieval durations
    now = dt.utcnow()
    print("INFO: Submitting TimeSeriesClient SPARQL queries at", now.strftime("%Y-%m-%dT%H:%M:%SZ"))
    start_1d = now - td(hours=24 * 1)
    start_2d = now - td(hours=24 * 2)
    start_7d = now - td(hours=24 * 7)

    # Initialise dictionary for all retrieved time series objects
    timeseries_dict = {}

    # Obtain all instantiated GasTerminals in KG
    terminals = kg.get_instantiated_terminals(kg.QUERY_ENDPOINT)
    for terminal in terminals:
        terminalIRI = terminals[terminal]
        # Get MeasurementIRI to which time series is actually connected to
        measurement_iri = kg.get_measurementIRI(kg.QUERY_ENDPOINT, terminalIRI)

        # Retrieve instantaneous gas flow time series history
        try:
            # Get results for last 24 hours
            timeseries = TSClient.getTimeSeriesWithinBounds([measurement_iri],
                                                            Instant.ofEpochSecond(round(start_1d.timestamp())),
                                                            Instant.ofEpochSecond(round(now.timestamp())))
            if not timeseries.getTimes():
                # Try last 48h if nothing available for last 24h
                print("WARNING: No results in last 24 hours, trying last 48h ...")
                timeseries = TSClient.getTimeSeriesWithinBounds([measurement_iri],
                                                                Instant.ofEpochSecond(round(start_2d.timestamp())),
                                                                Instant.ofEpochSecond(round(now.timestamp())))
                if not timeseries.getTimes():
                    # Last resort, try entire last week
                    print("WARNING: No results in last 48 hours, trying last week...")
                    timeseries = TSClient.getTimeSeriesWithinBounds([measurement_iri],
                                                                    Instant.ofEpochSecond(round(start_7d.timestamp())),
                                                                    Instant.ofEpochSecond(round(now.timestamp())))
            # populate timeseries dictionary - format:
            # terminal name: [terminal IRI, measurement IRI, Java time series object]
            timeseries_dict[terminal] = [terminalIRI, measurement_iri, timeseries]

        except Exception as error:
            callbackFailure(error)
        
    callbackSuccess(timeseries_dict)


def onSuccess(timeseries):
    """
        Runs when data is successfully returned by the query.
        
        Arguments:
            data - resulting data from KG
    """

    print("INFO: All SPARQL queries successful, time series received.")
    print("INFO: Number of time series is", len(timeseries))

    # Initialise (JSON) results String
    res = ''

    # Loop through all time series/gas terminals
    for ts in timeseries:
        # Retrieve gas flow values and align format
        num_vals = timeseries[ts][2].getValues(timeseries[ts][1])
        num_vals = [round(float(val), 3) for val in num_vals]

        # Retrieve times as list of Java Instant objects and format as Strings in "2021-06-04T02:58:00.000Z" format
        times_instant = timeseries[ts][2].getTimes()
        times_unix = [t.getEpochSecond() for t in times_instant]
        times_string = [dt.utcfromtimestamp(t).strftime("%Y-%m-%dT%H:%M:%S.000Z") for t in times_unix]

        # Loop through all time entries
        for entry in range(len(times_string)):
            # Append results and align format as required for (later) visualisation
            res += '{"s": "' + timeseries[ts][0] + '", "UTC": "' + times_string[entry] + '", "num_val": "' \
                   + str(num_vals[entry]) + '", "label": "' + ts + '"}, '

    # Adjust final (JSON) results String (removing trailing white spaces, last comma, and add [])
    res_final = res.strip()[:-1]
    res_final = '[' + res_final + ']'

    # Write to a dated file
    now = dt.now()
    filename = "flow-data-" + now.strftime("%Y-%m-%d") + ".json"
    filepath = os.path.join(kg.OUTPUT_DIR, filename)
    
    try:
        print("INFO: Writing data to file at", filepath)
        with open(filepath, "w") as file:
            file.write(res_final)
            file.close()
            
        # Also make copy to flow-data-latest.json (so we don't have to write a dynamic wget call to download it later).
        latestCopy = os.path.join(kg.OUTPUT_DIR, "flow-data-latest.json")
        shutil.copy(filepath, latestCopy)
        print("INFO: Data also written to file at", latestCopy)

        print("SUCCESS: Script completed.")

    except Exception:
        print("WARNING: Could not write data to file, will try local output...")
        
        try:
            with open(filename, "w") as file:
                file.write(res_final)
                file.close()
            print("INFO: Data written to file at ./" + filename)

            print("SUCCESS: Script completed.")
            
        except Exception:
            print("ERROR: Could not write out data file!")
        

def onFailure(error):
    """
        Runs when query results in a failure/exception.
        
        Arguments:
            error - thrown error
    """
    print("ERROR: Could not complete SPARQL query, error is as follows...")
    print("\n" + str(error) + "\n")
    sys.exit()


def main():
    """
        Main function.
    """
    print("\n")
    get_gasflow_history(onSuccess, onFailure)


# Entry point, calls main function
if __name__ == "__main__":
    main()