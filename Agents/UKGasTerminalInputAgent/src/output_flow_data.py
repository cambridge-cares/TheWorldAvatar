"""
    This script queries the KG for the last day (or any other specified duration) of flow data for every terminal,
    this data is then written to a local JSON file.
    
    It comprises part of the Gas Grid Agent tool and should be executed daily via crontab.
    
    Authors: support<@>cmclinnovations.com, mh807<@>cam.ac.uk
"""

import os
import sys
import shutil
from datetime import datetime as dt

# get the jpsBaseLibGateWay instance from the jpsSingletons module
from src.jpsSingletons import jpsBaseLibGW
# get settings and functions from kg_utils module
import src.kg_utils as kg


def get_gasflow_history(duration, callbackSuccess, callbackFailure):
    """
        Retrieves gas flow time series data for all instantiated Gas Terminals in the KG.
        Once the request is complete, one of the input callback functions will be called.
        
        Arguments:
            duration        - number of hours for which latest data shall be retrieved (in h)
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

    # Initialise timestamps for gas flow time series retrieval durations (time series entries stored as UTC times!)
    # Java Instant instances are associated with UTC (i.e. hold a value of date-time with a UTC time-line)
    now = Instant.now()
    print("INFO: Submitting TimeSeriesClient SPARQL queries at",
          dt.utcfromtimestamp(now.getEpochSecond()).strftime("%Y-%m-%dT%H:%M:%SZ"))
    start_1 = now.minusSeconds(int(1 * duration * 60 * 60))
    start_2 = now.minusSeconds(int(2 * duration * 60 * 60))
    start_7 = now.minusSeconds(int(7 * duration * 60 * 60))

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
            # Get results for last "duration" hours (e.g. 24h)
            timeseries = TSClient.getTimeSeriesWithinBounds([measurement_iri], start_1, now)

            if not timeseries.getTimes():
                # Try last "2 x duration" hours if nothing available for last "duration" hours (e.g. 48h)
                print("WARNING: No results in last %i h, trying last %i h ..." % (duration, 2 * duration))
                timeseries = TSClient.getTimeSeriesWithinBounds([measurement_iri], start_2, now)

                if not timeseries.getTimes():
                    # Last resort, try last "7 x duration" hours (e.g. last week)
                    print("WARNING: No results in last %i h, trying last %i h ..." % (2 * duration, 7 * duration))
                    timeseries = TSClient.getTimeSeriesWithinBounds([measurement_iri], start_7, now)
            # populate timeseries dictionary - format:
            # terminal name: [terminal IRI, measurement IRI, Java time series object]
            timeseries_dict[terminal] = [terminalIRI, measurement_iri, timeseries]

        except Exception as error:
            callbackFailure(error)

    callbackSuccess(timeseries_dict)


def onSuccess(timeseries_data):
    """
        Runs when timeseries data is successfully returned by the get_gasflow_history function.
        
        Arguments:
            timeseries_data - dictionary of time series data in the format:
                              terminal name: [terminal IRI, measurement IRI, Java time series object]
    """

    print("INFO: All SPARQL queries successful, time series received.")
    print("INFO: Number of retrieved time series:", len(timeseries_data))

    # Obtain total number of retrieved data points
    data_points = 0
    for ts in timeseries_data:
        data_points += len(timeseries_data[ts][2].getTimes())
    print("INFO: Number of retrieved time series data points:", data_points)

    # Initialise (JSON) results String
    res = ''

    # Loop through all time series/gas terminals
    for ts in timeseries_data:
        # Retrieve gas flow values and align format
        num_vals = timeseries_data[ts][2].getValues(timeseries_data[ts][1])
        num_vals = [round(float(val), 4) for val in num_vals]

        # Retrieve times as list of Java Instant objects and format as Strings in "2021-06-04T02:58:00.000Z" format
        times_instant = timeseries_data[ts][2].getTimes()
        times_unix = [t.getEpochSecond() for t in times_instant]
        times_string = [dt.utcfromtimestamp(t).strftime("%Y-%m-%dT%H:%M:%S.000Z") for t in times_unix]

        # Loop through all time entries
        for entry in range(len(times_string)):
            # Append results String in format as required for (later) visualisation
            res += '{"s": "' + timeseries_data[ts][0] + '", "UTC": "' + times_string[entry] + '", "num_val": "' \
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
        Runs when get_gasflow_history function results in a failure/exception.
        
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

    # Duration of latest historical data to retrieve (in h)
    duration = 24
    get_gasflow_history(duration, onSuccess, onFailure)


# Entry point, calls main function
if __name__ == "__main__":
    main()
