###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 05 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# readings data from the API and instantiate it in the KG

import uuid
import metoffer
import datetime as dt

#import agentlogging
from metoffice.kgutils.javagateway import jpsBaseLibGW
from metoffice.dataretrieval.readings import *
from metoffice.dataretrieval.stations import *
from metoffice.errorhandling.exceptions import APIException
from metoffice.kgutils.kgclient import KGClient
from metoffice.kgutils.tsclient import TSClient
from metoffice.kgutils.prefixes import create_sparql_prefix
from metoffice.kgutils.prefixes import PREFIXES
from metoffice.kgutils.querytemplates import *
from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT, DATAPOINT_API_KEY
from metoffice.utils.readings_mapping import READINGS_MAPPING, UNITS_MAPPING, COMPASS, TIME_FORMAT, DATACLASS


# # Initialise logger
# logger = agentlogging.get_logger("dev")


def instantiate_station_readings(instantiated_sites_list: list,
                                 query_endpoint: str = QUERY_ENDPOINT,
                                 update_endpoint: str = UPDATE_ENDPOINT) -> None:
        """
            Instantiates readings for the provided list of measurement stations
            
            Arguments:
                instantiated_sites_list - list of dictionaries with instantiated
                                          stations/sites in the form [{id : iri},]
        """

        # Create MetOffice client to retrieve readings via API
        try:
            metclient = metoffer.MetOffer(DATAPOINT_API_KEY)
        except:
            raise APIException("MetOffer client could not be created to retrieve station readings.")
            #logger.error("MetOffer client could not be created to retrieve station readings.")
        
        # Initialise update query
        query_string = f"""
            {create_sparql_prefix('rdf')}
            {create_sparql_prefix('rdfs')}
            {create_sparql_prefix('xsd')}
            {create_sparql_prefix('ems')}
            {create_sparql_prefix('kb')}
            {create_sparql_prefix('om')}
            {create_sparql_prefix('ts')}
            INSERT DATA {{
        """

        # Initialise lists for TimeSeriesClient bulk Init
        dataIRIs = []
        dataClasses = []
        timeUnit = []

        # Get already instantiated observations and forecasts (across all stations)
        instantiated_obs = get_all_instantiated_observations(query_endpoint, 
                                                             update_endpoint)
        instantiated_fcs = get_all_instantiated_forecasts(query_endpoint, 
                                                          update_endpoint)
        # Get short version of variable type from full quantity type
        instantiated_obs['reading'] = instantiated_obs['quantityType'].apply(lambda x: x.split('#')[-1])
        instantiated_fcs['reading'] = instantiated_fcs['quantityType'].apply(lambda x: x.split('#')[-1])                                                        

        # Loop over all sites
        for id in instantiated_sites_list:
            
            # Get lists of instantiated readings for current station
            inst_obs = instantiated_obs[instantiated_obs['stationID'] == id]['reading'].tolist()
            inst_fcs = instantiated_fcs[instantiated_fcs['stationID'] == id]['reading'].tolist()

            # Load observations and forecasts for that station from API
            available_obs, available_fcs = retrieve_readings_concepts_for_station(metclient, id)
            
            # Derive quantities to instantiate
            obs = [i for i in available_obs if i not in inst_obs]
            fcs = [i for i in available_fcs if i not in inst_fcs]
            both = [i for i in obs if i in fcs]
            obs = list(set(obs) - set(both))
            fcs = list(set(fcs) - set(both))

            # Get station IRI
            station_iri = instantiated_sites_list[id]

            # Create triples and input lists for TimeSeriesClient bulkInit
            triples1, reading_iris, dataIRIs1, dataClasses1, _ = add_readings_for_station(station_iri, both, is_observation=True)
            triples2, _, dataIRIs2, dataClasses2, _ = add_readings_for_station(station_iri, both, reading_iris, is_observation=False)
            triples3, _, dataIRIs3, dataClasses3, timeUnit3 = add_readings_for_station(station_iri, obs, is_observation=True)
            triples4, _, dataIRIs4, dataClasses4, timeUnit4 = add_readings_for_station(station_iri, fcs, is_observation=False)

            # Add triples to INSERT DATA query
            query_string += triples1
            query_string += triples2
            query_string += triples3
            query_string += triples4

            # Append lists to overarching TimeSeriesClient input lists
            if dataIRIs1 + dataIRIs3:
                dataIRIs.append(dataIRIs1 + dataIRIs3)
                dataClasses.append(dataClasses1 + dataClasses3)
                timeUnit.append(timeUnit3)
            if dataIRIs2 + dataIRIs4:
                dataIRIs.append(dataIRIs2 + dataIRIs4)
                dataClasses.append(dataClasses2 + dataClasses4)
                timeUnit.append(timeUnit4)

        # Close query
        query_string += f"}}"

        # Instantiate all non-time series triples
        kg_client = KGClient(query_endpoint, update_endpoint)
        kg_client.performUpdate(query_string)

        if dataIRIs:
            # Instantiate all time series triples
            ts_client = TSClient()
            ts_client.ts_client.bulkInitTimeSeries(dataIRIs, dataClasses, timeUnit)


def instantiate_all_station_readings(query_endpoint: str = QUERY_ENDPOINT,
                                     update_endpoint: str = UPDATE_ENDPOINT) -> None:
        """
            Instantiates all readings for all instantiated stations
        """

        stations = get_all_metoffice_stations()
        selection = ['3041']
        stations_subset = {key: stations[key] for key in selection}
        
        instantiate_station_readings(stations_subset)


def add_readings_for_station(station_iri: str,
                             readings: list, readings_iris: list = None, 
                             is_observation: bool = True,
                             quantity_comments: list = None):
    """
        Return SPARQL update query string to instantiate readings for given 
        station IRI (query string to be included in overarching INSERT DATA query)

        Arguments:
            station_iri - Station IRI without trailing '<' and '>'
            readings - list of station readings to instantiate
                       (i.e. OntoEMS concept names)
            readings_iris - list of IRIs for station readings (only relevant to
                            link observation and forecast readings for same quantity
                            to same instance instead of creating duplicates)
            is_observation - boolean to indicate whether readings are measure
                             or forecast
            quantity_comments - comments to be attached to quantities
        
        Returns
            triples - triples to be added to INSERT DATA query
            created_reading_iris - list of newly created quantity IRIs
            dataIRIs, dataClasses, timeUnit - to be appended to input arguments
                                              to TimSeriesClient bulkInit call
    """

    if readings_iris and (len(readings) != len(readings_iris)):
        raise ValueError("Length or readings and readings_iris does not match.")

    # Initialise "creation" time for forecasts
    t = dt.datetime.utcnow().strftime('%Y-%m-%dT%H:00:00Z')

    # Initialise return values : triples for INSERT DATA query & input lists
    # for bulkInit function using TimeSeriesClient
    triples = ''
    dataIRIs = []
    dataClasses = []
    timeUnit = TIME_FORMAT
    # List for all created station readings IRIs
    created_reading_iris = []

    # Get concepts and create IRIs
    for i in range(len(readings)):
        r = readings[i]
        # Create IRI for reported quantity
        quantity_type = PREFIXES['ems'] + r
        if not readings_iris:
            quantity_iri = PREFIXES['kb'] + r + '_' + str(uuid.uuid4())
            created_reading_iris.append(quantity_iri)
        else:
            quantity_iri = readings_iris[i]
        # Create Measure / Forecast IRI
        if is_observation:
            data_iri = PREFIXES['kb'] + 'Measure_' + str(uuid.uuid4())
            data_iri_type = PREFIXES['om'] + 'Measure'
            creation_time = None
        else:
            data_iri = PREFIXES['kb'] + 'Forecast_' + str(uuid.uuid4())
            data_iri_type = PREFIXES['ems'] + 'Forecast'
            creation_time = t

        unit = UNITS_MAPPING[r][0]
        symbol = UNITS_MAPPING[r][1]

        # Add triples to instantiate
        comment = quantity_comments[i] if quantity_comments else None
        triples += add_om_quantity(station_iri, quantity_iri, quantity_type,
                                  data_iri, data_iri_type, unit, symbol,
                                  is_observation, creation_time=creation_time, 
                                  comment=comment)

        # Get data to bulkInit time series
        dataIRIs.append(data_iri)
        dataClasses.append(DATACLASS)

    return triples, created_reading_iris, dataIRIs, dataClasses, timeUnit


def condition_readings_data(readings_data: list, only_keys: bool = True) -> dict:
    """
        Condition retrieved MetOffer readings as required for query template

        Arguments:
            readings_data - readings data list as returned by MetOffer
            only_keys - boolean flag indicating whether only the keys (i.e. variable
                        names) shall be retrieved or also time series data
    """
    
    # Read all unique measurement variables
    read_variables = [v for data in readings_data for v in list(data.keys())]
    read_variables = list(set(read_variables))
    relevant_variables = list(set(read_variables) & set(READINGS_MAPPING.keys()))
    # Initialise dict of conditioned readings
    conditioned = dict(zip(relevant_variables, [None]*len(relevant_variables)))

    if not only_keys:
        # Read reported times and values for variables
        for read_var in relevant_variables:
            if read_var == 'timestamp':
                conditioned[read_var] = [dt.datetime.strftime(r[read_var][0], TIME_FORMAT) for r in readings_data]
            elif read_var == 'Wind Direction':
                conditioned[read_var] = [float(COMPASS[r[read_var][0]]) for r in readings_data]
            else:
                conditioned[read_var] = [float(r[read_var][0]) for r in readings_data]

    return conditioned


def retrieve_readings_concepts_for_station(metclient, station_id: str, 
                                          observations: bool = True,
                                          forecasts: bool = True):
    """
        Retrieve station readings via Metoffer client
    """

    available_obs = []
    available_fcs = []

    if observations:
        # Load OBSERVATIONS data for that station
        try:
            obs = metclient.loc_observations(station_id)
            observation = metoffer.Weather(obs)
            available_obs = condition_readings_data(observation.data)
            available_obs = list(available_obs.keys())
            available_obs = [READINGS_MAPPING[i] for i in available_obs]
        except:
            print('Error while retrieving observation for station ID: {:>10}'.format(station_id))
            #logger.warning('Error while retrieving observation for station ID: {:>10}'.format(id))

    if forecasts:
        # Load 3-hourly FORECAST data for that station
        try:
            fc = metclient.loc_forecast(station_id, metoffer.THREE_HOURLY)
            forecast = metoffer.Weather(fc)
            available_fcs = condition_readings_data(forecast.data)
            available_fcs = list(available_fcs.keys())
            available_fcs = [READINGS_MAPPING[i] for i in available_fcs]
        except:
            print('Error while retrieving forecast for station ID: {:>10}'.format(station_id))
            #logger.warning('Error while retrieving forecast for station ID: {:>10}'.format(id))

    return available_obs, available_fcs


if __name__ == '__main__':

    print(dt.datetime.now().strftime('%Y-%m-%dT%H:%M'))
    instantiate_all_station_readings()
    print(dt.datetime.now().strftime('%Y-%m-%dT%H:%M'))
