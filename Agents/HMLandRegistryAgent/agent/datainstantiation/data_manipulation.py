################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 13 Mar 2023                            #
################################################

# The purpose of this module is to provide functionality to manipulate instantiated 
# data, primarily to trigger new derivation cascade for visualisation purposes


from agent.datamodel.iris import *
from agent.errorhandling.exceptions import InvalidInput, TSException
from agent.kgutils.tsclient import TSClient
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.datainstantiation.derivation_markup import retrieve_derivation_instances

from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def scale_property_price_index(ppi_iri: str, scaler: float, timesteps: int = 1,
                               kg_client=None, query_endpoint=QUERY_ENDPOINT,
                               update_endpoint=UPDATE_ENDPOINT):
    """
    Retrieve time series data for instantiated property price index and scale it
    by a given factor.

    Arguments:
        ppi_iri (str): IRI of property price index to be scaled
        timesteps (int): Number of most recent time steps to scale
        scaler (float): Factor by which to scale time series data
    """
    
    # Initialise KG Client with PySparqlClient instance (no update required!)
    if not kg_client:
        kg_client = PySparqlClient(query_endpoint=query_endpoint,
                                   update_endpoint=query_endpoint)
    # Initialise TS client
    ts_client = TSClient(kg_client=kg_client)
    # Create PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=query_endpoint, update_endpoint=update_endpoint)

    # 1) Retrieve instantiated time series data for property price index
    try:
        # Retrieve time series in try-with-resources block to ensure closure of RDB connection
        with ts_client.connect() as conn:
            ts = ts_client.tsclient.getTimeSeries([ppi_iri], conn)
        # Parse data as Series (i.e. unwrap Java data types)
        dates = [d.toString() for d in ts.getTimes()]
        values = [v for v in ts.getValues(ppi_iri)]
    except Exception as ex:
        logger.error('Error retrieving/unwrapping Property Price Index time series: {}'.format(ex))
        raise TSException('Error retrieving/unwrapping Property Price Index time series') from ex

    # 2) Scale specified number of most recent time steps
    if len(values) < timesteps:
        raise InvalidInput('Number of time steps to scale exceeds number of available time steps')
    else:
        scaled_values = [float(v) for v in values[:-timesteps]]
        scaled_values.extend([round(float(v) * scaler, 2) for v in values[-timesteps:]])

    # 3) Update/overwrite time series data in RDB
    ts = TSClient.create_timeseries(dates, [ppi_iri], [scaled_values])
    try:
        with ts_client.connect() as conn:
            ts_client.tsclient.addTimeSeriesData(ts, conn)
    except Exception as ex:
        logger.error('Error adding time series data: {}'.format(ex))
        raise TSException('Error adding time series data') from ex

    # 4) Trigger new derivation cascade
    # Update timestamp of pure input
    derivation_client.updateTimestamp(ppi_iri)
    # Request for derivation update
    avg_derivations, value_derivations = retrieve_derivation_instances(ppi_iri, 
                                                                       sparql_client=kg_client)
    # 1) Average Square Metre Price per Postal Code
    for d in avg_derivations:
        # Request derivation update for immediate execution (i.e. as synchronous call)
        derivation_client.unifiedUpdateDerivation(d)
        # Only mark derivation as requested to be executed with next asynchronous call
        #derivation_client.derivation_client.updateMixedAsyncDerivation(d)
    # 2) Property Value Estimation