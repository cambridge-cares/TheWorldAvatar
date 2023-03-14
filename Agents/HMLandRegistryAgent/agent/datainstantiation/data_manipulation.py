################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 13 Mar 2023                            #
################################################

# The purpose of this module is to provide functionality to manipulate instantiated 
# data, primarily to trigger new derivation cascade for visualisation purposes


from agent.errorhandling.exceptions import TSException
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.utils.stack_configs import QUERY_ENDPOINT

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def scale_property_price_index(ppi_iri, scaler, kg_client=None, 
                               query_endpoint=QUERY_ENDPOINT):
    """
    Retrieve time series data for instantiated property price index and scale it
    by a given factor.

    Arguments:
        ppi_iri (str): IRI of property price index to be scaled
        scaler (float): Factor by which to scale time series data
    """
    
    # Initialise KG client (no update required!)
    if not kg_client:
        kg_client = KGClient(query_endpoint, query_endpoint)
    # Initialise TS client
    ts_client = TSClient(kg_client=kg_client)

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

    # 2) Scale time series data
    scaled_values = [float(v) * scaler for v in values]

    # 3) Update/overwrite time series data in RDB
    ts = TSClient.create_timeseries(dates, [ppi_iri], [scaled_values])
    try:
        with ts_client.connect() as conn:
            ts_client.tsclient.addTimeSeriesData(ts, conn)
    except Exception as ex:
        logger.error('Error adding time series data: {}'.format(ex))
        raise TSException('Error adding time series data') from ex
