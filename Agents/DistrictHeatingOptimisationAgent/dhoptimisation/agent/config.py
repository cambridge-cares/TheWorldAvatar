################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 29 Sep 2023                            #
################################################

# The purpose of this module is to provide some overarching configurations and
# settings for the agent

from dhoptimisation.utils import logger
from dhoptimisation.utils.env_configs import DB_URL
from dhoptimisation.utils.baselib_gateway import jpsBaseLibGW


#
# Define some default values
#
# Time series value data types (default: Double)
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DOUBLE = jpsBaseLibView.java.lang.Double.TYPE
INTEGER = jpsBaseLibView.java.lang.Integer.TYPE
BOOLEAN = jpsBaseLibView.java.lang.Boolean.TYPE
# Time series format
TIME_FORMAT = "%Y-%m-%dT%H:%M:%SZ"

# Heat generation/sourcing history length to retrieve, i.e., to assess active
# heat generators in previous time step and gas turbine idle time
HIST_LENGTH = 12    # hours


def get_rdb_endpoint(ts_details:dict):
    """
    Determine rdb url and time format to use when retrieving/creating time series

    Arguments:
        ts_details {dict} -- time series details as retrieved from the KG
    
    Returns:
        str -- RDB URL to use when querying/creating time series
        str -- TIME FORMAT to use when parsing queried time series
    """

    # Retrieve RDB URL to use with following priority:
    # 1) Use URL instantiated in KG for time series to forecast
    # 2) Use default URL from environment variables
    rdb_url = ts_details.get('rdb_url')
    if not rdb_url:
        logger.warning('No RDB URL associated with retrieved time series in KG. Using default from environment variables.')
        rdb_url = DB_URL

    # Throw exception if RDB URL is not set
    if not rdb_url:
        msg = 'RDB URL to use could not be determined: neither instantiated nor provided in docker-compose.yml.'
        logger.error(msg)
        raise ValueError(msg)
    else:
        # Replace potentially occurring 'localhost' (i.e., depending on TimeSeriesClient setting
        # when time series was instantiated) with 'host.docker.internal' for Docker deployment
        # NOTE: This is a workaround for the fact that the Docker container cannot access the
        #       local host (i.e., the host machine) via 'localhost' --> likely this will never be
        #       reached, as this entire use case is designed to be deployed within a single stack
        rdb_url = rdb_url.replace('localhost', 'host.docker.internal')

    # Retrieve TIME FORMAT to use with same priority
    time_format = ts_details.get('time_format')
    if not time_format:
        logger.warning(f'No time format associated with retrieved time series in KG. Using default format: {TIME_FORMAT}.')
        time_format = TIME_FORMAT
    
    # Ensure proper Python syntax of retrieved time format 
    # (i.e., to avoid issues when parsing retrieved timestamps)
    # NOTE: Instantiated time format in KG (via TimeSeriesClient) is likely either
    #       already in Python syntax or in (some sort of) ISO 8601 format
    iso_to_python_format = {
        "YYYY-MM-DD": "%Y-%m-%d",
        "YYYY-MM-DDTHH:MM:SSZ": "%Y-%m-%dT%H:%M:%SZ",
        "YYYYMMDDTHHMMSSZ": "%Y%m%dT%H%M%SZ",
        "YYYY-MM-DDTHH:MM:SS.SSSSSSZ": "%Y-%m-%dT%H:%M:%S.%fZ",
    }
    if time_format not in iso_to_python_format.values():
        # Retrieve mapped Python compliant format; return itself if not mapped
        time_format = iso_to_python_format.get(time_format.upper(), time_format)
    if '%' not in time_format:
        msg = 'Non-compliant time format retrieved from KG.'
        logger.error(msg)
        raise ValueError(msg)
    
    return rdb_url, time_format
