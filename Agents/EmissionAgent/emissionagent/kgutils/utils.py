################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 03 Aug 2023                            #
################################################

# The purpose of this module is to provide frequently used utility functions
# for knowledge graph interactions

import pandas as pd
from dateutil.parser import isoparse
from distutils.util import strtobool

from py4jps import agentlogging

from emissionagent.datamodel import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def remove_unnecessary_whitespace(query: str) -> str:
    """
    Remove unnecessary whitespaces
    """
    query = ' '.join(query.split())
    return query


def get_list_of_unique_values(res: list, key: str) -> list:
    """
    Unpacks a query result list (i.e., list of dicts) into a list of 
    unique values for the given dict key.
    """    
    res_list =  list(set([r.get(key) for r in res]))
    res_list = [v for v in res_list if v is not None]
    return res_list


def get_unique_value(res: list, key: str, cast_to=None) -> str:
    """
    Unpacks a query result list (i.e., list of dicts) into unique 
    value for the given dict key (returns None if no unique value is found)

    Tries to cast result in case 'cast_to' datatype is provided
    """
    
    res_list =  get_list_of_unique_values(res, key)
    if len(res_list) == 1:
        # Try to cast retrieved value to target type (throws error if not possible)
        if cast_to and issubclass(cast_to, bool):
            res_list[0] = bool(strtobool(res_list[0]))
        elif cast_to and issubclass(cast_to, (int, float)):
            res_list[0] = cast_to(res_list[0])
        return res_list[0]
    else:
        if len(res_list) == 0:
            msg = f"No value found for key: {key}."
        else:
            msg = f"Multiple values found for key: {key}."
        logger.warning(msg)
        return None


def convert_time_to_timestamp(time):
    """
    Converts a time to a unix timestamp (in seconds)

    Arguments:
        time: a time (int, str, pd.Timestamp)
    Returns:
        unix timestamp (in s)
    """

    # convert time to unix timestamp
    if isinstance(time, int):
        time_stamp = time
    elif isinstance(time, str):
        time_stamp = pd.Timestamp(
            isoparse(time)).tz_convert('UTC').tz_localize(None).timestamp()
    elif isinstance(time, pd.Timestamp):
        time_stamp = time.timestamp()
    else:
        raise ValueError(
            f'Unknown time format: {time}. Please use int, str or pd.Timestamp')
    return int(time_stamp)