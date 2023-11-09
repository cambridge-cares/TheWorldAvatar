# The purpose of this module is to provide some common utility methods

import re
    
from py4jps import agentlogging

from dhoptimisation.entry_point import logger_level

# Initialise aligned logger instance (ensure consistent logger level throughout!)
logger = agentlogging.get_logger(logger_level)


def raise_error(errortype:Exception, msg):
    logger.error(msg)
    raise errortype(msg)


def extend_setup_dictionary(setup_dictionary: dict, new_entries: dict):
    # Add new keys with empty list as initial value
    new_keys = {key: [] for key in new_entries if key not in setup_dictionary}
    setup_dictionary.update(new_keys)
    # Add values 
    for key, value in new_entries.items():
        setup_dictionary[key].append(value)
    return setup_dictionary


def extract_iris_from_setup_dict(setup_dict: dict):
    # Extracts all IRIs from optimisation setup dict
    url_pattern = re.compile(r"^(http|https)://[a-zA-Z0-9.-]+")
    iris = []
    for v in setup_dict.values():
        if isinstance(v, list):
            for e in v:
                if isinstance(e, str) and url_pattern.match(e):
                    iris.append(e)
        else:
            if isinstance(v, str) and url_pattern.match(v):
                iris.append(v)
    return iris
