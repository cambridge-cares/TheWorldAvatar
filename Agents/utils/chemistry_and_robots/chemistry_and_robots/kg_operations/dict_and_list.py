#######################################################
## Some utility functions handling the list and dict ##
#######################################################
from typing import Any, Dict, List
from collections.abc import Mapping

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


def get_sublist_in_list_of_dict_matching_key_value(list_of_dict: List[Dict], key: str, value: Any) -> list:
    if len(list_of_dict) > 0:
        try:
            sublist = [d for d in list_of_dict if d[key] == value]
        except KeyError:
            logger.error("Key '%s' is not found in the given list of dict: %s" % (key, str(list_of_dict)))
        else:
            return sublist
    else:
        logger.error("An empty list is passed in while requesting return sublist given key '%s'." % (key))
        return []

def get_unique_values_in_list_of_dict(list_of_dict: List[dict], key: str) -> list:
    return list(set(get_value_from_list_of_dict(list_of_dict, key)))

def get_the_unique_value_in_list_of_dict(list_of_dict: List[dict], key: str) -> Any:
    if not check_if_key_in_list_of_dict(list_of_dict, key):
        return None
    list_unique_value = list(set(get_value_from_list_of_dict(list_of_dict, key)))
    if len(list_unique_value) != 1:
        raise Exception(f"""Exactly one '{key}' is expected, but found: {list_unique_value} in: {list_of_dict}""")
    return list_unique_value[0]

def get_value_from_list_of_dict(list_of_dict: List[dict], key: str) -> list:
    if len(list_of_dict) > 0:
        try:
            list_of_values = [d[key] for d in list_of_dict if key in d]
        except KeyError:
            logger.error("Key '%s' is not found in the given list of dict: %s" % (key, str(list_of_dict)))
            return []
        else:
            return list_of_values
    else:
        logger.error("An empty list is passed in while requesting return value of key '%s'." % (key))
        return []

def keep_wanted_keys_from_list_of_dict(list_of_dict: List[dict], wanted_keys: List[str]) -> list:
    return_list = []
    for one_dict in list_of_dict:
        return_list.append({key:one_dict[key] for key in wanted_keys})
    return return_list

def remove_unwanted_keys_from_list_of_dict(list_of_dict: List[dict], unwanted_keys: List[str]) -> list:
    return [_d for _d in [{key:d[key] for key in d if key not in unwanted_keys} for d in list_of_dict] if _d != {}]

def remove_duplicate_dict_from_list_of_dict(list_of_dict: List[dict]) -> list:
    return [dict(t) for t in {tuple(sorted(d.items())) for d in list_of_dict}]

def check_if_key_in_list_of_dict(list_of_dict: List[dict], key: str):
    for d in list_of_dict:
        if key in d:
            return True
    return False

def check_if_two_lists_equal(list_a: list, list_b: list) -> bool:
    if list_a is None and list_b is None:
        return True
    if list_a is None or list_b is None:
        return False
    if len(list_a) != len(list_b):
        return False
    return list_a.sort() == list_b.sort()

# see https://stackoverflow.com/a/63543967
def deep_update(d1, d2):
    if all((isinstance(d, Mapping) for d in (d1, d2))):
        for k, v in d2.items():
            d1[k] = deep_update(d1.get(k), v)
        return d1
    return d2
