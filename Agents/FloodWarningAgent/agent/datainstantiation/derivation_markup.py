################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) # 
#          Jiaru Bai                           #   
# Date: 04 Mar 2023                            #
################################################

# This module provides functionality to create derivation markup for newly 
# instantiated flood warnings/alert to be picked up by the flood assessment agent

from typing import Any, Dict, List

from agent.datamodel import *
from agent.utils.env_configs import FLOOD_ASSESSMENT_AGENT_IRI

from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


def retrieve_flood_assessment_derivation_iri(
    sparql_client: PySparqlClient,
    flood_warning_iri: str,
    flood_assessment_agent_iri: str,
):
    query = f"""
            SELECT DISTINCT ?flood_assessment_derivation
            WHERE {{
                ?flood_assessment_derivation <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> <{flood_warning_iri}>.
                ?flood_assessment_derivation <{pda_iris.ONTODERIVATION_ISDERIVEDUSING}> <{flood_assessment_agent_iri}>.
            }}"""
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve flood assessment derivation iri: {query}")
    response = sparql_client.performQuery(query)
    logger.info(f"Response: {response}")
    if len(response) == 0:
        return None
    else:
        return response[0].get('flood_assessment_derivation')


def retrieve_affected_property_info(sparql_client: PySparqlClient, affected_property_iris: list):
    # Construct query to retrieve below information for each affected property from KG
    # NOTE for this iteration, we assume that the property value estimation is already computed 
    #      when the derivation is created
    # - market value (mv) IRI
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?property ?mv
            WHERE {{
                VALUES ?property {{ <{'> <'.join(affected_property_iris)}> }}
                OPTIONAL {{ ?property <{OBE_HASMARKETVALUE}> ?mv. }}
            }}"""

    # Remove empty enclosing brackets (only present in case of empty property list)
    query = query.replace('<>', '')
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve property info: {query}")
    response = sparql_client.performQuery(query)
    logger.info(f"Length of response: {len(response)}")

    # Rearrange response into a dictionary where the information for each property are grouped together
    # Target format of the dictionary:
    # {
    #     "property": {
    #         'mv': iri,
    #     },
    #     ... (other properties)
    # }
    property_info_dct = {}
    property_lst = get_unique_values_in_list_of_dict(response, 'property')
    for property_iri in property_lst:
        property_info_dct[property_iri] = {}
        sub_response = get_sublist_in_list_of_dict_matching_key_value(response, 'property', property_iri)
        # it will be assigned as None if the key is not found
        property_info_dct[property_iri]['mv'] = get_the_unique_value_in_list_of_dict(sub_response, 'mv')

    return property_info_dct


def flood_assessment_derivation_markup(
    derivation_client: PyDerivationClient,
    flood_assessment_derivation_iri: str = None,
    flood_warning_iri: str = None,
    affected_building_iris: List[str] = [],
    property_value_iris: List[str] = []
):
    if flood_assessment_derivation_iri is None:
        try:
            # Create async derivation for new info to get flood assessment computed
            input_lst = [flood_warning_iri] + affected_building_iris + property_value_iris
            derivation_iri = derivation_client.createAsyncDerivationForNewInfo(
                agentIRI=FLOOD_ASSESSMENT_AGENT_IRI,
                inputsAndDerivations=input_lst,
            )
            logger.info(f"Created Async derivation for new info: {derivation_iri}, which will be computed shortly")
        except Exception as e:
            logger.error(f"Failed to create async derivation for new info, inputsAndDerivations: {str(input_lst)}")
            raise e
    else:
        # Request for derivation update if the flood warning derivation is already instantiated
        derivation_client.unifiedUpdateDerivation(flood_assessment_derivation_iri)
        logger.info(f"Requested for derivation update: {flood_assessment_derivation_iri}, should be done shortly")


# Utility functions

def check_if_key_in_list_of_dict(list_of_dict: List[dict], key: str):
    for d in list_of_dict:
        if key in d:
            return True
    return False


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
    

def get_unique_values_in_list_of_dict(list_of_dict: List[dict], key: str) -> list:
    return list(set(get_value_from_list_of_dict(list_of_dict, key)))


def get_the_unique_value_in_list_of_dict(list_of_dict: List[dict], key: str) -> Any:
    if not check_if_key_in_list_of_dict(list_of_dict, key):
        return None
    list_unique_value = list(set(get_value_from_list_of_dict(list_of_dict, key)))
    if len(list_unique_value) != 1:
        raise Exception(f"""Exactly one '{key}' is expected, but found: {list_unique_value} in: {list_of_dict}""")
    return list_unique_value[0]
