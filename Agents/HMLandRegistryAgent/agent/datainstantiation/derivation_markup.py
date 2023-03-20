################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) # 
#          Jiaru Bai                           #   
# Date: 16 Mar 2023                            #
################################################

# This module provides functionality to create derivation markup for newly 
# instantiated flood warnings/alert to be picked up by the flood assessment agent

from typing import Any, Dict, List

from agent.datamodel import *

from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# =============================================================================
# Average Square Metre Price Derivation Markup (per PostalCode)

def retrieve_postal_code_info(sparql_client: PySparqlClient):
    # Construct query to retrieve below information for each postal code from KG
    # NOTE the query is designed to also retrieve postal codes with currently no 
    #      property in them (in such cases, the AvgSqmPrice agent will retrieve
    #      property sales transactions from nearby postcodes from Land Registry API)
    # - postal code (postal) IRI
    # - property price index (ppi) IRI
    # - transaction record (tx) IRI (if available) - to accommodate for the case 
    #   where there is no tx for a postal code but buildings in the postal code 
    #   are affected by a flood
    # Also, the query retrieve below information for each postal code if available
    # - average price per square meter (asp) IRI
    # - average price per square meter derivarion (derivation)
    # - transaction record used to calculate the existing asp (deriv_tx)
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?postal ?ppi ?tx ?asp ?derivation ?deriv_tx
            WHERE {{
                ?postal rdf:type <{OBE_POSTALCODE}>.
                ?address <{OBE_HAS_POSTALCODE}> ?postal;
                         <{OBE_HAS_ADMIN_DISTRICT}> ?district.
                ?ppi <{OBE_REPRESENTATIVE_FOR}> ?district.
                OPTIONAL {{ ?property <{OBE_HAS_ADDRESS}> ?address ; 
                                      rdf:type/rdfs:subClassOf* <{OBE_PROPERTY}>.
                    OPTIONAL {{ ?property <{OBE_HAS_LATEST_TRANSACTION}> ?tx. }}
                }}
                OPTIONAL {{
                    ?asp <{OBE_REPRESENTATIVE_FOR}> ?postal.
                    ?asp <{pda_iris.ONTODERIVATION_BELONGSTO}> ?derivation.
                    ?derivation <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?deriv_tx.
                    ?deriv_tx rdf:type <{LRPPI_TRANSACTION_RECORD}>.
                }}
            }}"""
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve postal code info: {query}")
    response = sparql_client.performQuery(query)

    # Rearrange response into a list of dictionaries where the information for each 
    # postal code are grouped together:
    # Target format of list of dictionaries:
    # [
    #     { # Postal code 1
    #         'postal_code': iri,
    #         'tx': [iri, iri, ...],
    #         'ppi': iri,
    #         'asp': iri,
    #         'derivation': iri,
    #         'deriv_tx': [iri, iri, ...]
    #     },
    #     ... # Postal code 2, 3, ...
    # ]
    # NOTE As tx/asp/derivation/deriv_tx are optional, they may not be presented in the returned list of dicts
    postal_code_info_lst = []
    postal_code_lst = get_unique_values_in_list_of_dict(response, 'postal')
    for postal_code in postal_code_lst:
        postal_code_info = {}
        postal_code_info['postal_code'] = postal_code
        sub_response = get_sublist_in_list_of_dict_matching_key_value(response, 'postal', postal_code)
        postal_code_info['ppi'] = get_the_unique_value_in_list_of_dict(sub_response, 'ppi')
        # 'tx' returned value will be an empty list if no tx is found
        postal_code_info['tx'] = get_unique_values_in_list_of_dict(sub_response, 'tx')
        # The function get_the_unique_value_in_list_of_dict returns None if key `asp` does not exist
        asp = get_the_unique_value_in_list_of_dict(sub_response, 'asp')
        if asp is not None:
            postal_code_info['asp'] = asp
            postal_code_info['derivation'] = get_the_unique_value_in_list_of_dict(sub_response, 'derivation')
            postal_code_info['deriv_tx'] = get_unique_values_in_list_of_dict(sub_response, 'deriv_tx')
        postal_code_info_lst.append(postal_code_info)

    return postal_code_info_lst


def avg_sqm_price_derivation_markup(
    derivation_client: PyDerivationClient,
    sparql_client: PySparqlClient,
    postal_code_iri: str,
    property_price_index_iri: str,
    transaction_record_iri_lst: List[str],
    existing_avg_sqm_price_iri: str = None,
    existing_asp_derivation_iri: str = None,
    existing_asp_derivation_tx_iri_lst: List[str] = None,
):
    if existing_avg_sqm_price_iri is None:
        try:
            # TODO [when turning this script into an agent]
            # It has been observed that the function call createSyncDerivationForNewInfo sometimes fails with the error:
            # ERROR - HTTPConnectionPool(host='statistics.data.gov.uk', port=80): Max retries exceeded
            #   (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x7f5d38710a60>: Failed to establish a new connection: [Errno 111] Connection refused'))
            # Given the current design, the derivation will not be created if the above error occurs
            # i.e. from the KG point of view, nothing happened
            # One might want to consider retrying the function call if the same error occurs
            # Either here or in the AverageSquareMetrePriceAgent where the HTTP request is made (the latter is preferred)

            # Create sync derivation for new info to get avg_sqm_price computed
            # NOTE Here we use the function call createSyncDerivationForNewInfoWithHttpUrl instead of createSyncDerivationForNewInfo
            # This is to workaround the fact that the hasHttpUrl for the agent operation is stored with host.docker.internal
            #   which is not accessible from the host machine, hence we converted it to localhost and manually pass it in
            # TODO [when turning this script into an agent] keep host.docker.internal or let stack manager to take care of the routing
            derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                agentIRI=AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI,
                agentURL=AVERAGE_SQUARE_METRE_PRICE_AGENT_URL,
                inputsIRI=[postal_code_iri, property_price_index_iri] + transaction_record_iri_lst,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
            logger.info(f"Created sync derivation for new info: {derivation.getIri()}")
            logger.info(f"Created OBE_AVERAGE_SM_PRICE: {derivation.getBelongsToIris(iris.OBE_AVERAGE_SM_PRICE)}")
        except Exception as e:
            logger.error(f"Failed to create sync derivation for new info, inputsIRI: {str([postal_code_iri, property_price_index_iri] + transaction_record_iri_lst)}")
            raise e
    else:
        # Construct a list of tx iri that are not used to calculate the existing avg sqm price
        new_tx_iri_lst = [iri for iri in transaction_record_iri_lst if iri not in existing_asp_derivation_tx_iri_lst]
        # Add the new tx iri to the existing derivation and request for update
        if bool(new_tx_iri_lst):
            # NOTE this block of code is never reached in the derivation mvp as we only changed PropertyPriceIndex
            # TODO [when turning this script into an agent] test this block of code
            # Add timestamp to new tx iri (nothing happens if the iri already has timestamp)
            derivation_client.addTimeInstanceCurrentTimestamp(new_tx_iri_lst)
            # Add new tx iri to the existing derivation
            sparql_client.performUpdate(
                f"""
                INSERT DATA {{
                    VALUES ?tx {{ {' '.join([f'<{iri}>' for iri in new_tx_iri_lst])} }}
                    <{existing_asp_derivation_iri}> <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?tx.
                }}"""
            )
            # Request for derivation update
            derivation_client.unifiedUpdateDerivation(existing_asp_derivation_iri)
            logger.info(f"Added new tx iri ({new_tx_iri_lst}) to existing derivation: {existing_asp_derivation_iri}")
        else:
            logger.info(f"No new tx iri to add to existing derivation: {existing_asp_derivation_iri}")


# =============================================================================
# Property Market Value Derivation Markup (per Property)


# =============================================================================
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
