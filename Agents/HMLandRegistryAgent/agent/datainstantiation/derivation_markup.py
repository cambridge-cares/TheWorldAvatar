################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) # 
#          Jiaru Bai                           #   
# Date: 16 Mar 2023                            #
################################################

# This module provides functionality to create derivation markup for
# 1) average price per square meter (asp) of a postal code
# 2) property market value estimate of a property

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

def retrieve_avgsqmprice_postal_code_info(sparql_client: PySparqlClient,
                                          postcodes: List[str] = None):
    """
    Construct query to retrieve below information for postcodes of interest/
    each instantiated postcode from KG

    Arguments:
        postcodes: list of postcode strings (e.g. ['CB1 1AA', 'CB1 1AB'])

    NOTE the query is designed to also retrieve postal codes with currently no 
        property in them (in such cases, the AvgSqmPrice agent will retrieve
        property sales transactions from nearby postcodes from Land Registry API)
    - postal code (postal) IRI
    - property price index (ppi) IRI
    - transaction record (tx) IRI (if available) - to accommodate for the case 
    where there is no tx for a postal code but buildings in the postal code 
    are affected by a flood
    Also, the query retrieve below information for each postal code if available
    - average price per square meter (asp) IRI
    - average price per square meter derivarion (derivation)
    - transaction record used to calculate the existing asp (deriv_tx)
    """

    if postcodes is not None:
        # If list of postcodes provided, retrieve information for those postcodes only ...
        value_statement = f"""VALUES ?postal_string {{ {' '.join([f'"{pc}"' for pc in postcodes])} }} .
                              ?postal <{RDFS_LABEL}> ?postal_string .
        """
    else:
        # ... otherwise, retrieve information for all postcodes
        value_statement = ""
    
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?postal ?ppi ?tx ?asp ?derivation ?deriv_tx
            WHERE {{
                {value_statement}
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
                    OPTIONAL {{
                        ?derivation <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?deriv_tx.
                        ?deriv_tx rdf:type <{LRPPI_TRANSACTION_RECORD}>.
                    }}
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
            # Create sync derivation for new info to get average square metre price computed
            # NOTE createSyncDerivationForNewInfo assumes derivation agent to listen 
            #      on URL containing host.docker.internal
            #      For other URLs, e.g. localhost, use createSyncDerivationForNewInfoWithHttpUrl

            #derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
            derivation = derivation_client.createSyncDerivationForNewInfo(
                agentIRI=AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI,
                # NOTE only relevant for createSyncDerivationForNewInfoWithHttpUrl
                #agentURL=AVERAGE_SQUARE_METRE_PRICE_AGENT_URL,
                inputsIRI=[postal_code_iri, property_price_index_iri] + transaction_record_iri_lst,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
            logger.info(f"Created sync derivation for new info: {derivation.getIri()}")
            logger.info(f"Created OBE_AVERAGE_SM_PRICE: {derivation.getBelongsToIris(OBE_AVERAGE_SM_PRICE)}")
        except Exception as e:
            logger.error(f"Failed to create sync derivation for new info, inputsIRI: {str([postal_code_iri, property_price_index_iri] + transaction_record_iri_lst)}")
            raise e
    else:
        # Construct a list of tx iris that are not used to calculate the existing avg sqm price
        new_tx_iri_lst = [iri for iri in transaction_record_iri_lst if iri not in existing_asp_derivation_tx_iri_lst]
        # Add the new tx iri to the existing derivation and request for update
        if bool(new_tx_iri_lst):
            # Add timestamp to new tx iri (nothing happens if the iri already has timestamp)
            derivation_client.addTimeInstanceCurrentTimestamp(new_tx_iri_lst)
            # Add new tx iri to the existing derivation
            sparql_client.performUpdate(
                f"""
                INSERT {{
                    <{existing_asp_derivation_iri}> <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?tx .
                }}
                WHERE {{
                    VALUES ?tx {{ {' '.join([f'<{iri}>' for iri in new_tx_iri_lst])} }}
                }}"""
            )
            logger.info(f"Added new tx iri ({new_tx_iri_lst}) to existing derivation: {existing_asp_derivation_iri}")
        else:
            logger.info(f"No new tx iri to add to existing derivation: {existing_asp_derivation_iri}")
        
        # Request for derivation update: 2 potential options
        # (as pure inputs (especially Property Price Index) have likely changed)
        # 1) Request derivation update for immediate execution (i.e. as synchronous call)
        derivation_client.unifiedUpdateDerivation(existing_asp_derivation_iri)
        # 2) Only mark derivation as requested to be executed with next asynchronous call
        #derivation_client.derivation_client.updateMixedAsyncDerivation(existing_asp_derivation_iri)


# =============================================================================
# Property Market Value Derivation Markup (per Property)

def retrieve_marketvalue_property_info(sparql_client: PySparqlClient,
                                       property_iris: List[str] = None):
    """
    Construct query to retrieve below information for properties of interest/
    each instantiated property from KG

    Arguments:
        property_iris: list of property IRIs for which to retrieve data

    - property price index (ppi) IRI
    - floor area (area) IRI
    - transaction record (tx) IRI (if available)
    - average sqm price (asp) IRI (if available)
    NOTE for this iteration, it's assumed that the average square metre price is already
         computed when the derivation is created (i.e. created as synchronous derivation)
    Also query if the market value is already computed
    - market value (mv) IRI (if available)
    - market value derivation IRI (if available)
    - market value derivation isDerivedFrom existing transaction record (if available)
    """
    
    if property_iris is not None:
        # If list of property IRIs provided, retrieve information for those properties only ...
        value_statement = f"VALUES ?property {{ {' '.join([f'<{prop}>' for prop in property_iris])} }} "
    else:
        # ... otherwise, retrieve information for all postcodes
        value_statement = ""
    
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?property ?ppi ?area ?tx ?asp ?mv ?derivation ?deriv_tx
            WHERE {{
                {value_statement}
                ?property rdf:type/rdfs:subClassOf* <{OBE_PROPERTY}>.
                ?property <{OBE_HAS_ADDRESS}> ?address.
                ?address <{OBE_HAS_ADMIN_DISTRICT}> ?district.
                ?ppi <{OBE_REPRESENTATIVE_FOR}> ?district.
                ?property <{OBE_HAS_TOTAL_FLOORAREA}> ?area.
                OPTIONAL {{
                    ?property <{OBE_HAS_LATEST_TRANSACTION}> ?tx.
                }}
                OPTIONAL {{
                    ?address <{OBE_HAS_POSTALCODE}> ?postal.
                    ?asp <{OBE_REPRESENTATIVE_FOR}> ?postal.
                    ?asp rdf:type <{OBE_AVERAGE_SM_PRICE}>.
                }}
                OPTIONAL {{
                    ?property <{OBE_HASMARKETVALUE}> ?mv.
                    ?mv <{pda_iris.ONTODERIVATION_BELONGSTO}> ?derivation.
                    OPTIONAL {{
                        ?derivation <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?deriv_tx.
                        ?deriv_tx rdf:type <{LRPPI_TRANSACTION_RECORD}>.
                    }}
                }}
            }}"""

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve property info: {query}")
    response = sparql_client.performQuery(query)
    logger.info(f"Length of response: {len(response)}")

    # Rearrange response into a dictionary where the information for each property are grouped together
    # Target format of the dictionary:
    # {
    #     "property": {
    #         'ppi': iri,
    #         'area': iri,
    #         'tx': iri,
    #         'asp': iri,
    #     },
    #     ... (other properties)
    # }
    # NOTE As tx/asp are optional, they may not be presented in the returned dictionary
    property_info_dct = {}
    property_lst = get_unique_values_in_list_of_dict(response, 'property')
    for property_iri in property_lst:
        property_info_dct[property_iri] = {}
        sub_response = get_sublist_in_list_of_dict_matching_key_value(response, 'property', property_iri)
        # it will be assigned as None if the key is not found, and it will raise an error if there are more than one
        property_info_dct[property_iri]['ppi'] = get_the_unique_value_in_list_of_dict(sub_response, 'ppi')
        property_info_dct[property_iri]['area'] = get_the_unique_value_in_list_of_dict(sub_response, 'area')
        property_info_dct[property_iri]['tx'] = get_the_unique_value_in_list_of_dict(sub_response, 'tx')
        property_info_dct[property_iri]['asp'] = get_the_unique_value_in_list_of_dict(sub_response, 'asp')
        property_info_dct[property_iri]['mv'] = get_the_unique_value_in_list_of_dict(sub_response, 'mv')
        property_info_dct[property_iri]['derivation'] = get_the_unique_value_in_list_of_dict(sub_response, 'derivation')
        property_info_dct[property_iri]['deriv_tx'] = get_the_unique_value_in_list_of_dict(sub_response, 'deriv_tx')

    return property_info_dct


def property_value_estimation_derivation_markup(
    derivation_client: PyDerivationClient,
    sparql_client: PySparqlClient,
    property_iri: str,
    property_price_index_iri: str,
    floor_area_iri: str,
    transaction_record_iri: str = None,
    avg_sqm_price_iri: str = None,
    market_value_iri: str = None,
    market_value_derivation_iri: str = None,
    market_value_derivation_tx_iri: str = None,
):
    if market_value_iri is None:
        try:
            # Create sync derivation for new info to get property value estimation computed
            input_lst = [property_price_index_iri, floor_area_iri, transaction_record_iri, avg_sqm_price_iri]
            input_iris = [iri for iri in input_lst if iri is not None]
            # NOTE createSyncDerivationForNewInfo assumes derivation agent to listen 
            #      on URL containing host.docker.internal
            #      For other URLs, e.g. localhost, use createSyncDerivationForNewInfoWithHttpUrl
            
            #derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
            derivation = derivation_client.createSyncDerivationForNewInfo(
                agentIRI=PROPERTY_VALUE_ESTIMATION_AGENT_IRI,
                # NOTE only relevant for createSyncDerivationForNewInfoWithHttpUrl
                #agentURL=PROPERTY_VALUE_ESTIMATION_AGENT_URL,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
            logger.info(f"Created sync derivation for new info: {derivation.getIri()}")
            logger.info(f"Created OM_AMOUNT_MONEY: {derivation.getBelongsToIris(iris.OM_AMOUNT_MONEY)}")
        except Exception as e:
            logger.error(f"Failed to create sync derivation for new info, inputsIRI: {str(input_iris)}")
            raise e
    else:
        # Property already has instantiated property value estimation 
        # TODO test
        logger.info(f"Market value {market_value_iri} exists for Property {property_iri}, no need to create derivation, checking if need to add any new transaction record...")

        if market_value_derivation_tx_iri is None:
            if transaction_record_iri is not None:
                # This means that instantiated property value is derived from AverageSquareMetrePrice
                # and FloorArea, but not from TransactionRecord
                # However, a new TransactionRecord was made available in HM Land Registry after the 
                # last time the PropertyValueEstimation was computed 
                # --> Add the new TransactionRecord to the inputs of derivation

                # Add timestamp to new tx iri (nothing happens if the iri already has timestamp)
                derivation_client.addTimeInstanceCurrentTimestamp(transaction_record_iri)
                # Add new tx iri to the existing derivation
                sparql_client.performUpdate(
                    f"""
                    INSERT DATA {{
                        <{market_value_derivation_iri}> <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> <{transaction_record_iri}>.
                    }}"""
                )
                logger.info(f"Added new tx iri ({transaction_record_iri}) to existing derivation: {market_value_derivation_iri} and requested for an update")
            else:
                # This means that instantiated property value is derived from AverageSquareMetrePrice 
                # and FloorArea, but not from TransactionRecord AND there's no new TransactionRecord
                # --> Do nothing
                logger.info(f"Market value {market_value_iri} already exist for Property {property_iri} and its inputs include AverageSquareMetrePrice {avg_sqm_price_iri} and FloorArea {floor_area_iri}, but not TransactionRecord, no new TransactionRecord is made available in HM Land Registry")
        else:
            # This means that instantiated property value is derived from TransactionRecord
            # Given the design of PropertySalesInstantiation (HM Land Registry) Agent: 
            #   IRI of a TransactionRecord will be kept the same, even if a new transaction
            #   record is published (i.e. only instantiated values and timestamp will be updated)
            # --> market_value_derivation_tx_iri and transaction_record_iri MUST be the same

            if market_value_derivation_tx_iri != transaction_record_iri:
                raise Exception(f"Market value {market_value_iri} already exist for Property {property_iri} and its inputs include TransactionRecord {market_value_derivation_tx_iri}, but it is different from the TransactionRecord {transaction_record_iri} directly connected to Property {property_iri} via {OBE_HAS_LATEST_TRANSACTION}")
            else:
                # One may choose to request for update this derivation here but it is not strictly necessary
                logger.info(f"Market value {market_value_iri} already exist for Property {property_iri} and its inputs include TransactionRecord {transaction_record_iri}")
                
        # Request for derivation update: 2 potential options
        # (as pure inputs (especially Property Price Index) have likely changed)
        # 1) Request derivation update for immediate execution (i.e. as synchronous call)
        derivation_client.unifiedUpdateDerivation(market_value_derivation_iri)
        # 2) Only mark derivation as requested to be executed with next asynchronous call
        #derivation_client.derivation_client.updateMixedAsyncDerivation(market_value_derivation_iri)


# =============================================================================
# Utility functions

def retrieve_derivation_instances(ppi_iri: str, sparql_client: PySparqlClient):
    """
    Retrieves ALL AvgSqmPrice and PropertyValueEstimation derivation instances 
    for a given Property Price Index (ppi_iri)
    """

    query = f"""
            SELECT DISTINCT ?deriv_avg ?deriv_value
            WHERE {{
                {{ ?deriv_avg <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> <{ppi_iri}> , 
                                                                        ?pc . 
                    ?pc <{RDF_TYPE}> <{OBE_POSTALCODE}> .
                }} UNION {{
                    ?deriv_value <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> <{ppi_iri}> , 
                                                                           ?area . 
                    ?area <{RDF_TYPE}> <{OM_AREA}> .
                }}
            }}"""
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve derivation instances: {query}")
    response = sparql_client.performQuery(query)
    # Extract lists of unique derivation instances
    avg_derivs = get_unique_values_in_list_of_dict(response, 'deriv_avg')
    value_derivs = get_unique_values_in_list_of_dict(response, 'deriv_value')

    return avg_derivs, value_derivs


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
