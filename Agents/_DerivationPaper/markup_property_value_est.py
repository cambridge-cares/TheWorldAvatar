from typing import List
import time

from pyderivationagent.data_model import iris as pda_iris
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
import chemistry_and_robots.kg_operations.dict_and_list as dal

from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
from configs import DERIVATION_INSTANCE_BASE_URL
from configs import PROPERTY_VALUE_ESTIMATION_AGENT_IRI
import iris

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# This module creates derivation markup for property value estimation (pve) of a property

def retrieve_property_info(sparql_client: PySparqlClient):
    # Construct query to retrieve below information for each property from KG
    # - property price index (ppi) IRI
    # - transaction record (tx) IRI (if available)
    # - average sqm price (asp) IRI (if available)
    # - floor area (area) IRI
    # NOTE for this iteration, we assume that the average sqm price is already computed if the derivation is created
    # Also query if the market value is already computed
    # - market value (mv) IRI (if available)
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?property ?ppi ?area ?tx ?asp ?mv
            WHERE {{
                ?property rdf:type/rdfs:subClassOf* <{iris.OBE_PROPERTY}>.
                ?property <{iris.OBE_HASADDRESS}> ?address.
                ?address <{iris.OBE_HASADMINISTRATIVEDISTRICT}> ?district.
                ?ppi <{iris.OBE_REPRESENTATIVE_FOR}> ?district.
                ?property <{iris.OBE_HASTOTALFLOORAREA}> ?area.
                OPTIONAL {{
                    ?property <{iris.OBE_HASLATESTTRANSACTIONRECORD}> ?tx.
                }}
                OPTIONAL {{
                    ?address <{iris.OBE_HASPOSTALCODE}> ?postal.
                    ?asp <{iris.OBE_REPRESENTATIVE_FOR}> ?postal.
                    ?asp rdf:type <{iris.OBE_AVERAGE_SM_PRICE}>.
                }}
                OPTIONAL {{
                    ?property <{iris.OBE_HASMARKETVALUE}> ?mv.
                }}
            }}"""

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve property info: {query}")
    response = sparql_client.performQuery(query)

    # Rearrange response into a list of dictionaries where the information for each property are grouped together
    # Target format of list of dictionaries:
    # [
    #     { # Property 1
    #         'property': iri,
    #         'ppi': iri,
    #         'area': iri,
    #         'tx': [iri, iri, ...],
    #         'asp': iri,
    #     },
    #     ... # Property 2, 3, ...
    # ]
    # NOTE As tx/asp are optional, they may not be presented in the returned list of dicts
    property_info_lst = []
    property_lst = dal.get_unique_values_in_list_of_dict(response, 'property')
    for property_iri in property_lst:
        property_info = {}
        property_info['property'] = property_iri
        sub_response = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'property', property_iri)
        # it will be assigned as None if the key is not found
        property_info['ppi'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'ppi')
        property_info['area'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'area')
        property_info['tx'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'tx')
        property_info['asp'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'asp')
        property_info['mv'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'mv')
        property_info_lst.append(property_info)

    return property_info_lst


def property_value_estimation_derivation_markup(
    derivation_client: PyDerivationClient,
    sparql_client: PySparqlClient,
    property_iri: str,
    property_price_index_iri: str,
    floor_area_iri: str,
    transaction_record_iri: str = None,
    avg_sqm_price_iri: str = None,
    market_value_iri: str = None,
):
    if market_value_iri is None:
        try:
            # Create sync derivation for new info to get property value estimation computed
            input_lst = [property_price_index_iri, floor_area_iri, transaction_record_iri, avg_sqm_price_iri]
            input_iris = [iri for iri in input_lst if iri is not None]
            derivation = derivation_client.createSyncDerivationForNewInfo(
                agentIRI=PROPERTY_VALUE_ESTIMATION_AGENT_IRI,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
            logger.info(f"Created sync derivation for new info: {derivation.getIri()}")
            logger.info(f"Created OM_AMOUNT_MONEY: {derivation.getBelongsToIris(iris.OM_AMOUNT_MONEY)}")
        except Exception as e:
            logger.error(f"Failed to create sync derivation for new info, inputsIRI: {str(input_iris)}")
            raise e
    else:
        # TODO think through what to do if the market value is already computed
        pass
        # # Construct a list of tx iri that are not used to calculate the existing avg sqm price
        # new_tx_iri_lst = [iri for iri in transaction_record_iri_lst if iri not in existing_asp_derivation_tx_iri_lst]
        # # Add the new tx iri to the existing derivation and request for update
        # if bool(new_tx_iri_lst):
        #     # Add timestamp to new tx iri
        #     derivation_client.addTimeInstanceCurrentTimestamp(new_tx_iri_lst)
        #     # Add new tx iri to the existing derivation
        #     sparql_client.performUpdate(
        #         f"""
        #         INSERT DATA {{
        #             VALUES ?tx {{ {' '.join([f'<{iri}>' for iri in new_tx_iri_lst])} }}
        #             <{existing_asp_derivation_iri}> <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?tx.
        #         }}"""
        #     )
        #     # Request for derivation update
        #     derivation_client.unifiedUpdateDerivation(existing_asp_derivation_iri)
        #     logger.info(f"Added new tx iri ({new_tx_iri_lst}) to existing derivation: {existing_asp_derivation_iri}")
        # else:
        #     logger.info(f"No new tx iri to add to existing derivation: {existing_asp_derivation_iri}")


if __name__ == '__main__':
    # Create a PySparqlClient instance
    sparql_client = PySparqlClient(
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Retrieve all property info
    property_info_lst = retrieve_property_info(sparql_client)
    print(f'Number of properties: {len(property_info_lst)}')

    # Create a PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Add derivation markup for each postal code
    for i in range(len(property_info_lst)):
        time.sleep(1)
        logger.info("=============================================================")
        logger.info(f"Processing property {i+1}/{len(property_info_lst)}")
        property_value_estimation_derivation_markup(
            derivation_client=derivation_client,
            sparql_client=sparql_client,
            property_price_index_iri=property_info_lst[i]['ppi'],
            floor_area_iri=property_info_lst[i]['area'],
            transaction_record_iri=property_info_lst[i]['tx'],
            avg_sqm_price_iri=property_info_lst[i]['asp'],
            market_value_iri=property_info_lst[i]['mv'],
        )
