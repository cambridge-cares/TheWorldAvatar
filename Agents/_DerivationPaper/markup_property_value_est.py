from pathlib import Path
import time
import os

from pyderivationagent.data_model import iris as pda_iris
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
import chemistry_and_robots.kg_operations.dict_and_list as dal

from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
from configs import DERIVATION_INSTANCE_BASE_URL
from configs import PROPERTY_VALUE_ESTIMATION_AGENT_IRI
from configs import PROPERTY_VALUE_ESTIMATION_AGENT_URL
import iris

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# This module creates derivation markup for property value estimation (pve) of a property

# Specify name of csv with affected properties (determined using QGIS)
affected = 'affected_property_iris.csv'

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
    property_lst = dal.get_unique_values_in_list_of_dict(response, 'property')
    for property_iri in property_lst:
        property_info_dct[property_iri] = {}
        sub_response = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'property', property_iri)
        # it will be assigned as None if the key is not found
        property_info_dct[property_iri]['ppi'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'ppi')
        property_info_dct[property_iri]['area'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'area')
        property_info_dct[property_iri]['tx'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'tx')
        property_info_dct[property_iri]['asp'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'asp')
        property_info_dct[property_iri]['mv'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'mv')

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
):
    if market_value_iri is None:
        try:
            # Create sync derivation for new info to get property value estimation computed
            input_lst = [property_price_index_iri, floor_area_iri, transaction_record_iri, avg_sqm_price_iri]
            input_iris = [iri for iri in input_lst if iri is not None]
            # NOTE Here we use the function call createSyncDerivationForNewInfoWithHttpUrl instead of createSyncDerivationForNewInfo
            # This is to workaround the fact that the hasHttpUrl for the agent operation is stored with host.docker.internal
            #   which is not accessible from the host machine, hence we converted it to localhost and manually pass it in
            # TODO [when turning this script into an agent] keep host.docker.internal or let stack manager to take care of the routing
            derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                agentIRI=PROPERTY_VALUE_ESTIMATION_AGENT_IRI,
                agentURL=PROPERTY_VALUE_ESTIMATION_AGENT_URL,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
            logger.info(f"Created sync derivation for new info: {derivation.getIri()}")
            logger.info(f"Created OM_AMOUNT_MONEY: {derivation.getBelongsToIris(iris.OM_AMOUNT_MONEY)}")
        except Exception as e:
            logger.error(f"Failed to create sync derivation for new info, inputsIRI: {str(input_iris)}")
            raise e
    else:
        logger.info(f"Market value is already computed for {property_iri}, no need to create derivation")
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


def get_the_affected_buildings(input_csv):
    """
    Get the list of buildings that are affected by the flood event.
    To do it perooperly, the area should be queried from the polygon of the flood event.
    """
    # Extract IRIs from csv file
    with open(input_csv, 'r') as f:
        iris = f.read()
    iris = iris.split('\n')
    iris = iris[1:-1]
    return iris


if __name__ == '__main__':
    # Create a PySparqlClient instance
    sparql_client = PySparqlClient(
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Retrieve all property info
    property_info_dct = retrieve_property_info(sparql_client)
    print(f'Number of properties: {len(property_info_dct)}')

    # Create a PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Get the list of buildings that are affected by the flood event
    affected_building_iris = get_the_affected_buildings(os.path.join(Path(__file__).parent, 'data', affected))

    # Add derivation markup for each postal code
    for i in range(len(affected_building_iris)):
        time.sleep(1)
        logger.info("=============================================================")
        logger.info(f"Processing property {i+1}/{len(affected_building_iris)}")
        if affected_building_iris[i] in property_info_dct:
            _info = property_info_dct[affected_building_iris[i]]
            property_value_estimation_derivation_markup(
                derivation_client=derivation_client,
                sparql_client=sparql_client,
                property_iri=affected_building_iris[i],
                property_price_index_iri=_info['ppi'],
                floor_area_iri=_info['area'],
                transaction_record_iri=_info['tx'],
                avg_sqm_price_iri=_info['asp'],
                market_value_iri=_info['mv'],
            )
        else:
            logger.warning(f"Property {affected_building_iris[i]} not found in the property info list")
