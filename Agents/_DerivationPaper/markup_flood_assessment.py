from pathlib import Path
from typing import List
import time
import os

from pyderivationagent.data_model import iris as pda_iris
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
import chemistry_and_robots.kg_operations.dict_and_list as dal

from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
from configs import DERIVATION_INSTANCE_BASE_URL
from configs import FLOOD_ASSESSMENT_AGENT_IRI
import iris

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# This module creates derivation markup for flood assessment of a property

# Specify name of csv with affected properties (determined using QGIS)
affected = 'affected_property_iris.csv'

# Flood warning iri
flood_warning_iri = iris.flood_warning_iri


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
    # NOTE for this iteration, we assume that the property value estimation is already computed if the derivation is created
    # - market value (mv) IRI
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?property ?mv
            WHERE {{
                VALUES ?property {{ <{'> <'.join(affected_property_iris)}> }}
                ?property <{iris.OBE_HASMARKETVALUE}> ?mv.
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
    #         'mv': iri,
    #     },
    #     ... (other properties)
    # }
    property_info_dct = {}
    property_lst = dal.get_unique_values_in_list_of_dict(response, 'property')
    for property_iri in property_lst:
        property_info_dct[property_iri] = {}
        sub_response = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'property', property_iri)
        # it will be assigned as None if the key is not found
        property_info_dct[property_iri]['mv'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'mv')

    return property_info_dct


def flood_assessment_derivation_markup(
    derivation_client: PyDerivationClient,
    sparql_client: PySparqlClient,
    flood_warning_iri: str,
    affected_building_iris: List[str],
    property_value_iris: List[str],
    flood_assessment_derivation_iri: str = None,
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

    # TODO here we mocked the affected buildings, should be done by querying the flood event polygon from ontop
    # Get the list of buildings that are affected by the flood event
    affected_building_iris = get_the_affected_buildings(os.path.join(Path(__file__).parent, 'data', affected))

    # Retrieve all affected building info
    property_info_dct = retrieve_affected_property_info(sparql_client, affected_building_iris)
    print(f'Number of properties: {len(property_info_dct)}')

    # Create a PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Add derivation markup for the flood event
    logger.info("=============================================================")
    logger.info(f"Processing flood event {flood_warning_iri}")
    flood_assessment_derivation_markup(
        derivation_client=derivation_client,
        sparql_client=sparql_client,
        flood_warning_iri=flood_warning_iri,
        affected_building_iris=affected_building_iris,
        property_value_iris=[property_info_dct[iri]['mv'] for iri in affected_building_iris],
        flood_assessment_derivation_iri=retrieve_flood_assessment_derivation_iri(
            sparql_client=sparql_client,
            flood_warning_iri=flood_warning_iri,
            flood_assessment_agent_iri=FLOOD_ASSESSMENT_AGENT_IRI,
        )
    )
