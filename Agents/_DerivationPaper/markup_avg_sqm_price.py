from typing import List
import time

from pyderivationagent.data_model import iris as pda_iris
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
import chemistry_and_robots.kg_operations.dict_and_list as dal

from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
from configs import DERIVATION_INSTANCE_BASE_URL
from configs import AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI
from configs import AVERAGE_SQUARE_METRE_PRICE_AGENT_URL
import iris

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# This module creates derivation markup for average price per square meter (asp) of a postal code

def retrieve_postal_code_info(sparql_client: PySparqlClient):
    # Construct query to retrieve below information for each postal code from KG
    # - transaction record (tx) IRI
    # - property price index (ppi) IRI
    # Also, the query retrieve below information for each postal code if available
    # - average price per square meter (asp) IRI
    # - average price per square meter derivarion (derivation)
    # - transaction record used to calculate the existing asp (deriv_tx)
    query = f"""{pda_iris.PREFIX_RDF} {pda_iris.PREFIX_RDFS}
            SELECT DISTINCT ?postal ?tx ?ppi ?asp ?derivation ?deriv_tx
            WHERE {{
                ?postal rdf:type <{iris.OBE_POSTALCODE}>.
                ?address <{iris.OBE_HASPOSTALCODE}> ?postal;
                        <{iris.OBE_HASADMINISTRATIVEDISTRICT}> ?district.
                ?ppi <{iris.OBE_REPRESENTATIVE_FOR}> ?district.
                ?property <{iris.OBE_HASADDRESS}> ?address.
                ?property rdf:type/rdfs:subClassOf* <{iris.OBE_PROPERTY}>.
                ?property <{iris.OBE_HASLATESTTRANSACTIONRECORD}> ?tx.
                OPTIONAL {{
                    ?asp <{iris.OBE_REPRESENTATIVE_FOR}> ?postal.
                    ?asp <{pda_iris.ONTODERIVATION_BELONGSTO}> ?derivation.
                    ?derivation <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> ?deriv_tx.
                    ?deriv_tx rdf:type <{iris.PPI_TRANSACTIONRECORD}>.
                }}
            }}"""

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    logger.info(f"Query to retrieve postal code info: {query}")
    response = sparql_client.performQuery(query)

    # Rearrange response into a list of dictionaries where the information for each postal code are grouped together
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
    # NOTE As asp/derivation/deriv_tx are optional, they may not be presented in the returned list of dicts
    postal_code_info_lst = []
    postal_code_lst = dal.get_unique_values_in_list_of_dict(response, 'postal')
    for postal_code in postal_code_lst:
        postal_code_info = {}
        postal_code_info['postal_code'] = postal_code
        sub_response = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'postal', postal_code)
        postal_code_info['tx'] = dal.get_unique_values_in_list_of_dict(sub_response, 'tx')
        postal_code_info['ppi'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'ppi')
        # The function dal.get_the_unique_value_in_list_of_dict returns None if key `asp` does not exist
        asp = dal.get_the_unique_value_in_list_of_dict(sub_response, 'asp')
        if asp is not None:
            postal_code_info['asp'] = asp
            postal_code_info['derivation'] = dal.get_the_unique_value_in_list_of_dict(sub_response, 'derivation')
            postal_code_info['deriv_tx'] = dal.get_unique_values_in_list_of_dict(sub_response, 'deriv_tx')
        postal_code_info_lst.append(postal_code_info)

    return postal_code_info_lst


def avg_sqm_price_derivation_markup(
    derivation_client: PyDerivationClient,
    sparql_client: PySparqlClient,
    postal_code_iri: str,
    transaction_record_iri_lst: List[str],
    property_price_index_iri: str,
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
            # Add timestamp to new tx iri
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


if __name__ == '__main__':
    # Create a PySparqlClient instance
    sparql_client = PySparqlClient(
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Retrieve all postal code info
    # TODO this method assumes that each postal code already has at least one transaction record
    postal_code_info_lst = retrieve_postal_code_info(sparql_client)
    print(f'Number of postal codes: {len(postal_code_info_lst)}')

    # Create a PyDerivationClient instance
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=SPARQL_QUERY_ENDPOINT,
        update_endpoint=SPARQL_UPDATE_ENDPOINT,
    )

    # Add derivation markup for each postal code
    for i in range(len(postal_code_info_lst)):
        time.sleep(1)
        logger.info("=============================================================")
        logger.info(f"Processing postal code {i+1}/{len(postal_code_info_lst)}")
        avg_sqm_price_derivation_markup(
            derivation_client=derivation_client,
            sparql_client=sparql_client,
            postal_code_iri=postal_code_info_lst[i]['postal_code'],
            transaction_record_iri_lst=postal_code_info_lst[i]['tx'],
            property_price_index_iri=postal_code_info_lst[i]['ppi'],
            existing_avg_sqm_price_iri=postal_code_info_lst[i].get('asp'),
            existing_asp_derivation_iri=postal_code_info_lst[i].get('derivation'),
            existing_asp_derivation_tx_iri_lst=postal_code_info_lst[i].get('deriv_tx'),
        )
