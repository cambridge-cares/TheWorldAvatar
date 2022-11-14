################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from agent.datamodel.iris import *
from agent.datamodel.data import GBP_PER_SM, ONS_ENDPOINT

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    def get_avgsm_price_iri(self, postcode_iri:str) -> str:
        # Retrieve IRI of average square metre price for postcode
        query = f"""
            SELECT DISTINCT ?avg_price_iri
            WHERE {{        
            <{postcode_iri}> <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                            ^<{OBE_REPRESENTATIVE_FOR}> ?avg_price_iri .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if res:
            avg_price = res[0]['avg_price_iri']
        else:
            avg_price = None
        return avg_price


    def get_tx_details_and_floor_areas(self, tx_iris:list) -> list:
        # Retrieve transaction details and floor areas for list of transactions
        # Returns list of dictionaries with keys: tx_iri, price, date, floor_area
        if tx_iris:
            values_statement = self.format_iris_values_statement(tx_iris)

        query = f"""
            SELECT ?tx_iri ?price ?date ?floor_area
            WHERE {{   
            VALUES ?tx_iri {{ {values_statement} }} 
            ?tx_iri <{RDF_TYPE}> <{LRPPI_TRANSACTION_RECORD}> ;
                    <{LRPPI_DATE}> ?date ;
                    <{LRPPI_PRICE}> ?price ;
                    ^<{OBE_HAS_LATEST_TRANSACTION}> ?property .
            ?property <{OBE_HAS_TOTAL_FLOOR_AREA}>/<{OM_HAS_VALUE}>/<{OM_NUM_VALUE}> ?floor_area .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        return res


    def instantiate_average_price(self, postcode_iri, avg_price_iri, avg_price) -> str:
        # Returns INSERT DATA query to instantiate/update average square metre price

        # Create unique IRIs for new instances
        measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        
        query = f"""
            <{avg_price_iri}> <{RDF_TYPE}> <{OBE_AVERAGE_SM_PRICE}> . 
            <{avg_price_iri}> <{OBE_REPRESENTATIVE_FOR}> <{postcode_iri}> . 
            <{avg_price_iri}> <{OM_HAS_VALUE}> <{measure_iri}> . 
            <{measure_iri}> <{RDF_TYPE}> <{OM_MEASURE}> . 
            <{measure_iri}> <{OM_NUM_VALUE}> \"{avg_price}\"^^<{XSD_INTEGER}> . 
            <{measure_iri}> <{OM_HAS_UNIT}> <{UOM_GBP_M2}> . 
            <{UOM_GBP_M2}> <{OM_SYMBOL}> \"{GBP_PER_SM}\"^^<{XSD_STRING}> . 
        """
        #TODO: Triple with symbol potentially to be removed once OntoUOM contains
        #      all relevant units/symbols and is uploaded to the KB
        return self.remove_unnecessary_whitespace(query)


    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query


    def format_literal_values_statement(self, values_list) -> str:
        # Create list of postcodes of interest
        values = '", "'.join(values_list)
        values = values.replace(',', f'^^<{XSD_STRING}>')
        values = f'"{values}"^^<{XSD_STRING}>'
        return values


    def format_iris_values_statement(self, values_list) -> str:
        # Create list of IRIs of interest
        iris = '> <'.join(values_list)
        iris = '<' + iris + '>'
        return iris
