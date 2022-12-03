################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Oct 2022                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import json
import uuid
import urllib.parse
import requests

from pyderivationagent.kg_operations import PySparqlClient

from py4jps import agentlogging
from avgsqmpriceagent.datamodel.iris import *
from avgsqmpriceagent.datamodel.data import GBP_PER_SM
from avgsqmpriceagent.errorhandling.exceptions import APIException
from avgsqmpriceagent.datamodel.data import ONS_ENDPOINT

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    
    #
    # EXTERNAL SPARQL QUERIES
    #
    def get_nearby_postcodes(self, postcode_str:str) -> str:
        # Retrieve postcodes within same Super Output Area (SOA, middle layer) as given postcode
        # including their easting and northing coordinates    
        query = f"""
            SELECT ?pc ?easting ?northing
            WHERE {{
                ?s <{RDF_TYPE}> <{ONS_POSTCODE}> ; 
                <{RDFS_LABEL}> \"{postcode_str}\" ; 
                <{ONS_WITHIN_SOSA}> ?sosa . 
                ?sosa ^<{ONS_WITHIN_SOSA}> ?pc_iri . 
                ?pc_iri <{RDF_TYPE}> <{ONS_POSTCODE}> ; 
                        <{RDFS_LABEL}> ?pc ; 
                        <{ONS_NORTHING}> ?northing ; 
                        <{ONS_EASTING}> ?easting . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        query = urllib.parse.quote(query)
        # Perform GET request
        url = ONS_ENDPOINT + '.json?query=' + query
        res = requests.get(url)
        if res.status_code != 200:
            logger.error('Error retrieving data from ONS API.')
            raise APIException('Error retrieving data from ONS API.')

        # Extract and unwrap results
        data = json.loads(res.text)
        return data


    #
    # SPARQL QUERIES
    #
    def get_postcode_iris(self, postcodes:list) -> list:
        # Retrieve IRI(s) of postcode(s) with given label(s)
        values_statement = self.format_literal_values_statement(postcodes)
        query = f"""
            SELECT ?pc_iri
            WHERE {{        
            VALUES ?pc {{ {values_statement} }}
            ?pc_iri <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                    <{RDFS_LABEL}> ?pc . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        postcode_iris = [r['pc_iri'] for r in res]
        return postcode_iris


    def get_postcode_strings(self, postcode_iris:list) -> list:
        # Retrieve string(s)/label(s) of postcode IRI(s)
        values_statement = self.format_iris_values_statement(postcode_iris)
        query = f"""
            SELECT ?pc
            WHERE {{    
            VALUES ?pc_iri {{ {values_statement} }}    
            ?pc_iri <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                    <{RDFS_LABEL}> ?pc . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        postcode_strs = [r['pc'] for r in res]
        return postcode_strs


    def get_tx_iris_for_postcodes(self, postcode_iris:list) -> list:
        # Retrieve IRIs of all transactions for postcode(s)
        values_statement = self.format_iris_values_statement(postcode_iris)
        query = f"""
            SELECT ?tx_iri
            WHERE {{   
            VALUES ?pc_iri {{ {values_statement} }}     
            ?property <{RDF_TYPE}>/<{RDFS_SUBCLASS}>* <{OBE_PROPERTY}> ;
                    <{OBE_HAS_LATEST_TRANSACTION}> ?tx_iri ;
                    <{OBE_HAS_ADDRESS}>/<{OBE_HAS_POSTALCODE}> ?pc_iri . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        tx_iris = [r['tx_iri'] for r in res]
        return tx_iris


    def get_tx_count_for_postcodes(self, postcodes:list) -> str:
        # Retrieve number of available sales transactions for postcode(s) and
        # return dictionary with postcode as key and number of transactions as value
        values_statement = self.format_literal_values_statement(postcodes)    
        query = f"""
            SELECT DISTINCT ?pc (count(?tx) as ?txs )
            WHERE {{   
            VALUES ?pc {{ {values_statement} }}     
            ?property <{RDF_TYPE}>/<{RDFS_SUBCLASS}>* <{OBE_PROPERTY}> ;
                    <{OBE_HAS_ADDRESS}>/<{OBE_HAS_POSTALCODE}>/<{RDFS_LABEL}> ?pc ;
                    <{OBE_HAS_LATEST_TRANSACTION}> ?tx ;
            }}
            GROUP BY ?pc
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        tx_map = {r['pc']: r['txs'] for r in res}
        return tx_map


    def get_ppi_iri(self, postcode_iri:str) -> str:
        # Retrieve IRI of Property Price Index for postcode
        query = f"""
            SELECT DISTINCT ?ppi_iri
            WHERE {{        
            <{postcode_iri}> ^<{OBE_HAS_POSTALCODE}>/<{OBE_HAS_ADMIN_DISTRICT}> ?local_authority .
            ?local_authority ^<{OBE_REPRESENTATIVE_FOR}> ?ppi_iri .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        if len(res) == 1:
            ppi = res[0]['ppi_iri']
        else:
            ppi = None
        return ppi


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
        """
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
