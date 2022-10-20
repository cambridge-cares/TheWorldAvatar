################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 19 Oct 2022                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

import uuid
from avgsmprice.datamodel.data_mapping import GBP_PER_SM

#import agentlogging

from avgsmprice.datamodel.iris import *

# Initialise logger
#logger = agentlogging.get_logger("prod")


#
# EXTERNAL SPARQL QUERIES
#
def get_nearby_postcodes(postcode_str:str) -> str:
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

    return remove_unnecessary_whitespace(query)


#
# SPARQL QUERIES
#
def get_postcode_iris(postcodes:list) -> str:
    # Retrieve IRI(s) of postcode(s) with given label(s)
    values_statement = format_literal_values_statement(postcodes)
    query = f"""
        SELECT ?pc_iri
        WHERE {{        
        VALUES ?pc {{ {values_statement} }}
        ?pc_iri <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                <{RDFS_LABEL}> ?pc . 
        }}
    """
    return remove_unnecessary_whitespace(query)


def get_postcode_strings(postcode_iris:list) -> str:
    # Retrieve string(s)/label(s) of postcode IRI(s)
    values_statement = format_iris_values_statement(postcode_iris)
    query = f"""
        SELECT ?pc
        WHERE {{    
        VALUES ?pc_iri {{ {values_statement} }}    
        ?pc_iri <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                <{RDFS_LABEL}> ?pc . 
        }}
    """
    return remove_unnecessary_whitespace(query)


def get_tx_iris_for_postcodes(postcode_iris:list) -> str:
    # Retrieve IRIs of all transactions for postcode(s)
    values_statement = format_iris_values_statement(postcode_iris)
    query = f"""
        SELECT ?tx_iri
        WHERE {{   
        VALUES ?pc_iri {{ {values_statement} }}     
        ?property <{RDF_TYPE}>/<{RDFS_SUBCLASS}>* <{OBE_PROPERTY}> ;
                  <{OBE_HAS_LATEST_TRANSACTION}> ?tx_iri ;
                  <{OBE_HAS_ADDRESS}>/<{OBE_HAS_POSTALCODE}> ?pc_iri . 
        }}
    """
    return remove_unnecessary_whitespace(query)


def get_tx_count_for_postcodes(postcodes:list) -> str:
    # Retrieve number of available sales transactions for postcode(s)
    values_statement = format_literal_values_statement(postcodes)    
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
    return remove_unnecessary_whitespace(query)


def get_ppi_iri(postcode_iri:str) -> str:
    # Retrieve IRI of Property Price Index for postcode
    query = f"""
        SELECT DISTINCT ?ppi_iri
        WHERE {{        
        <{postcode_iri}> ^<{OBE_HAS_POSTALCODE}>/<{OBE_HAS_ADMIN_DISTRICT}> ?local_authority .
        ?local_authority ^<{OBE_REPRESENTATIVE_FOR}> ?ppi_iri .
        }}
    """
    return remove_unnecessary_whitespace(query)


def get_avgsm_price_iri(postcode_iri:str) -> str:
    # Retrieve IRI of average square metre price for postcode
    query = f"""
        SELECT DISTINCT ?avg_price_iri
        WHERE {{        
        <{postcode_iri}> <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                         ^<{OBE_REPRESENTATIVE_FOR}> ?avg_price_iri .
        }}
    """
    return remove_unnecessary_whitespace(query)


def get_tx_details_and_floor_areas(tx_iris:list) -> str:
    # Retrieve transaction details and floor area for list of transactions
    if tx_iris:
        values_statement = format_iris_values_statement(tx_iris)

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
    return remove_unnecessary_whitespace(query)


def instantiate_average_price(postcode_iri, avg_price_iri, avg_price) -> str:
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
    return remove_unnecessary_whitespace(query)


def remove_unnecessary_whitespace(query: str) -> str:
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def format_literal_values_statement(values_list) -> str:
    # Create list of postcodes of interest
    values = '", "'.join(values_list)
    values = values.replace(',', f'^^<{XSD_STRING}>')
    values = f'"{values}"^^<{XSD_STRING}>'
    return values


def format_iris_values_statement(values_list) -> str:
    # Create list of IRIs of interest
    iris = '> <'.join(values_list)
    iris = '<' + iris + '>'
    return iris
