################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries


import agentlogging

from landregistry.datamodel.iris import *

# Initialise logger
logger = agentlogging.get_logger("prod")


#
# EXTERNAL SPARQL QUERIES
#
def query_price_paid_for_postcode(postcode: str) -> str:
    # Retrieve price paid data for postcode(s)
    pass

# query property price index


#
# SPARQL QUERIES
#
def get_instantiated_properties_with_location_info(property_iris: list = None) -> str:
    # Retrieve instantiated properties with location information from KG

    values_statement = ""
    if property_iris:
        # Create list of IRIs of interest
        iris = '> <'.join(property_iris)
        iris = '<' + iris + '>'
        values_statement = f"VALUES ?property_iri {{ {iris} }} "

    query = f"""
        SELECT ?property_iri ?address_iri ?postcode_iri ?district_iri
        WHERE {{
        {values_statement}
        ?property_iri <{RDF_TYPE}>/<{RDFS_SUBCLASS}>* <{OBE_PROPERTY}> ;
                      <{OBE_LOCATEDIN}> ?district_iri ;
                      <{OBE_HAS_ADDRESS}> ?address_iri .
        ?address_iri <{OBE_HAS_POSTALCODE}> ?postcode_iri .                  
        }}
    """

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query
