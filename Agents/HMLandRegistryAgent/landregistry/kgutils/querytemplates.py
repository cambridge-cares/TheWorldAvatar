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
# HM Land Registry Price Paid Data model detailed here:
# https://landregistry.data.gov.uk/app/root/doc/ppd
def get_transaction_data_for_postcodes(postcodes: list) -> str:
    # Retrieve price paid transaction data for postcode(s)
    
    # Create list of postcodes of interest
    values = '", "'.join(postcodes)
    values = values.replace(',', f'^^<{XSD_STRING}>')
    values = f'"{values}"^^<{XSD_STRING}>'

    query = f"""
        SELECT ?tx_iri ?price ?date ?property_type ?tx_category
               ?address_iri ?paon ?saon ?street ?town ?postcode ?district ?county
        WHERE
        {{
        VALUES ?postcode {{ {values} }}
        ?address_iri {LRCOMMON_POSTCODE} ?postcode.

        ?tx_iri {LRPPI_PROPERTY_ADDRESS} ?address_iri ;
                {LRPPI_PRICE} ?price ;
                {LRPPI_DATE} ?date ;
                {LRPPI_PROPERTY_TYPE}/{RDFS_LABEL} ?property_type ;
                {LRPPI_TX_CATEGORY}/{SKOS_LABEL} ?tx_category.
        OPTIONAL {{?address_iri {LRCOMMON_PAON} ?paon}}
        OPTIONAL {{?address_iri {LRCOMMON_SAON} ?saon}}
        OPTIONAL {{?address_iri {LRCOMMON_STREET} ?street}}
        OPTIONAL {{?address_iri {LRCOMMON_TOWN} ?town}}
        OPTIONAL {{?address_iri {LRCOMMON_DISTRICT} ?district}}
        OPTIONAL {{?address_iri {LRCOMMON_DISTRICT} ?county}}
        }}
    """
    
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query

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
               ?postcode ?street ?house_number
        WHERE {{
        {values_statement}
        ?property_iri <{RDF_TYPE}>/<{RDFS_SUBCLASS}>* <{OBE_PROPERTY}> ;
                      <{OBE_LOCATEDIN}> ?district_iri ;
                      <{OBE_HAS_ADDRESS}> ?address_iri .
        ?address_iri <{OBE_HAS_PROPERTYNUMBER}> ?house_number ;
                     <{ICONTACT_HAS_STREET}> ?street ;
                     <{OBE_HAS_POSTALCODE}> ?postcode_iri . 
        ?postcode_iri <{RDFS_LABEL}> ?postcode .
        }}
    """

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query
