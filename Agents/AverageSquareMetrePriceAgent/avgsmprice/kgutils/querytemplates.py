################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 19 Oct 2022                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

import uuid

#import agentlogging

from avgsmprice.datamodel.iris import *

# Initialise logger
#logger = agentlogging.get_logger("prod")


#
# EXTERNAL SPARQL QUERIES
#



#
# SPARQL QUERIES
#
def get_postcode_iri(postcode_str:str) -> str:
    # Retrieve IRI of postcode with given label
    query = f"""
        SELECT ?pc_iri
        WHERE {{        
        ?pc_iri <{RDF_TYPE}> <{OBE_POSTALCODE}> ; 
                <{RDFS_LABEL}> \"{postcode_str}\" . 
        }}
    """

    return remove_unnecessary_whitespace(query)


def get_tx_iris_for_postcode(postcode_iri:str) -> str:
        # Retrieve IRIs of all transactions for postcode
    query = f"""
        SELECT ?tx_iri
        WHERE {{        
        ?property <{RDF_TYPE}>/<{RDFS_SUBCLASS}>* <{OBE_PROPERTY}> ;
                  <{OBE_HAS_LATEST_TRANSACTION}> ?tx_iri ;
                  <{OBE_HAS_ADDRESS}>/<{OBE_HAS_POSTALCODE}> <{postcode_iri}>
        }}
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


def get_tx_details_and_floor_areas(tx_iris:list) -> str:
    # Retrieve transaction details and floor area for list of transactions

    values_statement = ""
    if tx_iris:
        # Create list of IRIs of interest
        iris = '> <'.join(tx_iris)
        iris = '<' + iris + '>'
        values_statement = f"VALUES ?tx_iri {{ {iris} }} "

    query = f"""
        SELECT ?tx_iri ?price ?date ?floor_area
        WHERE {{   
        {values_statement}
        ?tx_iri <{RDF_TYPE}> <{LRPPI_TRANSACTION_RECORD}> ;
                <{LRPPI_DATE}> ?date ;
                <{LRPPI_PRICE}> ?price ;
                ^<{OBE_HAS_LATEST_TRANSACTION}> ?property .
        ?property <{OBE_HAS_TOTAL_FLOOR_AREA}>/<{OM_HAS_VALUE}>/<{OM_NUM_VALUE}> ?floor_area .
        }}
    """

    return remove_unnecessary_whitespace(query)


def remove_unnecessary_whitespace(query: str) -> str:
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


# def get_instantiated_properties_with_location_info(property_iris: list = None) -> str:
#     # Retrieve instantiated properties with location information from KG

#     values_statement = ""
#     if property_iris:
#         # Create list of IRIs of interest
#         iris = '> <'.join(property_iris)
#         iris = '<' + iris + '>'
#         values_statement = f"VALUES ?property_iri {{ {iris} }} "

#     query = f"""
#         SELECT ?property_iri ?address_iri ?postcode_iri ?district_iri
#                ?property_type ?postcode ?street ?number ?bldg_name ?unit_name 
#                ?tx_iri
#         WHERE {{
#         {values_statement}
#         ?property_iri <{RDF_TYPE}> ?property_type . 
#         ?property_type <{RDFS_SUBCLASS}> <{OBE_PROPERTY}> . 
#         ?property_iri <{OBE_LOCATEDIN}> ?district_iri ; 
#                       <{OBE_HAS_ADDRESS}> ?address_iri . 
#         ?address_iri <{OBE_HAS_POSTALCODE}> ?postcode_iri . 
#         ?postcode_iri <{RDFS_LABEL}> ?postcode . 
#         OPTIONAL {{ ?address_iri <{ICONTACT_HAS_STREET}> ?street }}
#         OPTIONAL {{ ?address_iri <{ICONTACT_HAS_STREET_NUMBER}> ?number }}
#         OPTIONAL {{ ?address_iri <{ICONTACT_HAS_BUILDING}> ?bldg_name }}
#         OPTIONAL {{ ?address_iri <{OBE_HAS_UNIT_NAME}> ?unit_name }} 
#         OPTIONAL {{ ?property_iri <{OBE_HAS_LATEST_TRANSACTION}> ?tx_iri }} 
#         }}
#     """

#     # Remove unnecessary whitespaces
#     query = ' '.join(query.split())

#     return query


# def get_all_properties_with_postcode() -> str:
#     # Retrieve instantiated properties with location information from KG

#     query = f"""
#         SELECT ?property_iri ?postcode
#         WHERE {{
#         ?property_iri <{RDF_TYPE}> ?property_type ; 
#                       <{OBE_HAS_ADDRESS}> ?address_iri . 
#         ?address_iri <{OBE_HAS_POSTALCODE}>/<{RDFS_LABEL}> ?postcode . 
#         }}
#     """

#     # Remove unnecessary whitespaces
#     query = ' '.join(query.split())

#     return query


# def update_transaction_record(property_iri:None, address_iri:None, tx_iri:None, 
#                               new_tx_iri:None, price:None, date:None, 
#                               ppd_address_iri:None) -> str:
#     # Returns DELETE / INSERT query to instantiate/update transaction record
    
#     if property_iri and address_iri and price and date:
#         # Create tx_iri if not provided (i.e. not instantiated yet)
#         if not tx_iri:
#             tx_iri = KB + 'Transaction_' + str(uuid.uuid4())
        
#         # Start query building blocks
#         delete = f"""
#             DELETE {{
#                 <{tx_iri}> <{RDF_TYPE}> <{LRPPI_TRANSACTION_RECORD}> ; 
#                            <{LRPPI_PRICE}> ?price ; 
#                            <{LRPPI_DATE}> ?date ; 
#                            <{OWL_SAME_AS}> ?ppd_tx . 
#                 <{address_iri}> <{OBE_IS_PRESUMED_MATCH_OF}> ?ppd_address . 
#         }}
#         """

#         insert = f"""
#             INSERT {{
#                 <{property_iri}> <{OBE_HAS_LATEST_TRANSACTION}> <{tx_iri}> . 
#                 <{tx_iri}> <{RDF_TYPE}> <{LRPPI_TRANSACTION_RECORD}> ;
#                            <{LRPPI_PRICE}> \"{price}\"^^<{XSD_INTEGER}> ; 
#                            <{LRPPI_DATE}> \"{date}\"^^<{XSD_DATE}> . 
#         """
#         if new_tx_iri: insert += f"<{tx_iri}> <{OWL_SAME_AS}> <{new_tx_iri}> . "
#         if ppd_address_iri: insert += f"<{address_iri}> <{OBE_IS_PRESUMED_MATCH_OF}> <{ppd_address_iri}> . "
#         insert += f"}} "

#         where =f"""WHERE {{
#             <{property_iri}> <{OBE_HAS_ADDRESS}> <{address_iri}> . 
#             OPTIONAL {{ <{property_iri}> <{OBE_HAS_LATEST_TRANSACTION}> <{tx_iri}> . 
#                         <{tx_iri}> <{LRPPI_PRICE}> ?price ;
#                                 <{LRPPI_DATE}> ?date . }}
#             OPTIONAL {{ <{tx_iri}> <{OWL_SAME_AS}> ?ppd_tx }}
#             OPTIONAL {{ <{address_iri}> <{OBE_IS_PRESUMED_MATCH_OF}> ?ppd_address }} 
#             }}
#         """

#         query = delete + insert + where
#         # Remove unnecessary whitespaces
#         query = ' '.join(query.split())
    
#     else:
#         query = None

#     return query


# def get_all_admin_districts_with_price_indices() -> str:
#     # Retrieve instantiated administrative districts (i.e. local authorities) 
#     # including their ONS equivalent instance and (potentially) instantiated
#     # property price indices

#     query = f"""
#         SELECT ?local_authority ?ons_district ?ukhpi
#         WHERE {{
#         ?local_authority <{RDF_TYPE}> <{OBE_ADMIN_DISTRICT}> ;
#                          <{OWL_SAME_AS}> ?ons_district . 
#         OPTIONAL {{ ?local_authority ^<{OBE_REPRESENTATIVE_FOR}> ?ukhpi . 
#                     ?ukhpi <{RDF_TYPE}> <{OBE_PROPERTY_PRICE_INDEX}> . }} 
#         }}
#     """

#     # Remove unnecessary whitespaces
#     query = ' '.join(query.split())

#     return query


# def instantiate_property_price_index(district_iri, ppi_iri):
#     # Instantiate property price index for a given administrative district
#     query = f"""
#         INSERT DATA {{
#             <{ppi_iri}> <{RDF_TYPE}> <{OBE_PROPERTY_PRICE_INDEX}> . 
#             <{ppi_iri}> <{OBE_REPRESENTATIVE_FOR}> <{district_iri}> . 
#         }}
#     """

#     # Remove unnecessary whitespaces
#     query = ' '.join(query.split())

#     return query
