################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Sep 2022                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

import uuid
import pandas as pd

from epcdata.datamodel.iris import *
from epcdata.datamodel.data_mapping import UNITS_MAPPING
#from epcdata.kgutils.stackclients import PostGISClient


#
# EXTERNAL SPARQL QUERIES
#
def ons_postcodes_per_localauthority(local_authority_district: str) -> str:
    # Retrieve postcodes for given local authority district from ONS
    query = f"""
        SELECT DISTINCT *
        WHERE {{
        ?district_iri <{RDF_TYPE}> <{ONS_GEOGRAPGY}> ;
                      <{RDFS_LABEL}> "{local_authority_district}" ;
                      <{ONS_NAME}> ?district .                      
        ?postcode_iri <{ONS_BEST_LOCAL_AUTHORITY}> ?district_iri ;
                      <{RDFS_LABEL}> ?postcode
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query

#
# SPARQL QUERIES
#
def check_instantiated_local_authority(local_authority_district: str) -> str:
    # Check if local authority district is instantiated (per OntoBuiltEnv)
    query = f"""
        SELECT ?district_iri
        WHERE {{
        ?district <{RDF_TYPE}> <{OBE_ADMIN_DISTRICT}> ;
                  <{OWL_SAMEAS}> ?ons_district .
        ?ons_district <{ONS_NAME}> "{local_authority_district}"
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def instantiated_postalcodes() -> str:
    # Retrieve all instantiated postal codes (per OntoBuiltEnv)
    query = f"""
        SELECT ?postcode
        WHERE {{
        ?pc <{RDF_TYPE}> <{OBE_POSTALCODE}> ;
            <{RDFS_LABEL}> ?postcode
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def instantiated_epc_for_uprn(uprn: str) -> str:
    # Get latest instantiated epc (i.e. individual lodgement identifier) for UPRN
    query = f"""
        SELECT ?uprn ?certificate
        WHERE {{
            VALUES ?uprn {{ "{uprn}" }}
            ?property <{OBE_HAS_IDENTIFIER}> ?uprn ;
                      <{OBE_HAS_LATEST_EPC}> ?certificate
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def get_postcode_and_district_iris(postcode: str, local_authority_code: str) -> str:
    # Get IRIs of postcode and local authority district with given codes
    query = f"""
        SELECT ?postcode ?district
        WHERE {{
            ?postcode <{RDF_TYPE}> <{OBE_POSTALCODE}> ;
                      <{RDFS_LABEL}> "{postcode}" .
            ?district <{RDF_TYPE}> <{OBE_ADMIN_DISTRICT}> ;
                      <{OWL_SAMEAS}>/<{ONS_NAME}> "{local_authority_code}"
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query

#
# SPARQL UPDATES
#
def instantiate_postcodes_for_district(local_authority_district: str,
                                       district_data: pd.DataFrame,
                                       postcode_data: pd.DataFrame)-> str:
    """
        Create INSERT query to instantiate local authority district with postcodes

        Arguments:
            local_authority_district - local authority district code
            district data - DataFrame with 1 row and columns:
                            ['district_iri', 'district', 'iri']
            postcode_data - DataFrame with columns:
                            ['postcode_iri', 'postcode', 'iri']
    """
    # Start INSERT query
    query = f"""
        INSERT DATA {{
    """

    # Add AdministrativeDistrict triples
    d = district_data.iloc[0]
    q = f"""
        <{d['iri']}> <{RDF_TYPE}> <{OBE_ADMIN_DISTRICT}> .
        <{d['iri']}> <{RDFS_LABEL}> "{d['district']}" .
        <{d['iri']}> <{OWL_SAMEAS}> <{d['district_iri']}> .
        <{d['district_iri']}> <{ONS_NAME}> "{local_authority_district}" .
    """
    query += q

    # Add PostalCode triples
    for index, row in postcode_data.iterrows():
        q = f"""
            <{row['iri']}> <{RDF_TYPE}> <{OBE_POSTALCODE}> .
            <{row['iri']}> <{RDFS_LABEL}> "{row['postcode']}" .
            <{row['iri']}> <{OWL_SAMEAS}> <{row['postcode_iri']}> .
            <{row['postcode_iri']}> <{ONS_BEST_LOCAL_AUTHORITY}> <{d['district_iri']}> .
        """
        query += q

    # Close query
    query += f"}}"
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def add_epc_data(property_iri: str = None, uprn: str = None,
                 address_iri: str = None, addr_street: str = '', addr_number: str = '',
                 postcode_iri:str = None, district_iri: str = None,
                 built_form_iri: str = None, property_type_iri: str = None,
                 usage_iri: str = None, usage_label: str = None,
                 construction_start: str = None, construction_end: str = None,
                 floor_description: str = None, roof_description: str = None, 
                 wall_description: str = None, windows_description: str = None,  
                 floor_area: float = None, rooms: int = None,
                 epc_rating: str = None, epc_lmkkey: str = None) -> str:
    
    if property_iri and uprn:
        # Start INSERT query
        query = f"""
            INSERT DATA {{
        """

        # Returns triples to instantiate EPC data for single property
        if 'Building' in property_iri:
            triples = f"<{property_iri}> <{RDF_TYPE}> <{OBE_BUILDING}> . "
            if built_form_iri: triples += f"<{property_iri}> <{OBE_HAS_BUILT_FORM}> <{built_form_iri}> . "
        else:
            triples = f"<{property_iri}> <{RDF_TYPE}> <{OBE_FLAT}> . "

        # Postal code and admin district
        if address_iri: 
            triples += f"""<{property_iri}> <{OBE_HAS_ADDRESS}> <{address_iri}> . 
                           <{address_iri}> <{RDF_TYPE}> <{ICONTACT_ADDRESS}> .  
                           <{address_iri}> <{ICONTACT_HAS_STREET}> \"{addr_street}\"^^<{XSD_STRING}> .
                           <{address_iri}> <{OBE_HAS_PROPERTYNUMBER}> \"{addr_number}\"^^<{XSD_STRING}> . """
            if postcode_iri: triples += f"<{address_iri}> <{OBE_HAS_POSTALCODE}> <{postcode_iri}> . "
            if district_iri: triples += f"<{address_iri}> <{OBE_HAS_ADMIN_DISTRICT}> <{district_iri}> . "
        if district_iri: triples += f"<{property_iri}> <{OBE_LOCATEDIN}> <{district_iri}> . "
        
        # Instances        
        if property_type_iri: triples += f"<{property_iri}> <{OBE_HAS_PROPERTY_TYPE}> <{property_type_iri}> . "
        if usage_iri: 
            triples += f"<{property_iri}> <{OBE_HAS_USAGE}> <{usage_iri}> . "
            if usage_label: triples += f"<{usage_iri}> <{RDFS_LABEL}> \"{usage_label}\"^^<{XSD_STRING}> . "
        if construction_start or construction_end:
            #TODO: Potentially include relevant construction time bands in ABox
            interval_iri = KB + 'Interval_' + str(uuid.uuid4())
            triples += f"""<{property_iri}> <{OBE_HAS_CONSTRUCTION_DATE}> <{interval_iri}> . 
                           <{interval_iri}> <{RDF_TYPE}> <{TIME_INTERVAL}> . """
            if construction_start:
                instant_iri = KB + 'Instant_' + str(uuid.uuid4())
                triples += f"""<{interval_iri}> <{TIME_HAS_BEGINNING}> <{instant_iri}> . 
                               <{instant_iri}> <{RDF_TYPE}> <{TIME_INSTANT}> . 
                               <{instant_iri}> <{TIME_IN_DATETIME_STAMP}> \"{construction_start}\"^^<{XSD_DATETIMESTAMP}> . """
            if construction_end:
                instant_iri = KB + 'Instant_' + str(uuid.uuid4())
                triples += f"""<{interval_iri}> <{TIME_HAS_END}> <{instant_iri}> . 
                               <{instant_iri}> <{RDF_TYPE}> <{TIME_INSTANT}> . 
                               <{instant_iri}> <{TIME_IN_DATETIME_STAMP}> \"{construction_end}\"^^<{XSD_DATETIMESTAMP}> . """
        if floor_description: 
            floor_iri = OBE_FLOOR + '_' + str(uuid.uuid4())
            triples += f"""<{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> <{floor_iri}> . 
                           <{floor_iri}> <{RDF_TYPE}> <{OBE_FLOOR}> . 
                           <{floor_iri}> <{RDFS_COMMENT}> \"{floor_description}\"^^<{XSD_STRING}> . """
        if roof_description: 
            roof_iri = OBE_ROOF + '_' + str(uuid.uuid4())
            triples += f"""<{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> <{roof_iri}> . 
                           <{roof_iri}> <{RDF_TYPE}> <{OBE_ROOF}> . 
                           <{roof_iri}> <{RDFS_COMMENT}> \"{roof_description}\"^^<{XSD_STRING}> . """
        if wall_description: 
            wall_iri = OBE_WALL + '_' + str(uuid.uuid4())
            triples += f"""<{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> <{wall_iri}> . 
                           <{wall_iri}> <{RDF_TYPE}> <{OBE_WALL}> . 
                           <{wall_iri}> <{RDFS_COMMENT}> \"{wall_description}\"^^<{XSD_STRING}> . """
        if windows_description: 
            windows_iri = OBE_WINDOWS + '_' + str(uuid.uuid4())
            triples += f"""<{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> <{windows_iri}> . 
                           <{windows_iri}> <{RDF_TYPE}> <{OBE_WINDOWS}> . 
                           <{windows_iri}> <{RDFS_COMMENT}> \"{windows_description}\"^^<{XSD_STRING}> . """
        if floor_area:
            area_iri = KB + 'FloorArea_' + str(uuid.uuid4())
            measure_iri = KB + 'Measure_' + str(uuid.uuid4())
            unit_iri = UNITS_MAPPING[OM_AREA][0]
            unit_symbol = UNITS_MAPPING[OM_AREA][1]
            triples += f"""<{property_iri}> <{OBE_HAS_TOTAL_FLOOR_AREA}> <{area_iri}> . 
                           <{area_iri}> <{RDF_TYPE}> <{OM_AREA}> . 
                           <{area_iri}> <{OM_HAS_VALUE}> <{measure_iri}> . 
                           <{measure_iri}> <{RDF_TYPE}> <{OM_MEASURE}> . 
                           <{measure_iri}> <{OM_NUM_VALUE}> \"{floor_area}\"^^<{XSD_FLOAT}> .
                           <{measure_iri}> <{OM_HAS_UNIT}> <{unit_iri}> . 
                           <{unit_iri}> <{OM_SYMBOL}> \"{unit_symbol}\"^^<{XSD_STRING}> .
                           """

        # Literals
        if uprn: f"<{property_iri}> <{OBE_HAS_IDENTIFIER}> \"{uprn}\"^^<{XSD_STRING}> . "
        if epc_rating: f"<{property_iri}> <{OBE_HAS_ENERGYRATING}> \"{epc_rating}\"^^<{XSD_STRING}> . "
        if rooms: f"<{property_iri}> <{OBE_HAS_NUMBER_ROOMS}> \"{uprn}\"^^<{XSD_INTEGER}> . "
        if epc_lmkkey: f"<{property_iri}> <{OBE_HAS_LATEST_EPC}> \"{epc_lmkkey}\"^^<{XSD_STRING}> . "

        # Close query
        query += f"}}"
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

    else:
        triples = None
    
    return triples


# def filter_stations_in_circle(circle_center: str, circle_radius: str):
#     # Retrieve stationIRIs for stations of interest
#     lat, lon = circle_center.split('#')
#     lat = float(lat)
#     lon = float(lon)
#     circle_radius = float(circle_radius)
#     postgis_client = PostGISClient()
#     iris = postgis_client.get_feature_iris_in_circle(lat, lon, circle_radius)
#     iris = ', '.join(f'<{iri}>' for iri in iris)
#     filter_expression = f'FILTER ( ?station IN ({iris}) )'
    
#     return filter_expression


# def instantiated_metoffice_stations_with_details(circle_center: str = None,
#                                                  circle_radius: str = None) -> str:   
#     if circle_center and circle_radius:
#         # Retrieve only stations in provided circle (radius in km)
#         filter_expression = filter_stations_in_circle(circle_center, circle_radius)
#     else:
#         # Returns query to retrieve all instantiated station details
#         filter_expression = ''
    
#     # Construct query
#     query = f"""
#         SELECT ?stationID ?station ?label ?elevation ?dataIRI_obs ?dataIRI_fc
#         WHERE {{
#         {filter_expression}
#         ?station <{RDF_TYPE}> <{EMS_REPORTING_STATION}> ;
#                     <{EMS_DATA_SOURCE}> "Met Office DataPoint" ;
#                     <{EMS_HAS_IDENTIFIER}> ?stationID .
#         OPTIONAL {{ ?station <{RDFS_LABEL}> ?label }}
#         OPTIONAL {{ ?station <{EMS_HAS_OBSERVATION_ELEVATION}> ?elevation }}
#         OPTIONAL {{ ?station <{EMS_REPORTS}>/<{OM_HAS_VALUE}> ?dataIRI_obs }}
#         OPTIONAL {{ ?station <{EMS_REPORTS}>/<{EMS_HAS_FORECASTED_VALUE}> ?dataIRI_fc }}

#             }}
#     """
    
#     return query


# def split_insert_query(triples: str, max: int):
#     """"
#         Split large SPARQL insert query into list of smaller chunks (primarily
#         to avoid heap size/memory issues when executing large SPARQL updated)

#         Arguments
#             triples - original SPARQL update string with individual triples 
#                       separated by " . ", i.e. in the form
#                       <s1> <p1> <o1> .
#                       <s2> <p2> <o2> . 
#                       ...
#             max - maximum number of triples per SPARQL update

#     """

#     # Initialise list of return queries
#     queries = []

#     # Split original query every max'th occurrence of " . " and append queries list
#     splits = triples.split(' . ')
#     cutoffs = list(range(0, len(splits), max))
#     cutoffs.append(len(splits))
#     for i in range(len(cutoffs)-1):
#         start = cutoffs[i]
#         end = cutoffs[i+1]
#         query = ' . '.join([t for t in splits[start:end]])
#         query = " INSERT DATA { " + query + " } "
#         queries.append(query)
    
#     return queries
