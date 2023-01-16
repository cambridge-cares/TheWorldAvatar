################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Sep 2022                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

import uuid
import pandas as pd

from py4jps import agentlogging

from agent.datamodel.iris import *
from agent.datamodel.data_mapping import UNITS_MAPPING
#from epcdata.kgutils.stackclients import PostGISClient

# Initialise logger
logger = agentlogging.get_logger("prod")


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


def get_latest_epc_and_property_iri_for_uprn(uprn: str) -> str:
    # Get latest instantiated EPC (i.e. individual lodgement identifier) for UPRN
    query = f"""
        SELECT ?property ?uprn ?certificate
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


def get_ocgml_uprns(uprn: str):
    # Get all UPRNs associated with same building in OntoCityGml as provided uprn
    query = f"""
        SELECT DISTINCT ?uprns
        WHERE {{
            VALUES ?uprn {{ "{uprn}"^^<{XSD_INTEGER}> }}
            ?cityobj ^<{OSID_INTERSECTS_FEATURE}>/<{OSID_HAS_VALUE}> ?uprn ;
  		             ^<{OSID_INTERSECTS_FEATURE}>/<{OSID_HAS_VALUE}> ?uprns
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def get_parent_building(uprns: list):
    # Get IRI of instantiated parent building of UPRNs (i.e. identifiers of flats)
    values = '", "'.join(uprns)
    values = values.replace(',', f'^^<{XSD_STRING}>')
    values = f'"{values}"^^<{XSD_STRING}>'
    
    query = f"""
        SELECT DISTINCT ?building
        WHERE {{
            VALUES ?uprn {{ {values} }}
            ?property <{OBE_HAS_IDENTIFIER}> ?uprn ;
                      <{OBE_IS_IN}> ?building
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def get_children_and_parent_building_properties():
    # Get IRIs of instantiated properties and parent buildings as well as
    # key information of properties to "summarize"
    query = f"""
        SELECT DISTINCT ?parent_iri ?parent_id ?property_iri ?address_iri ?postcode_iri ?district_iri
                        ?addr_street ?addr_number ?addr_bldg_name ?addr_unit_name ?epc_rating ?rooms 
                        ?usage_iri ?usage_label ?property_type_iri ?built_form_iri ?construction_start
                        ?construction_end ?floor_area ?floor_description ?roof_description 
                        ?wall_description  ?windows_description
        WHERE {{
            ?property_iri <{OBE_IS_IN}> ?parent_iri ;
                      <{OBE_HAS_ADDRESS}> ?address_iri .
            ?address_iri <{OBE_HAS_POSTALCODE}> ?postcode_iri ;
                     <{OBE_HAS_ADMIN_DISTRICT}> ?district_iri .
            OPTIONAL {{ ?address_iri <{ICONTACT_HAS_STREET}> ?addr_street }}
            OPTIONAL {{ ?address_iri <{ICONTACT_HAS_STREET_NUMBER}> ?addr_number }}
            OPTIONAL {{ ?address_iri <{ICONTACT_HAS_BUILDING}> ?addr_bldg_name }}
            OPTIONAL {{ ?address_iri <{OBE_HAS_UNIT_NAME}> ?addr_unit_name }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_ENERGYRATING}> ?epc_rating }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_NUMBER_ROOMS}> ?rooms }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_USAGE}> ?usage_iri }}
            OPTIONAL {{ ?property_iri <{RDFS_LABEL}> ?usage_label }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_PROPERTY_TYPE}> ?property_type_iri }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_BUILT_FORM}> ?built_form_iri }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_CONSTRUCTION_DATE}>/<{TIME_HAS_BEGINNING}>/<{TIME_IN_DATETIME_STAMP}> ?construction_start }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_CONSTRUCTION_DATE}>/<{TIME_HAS_END}>/<{TIME_IN_DATETIME_STAMP}> ?construction_end }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?component1 .
                        ?component1 <{RDF_TYPE}> <{OBE_FLOOR}> ;
                                    <{RDFS_COMMENT}> ?floor_description }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?component2 .                                    
                        ?component2 <{RDF_TYPE}> <{OBE_ROOF}> ; 
                                    <{RDFS_COMMENT}> ?roof_description }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?component3 .  
                        ?component3 <{RDF_TYPE}> <{OBE_WALL}> ; 
                                    <{RDFS_COMMENT}> ?wall_description }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?component4 .  
                        ?component4 <{RDF_TYPE}> <{OBE_WINDOWS}> ; 
                                    <{RDFS_COMMENT}> ?windows_description }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_TOTAL_FLOOR_AREA}>/<{OM_HAS_VALUE}>/<{OM_NUM_VALUE}> ?floor_area }}
            OPTIONAL {{ ?parent_iri <{OBE_HAS_IDENTIFIER}> ?parent_id }}
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def get_children_and_parent_building_properties_non_domestic():
    # Get IRIs of instantiated properties and parent buildings as well as
    # key information of properties to "summarize"
    query = f"""
        SELECT DISTINCT ?parent_iri ?parent_id ?property_iri ?address_iri ?postcode_iri ?district_iri
                        ?addr_street ?addr_number ?addr_bldg_name ?addr_unit_name ?epc_rating  
                        ?usage_iri ?usage_label ?property_type_iri ?floor_area
        WHERE {{
            ?property_iri <{OBE_IS_IN}> ?parent_iri ;
                      <{OBE_HAS_ADDRESS}> ?address_iri .
            ?address_iri <{OBE_HAS_POSTALCODE}> ?postcode_iri ;
                     <{OBE_HAS_ADMIN_DISTRICT}> ?district_iri .
            OPTIONAL {{ ?address_iri <{ICONTACT_HAS_STREET}> ?addr_street }}
            OPTIONAL {{ ?address_iri <{ICONTACT_HAS_STREET_NUMBER}> ?addr_number }}
            OPTIONAL {{ ?address_iri <{ICONTACT_HAS_BUILDING}> ?addr_bldg_name }}
            OPTIONAL {{ ?address_iri <{OBE_HAS_UNIT_NAME}> ?addr_unit_name }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_ENERGYRATING}> ?epc_rating }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_USAGE}> ?usage_iri }}
            OPTIONAL {{ ?property_iri <{RDFS_LABEL}> ?usage_label }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_PROPERTY_TYPE}> ?property_type_iri }}
            OPTIONAL {{ ?property_iri <{OBE_HAS_TOTAL_FLOOR_AREA}>/<{OM_HAS_VALUE}>/<{OM_NUM_VALUE}> ?floor_area }}
            OPTIONAL {{ ?parent_iri <{OBE_HAS_IDENTIFIER}> ?parent_id }}
            
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query

def get_matched_buildings() -> str:
    # Retrieve all OntoBuiltEnv building with a OntoCityGml representations
    query = f"""
        SELECT DISTINCT ?obe_bldg
        WHERE {{
            ?obe_bldg <{OBE_HAS_OCGML_REPRESENTATION}> ?ocgml_bldg .
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query

def get_buildings_usage(bldg_iri: str):
    # Retrieve usages of OntoBuiltEnv buildings 
    query = f"""
        SELECT ?usage
        WHERE {{
          <{bldg_iri}> <{OBE_HAS_USAGE}> ?usage .
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def get_matched_ocgml_information(obe_endpoint, ocgml_endpoint, bldg_iris=[]) -> str:
    # Retrieved relevant OCGML information for matched buildings (i.e. OBE building IRIs)
    
    # Create list of IRIs of interest
    values = '> <'.join(bldg_iris)
    values = '<' + values + '>'

    query = f"""
        SELECT DISTINCT ?obe_bldg ?surf (DATATYPE(?geom) as ?datatype) ?geom ?height
        WHERE {{
            SERVICE <{obe_endpoint}> {{
                VALUES ?obe_bldg {{ {values} }}
                 ?obe_bldg <{OBE_HAS_OCGML_REPRESENTATION}> ?ocgml_bldg .
                }}
            SERVICE <{ocgml_endpoint}> {{
                ?ocgml_bldg <{OCGML_BLDG_HEIGHT}> ?height ;
                            <{OCGML_FOOTPRINT}> ?footprint .
                ?surf <{OCGML_ROOT_ID}> ?footprint ;
       		          <{OCGML_GEOM_TYPE}> ?geom .
       		    FILTER (!isBlank(?geom))
            }}
        }}
    """

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def get_ocgml_crs():
    # Retrieve coordinate reference system (from OCGML endpoint)
    query = f"""SELECT ?crs
        WHERE {{ ?s <{OCGML_SRSNAME}> ?crs . }}
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


def instantiate_epc_data(property_iri: str = None, uprn: str = None, parent_iri: str = None,
                         address_iri: str = None, addr_street: str = None, addr_number: str = None,
                         addr_bldg_name: str = None, addr_unit_name: str = None,
                         postcode_iri:str = None, district_iri: str = None,
                         built_form_iri: str = None, property_type_iri: str = None,
                         usage_iri: str = None, usage_label: str = None,
                         construction_start: str = None, construction_end: str = None,
                         floor_description: str = None, roof_description: str = None, 
                         wall_description: str = None, windows_description: str = None,  
                         floor_area: float = None, rooms: int = None,
                         epc_rating: str = None, epc_lmkkey: str = None) -> str:
    # Returns triples to instantiate EPC data for single property
    
    if property_iri and uprn:
        # Start INSERT query
        triples = f"""
            INSERT DATA {{
        """

        # Property definition
        if 'Building' in property_iri:
            triples += f"<{property_iri}> <{RDF_TYPE}> <{OBE_BUILDING}> . "
        else:
            triples += f"<{property_iri}> <{RDF_TYPE}> <{OBE_FLAT}> . "
        if parent_iri: triples += f"""<{property_iri}> <{OBE_IS_IN}> <{parent_iri}> . 
                                      <{parent_iri}> <{RDF_TYPE}> <{OBE_BUILDING}> . """

        # Postal code and admin district
        if address_iri: 
            triples += f"""<{property_iri}> <{OBE_HAS_ADDRESS}> <{address_iri}> . 
                           <{address_iri}> <{RDF_TYPE}> <{ICONTACT_ADDRESS}> . """
            if addr_street: triples += f"<{address_iri}> <{ICONTACT_HAS_STREET}> \"{addr_street}\"^^<{XSD_STRING}> . "
            if addr_number: triples += f"<{address_iri}> <{ICONTACT_HAS_STREET_NUMBER}> \"{addr_number}\"^^<{XSD_STRING}> . "
            if addr_bldg_name: triples += f"<{address_iri}> <{ICONTACT_HAS_BUILDING}> \"{addr_bldg_name}\"^^<{XSD_STRING}> . "
            if addr_unit_name: triples += f"<{address_iri}> <{OBE_HAS_UNIT_NAME}> \"{addr_unit_name}\"^^<{XSD_STRING}> . "
            if postcode_iri: triples += f"<{address_iri}> <{OBE_HAS_POSTALCODE}> <{postcode_iri}> . "
            if district_iri: triples += f"<{address_iri}> <{OBE_HAS_ADMIN_DISTRICT}> <{district_iri}> . "
        if district_iri: triples += f"<{property_iri}> <{OBE_LOCATEDIN}> <{district_iri}> . "
        
        # Instances        
        if property_type_iri: triples += f"<{property_iri}> <{OBE_HAS_PROPERTY_TYPE}> <{property_type_iri}> . "
        if built_form_iri: triples += f"<{property_iri}> <{OBE_HAS_BUILT_FORM}> <{built_form_iri}> . "
        if usage_iri:
            if (isinstance(usage_iri, list)):
                for item in usage_iri:
                    triples += f"<{property_iri}> <{OBE_HAS_USAGE}> <{item}> . "
            else:
                triples += f"<{property_iri}> <{OBE_HAS_USAGE}> <{usage_iri}> . "
            if usage_label: triples += f"<{property_iri}> <{RDFS_LABEL}> \"{usage_label}\"^^<{XSD_STRING}> . "
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
        if uprn:  triples += f"<{property_iri}> <{OBE_HAS_IDENTIFIER}> \"{uprn}\"^^<{XSD_STRING}> . "
        if epc_rating:  triples += f"<{property_iri}> <{OBE_HAS_ENERGYRATING}> \"{epc_rating}\"^^<{XSD_STRING}> . "
        if rooms:  triples += f"<{property_iri}> <{OBE_HAS_NUMBER_ROOMS}> \"{rooms}\"^^<{XSD_INTEGER}> . "
        if epc_lmkkey:  triples += f"<{property_iri}> <{OBE_HAS_LATEST_EPC}> \"{epc_lmkkey}\"^^<{XSD_STRING}> . "

        # Close query
        triples += f"}}"
        # Remove unnecessary whitespaces
        triples = ' '.join(triples.split())

    else:
        triples = None

    return triples


def update_epc_data(property_iri: str = None,
                    built_form_iri: str = None, property_type_iri: str = None,
                    usage_iri: str = None, usage_label: str = None,
                    construction_end: str = None, 
                    floor_description: str = None, roof_description: str = None, 
                    wall_description: str = None, windows_description: str = None,  
                    floor_area: float = None, rooms: int = None,
                    epc_rating: str = None, epc_lmkkey: str = None) -> str:
    # Returns DELETE / INSERT query to update instantiated EPC data

    # Not all building data is assumed to be "updatable", i.e. assumed constant:
    #   - UPRN and Parent building
    #   - Address data incl. links to postcode and admin district
    #   - Construction start date

    if property_iri:
        # Start query building blocks
        delete = f"""
            DELETE {{
                <{property_iri}> <{OBE_HAS_LATEST_EPC}> ?lmkkey ;
                                 <{OBE_HAS_ENERGYRATING}> ?epc_rating ;
                                 <{OBE_HAS_NUMBER_ROOMS}> ?rooms ;
                                 <{OBE_HAS_USAGE}> ?usage_iri ;
                                 <{OBE_HAS_PROPERTY_TYPE}> ?property_type ;
                                 <{OBE_HAS_BUILT_FORM}> ?built_form ;
                                 <{RDFS_LABEL}> ?usage_label .
                ?end_iri <{TIME_IN_DATETIME_STAMP}> ?end_time .
                ?floor <{RDFS_COMMENT}> ?floor_description .
                ?roof <{RDFS_COMMENT}> ?roof_description .
                ?wall <{RDFS_COMMENT}> ?wall_description .
                ?windows <{RDFS_COMMENT}> ?windows_description .
                ?area_measure <{OM_NUM_VALUE}> ?area .
        }}
        """

        insert = f"""
            INSERT {{
        """
        if epc_lmkkey:  insert += f"<{property_iri}> <{OBE_HAS_LATEST_EPC}> \"{epc_lmkkey}\"^^<{XSD_STRING}> . "
        if epc_rating: insert += f"<{property_iri}> <{OBE_HAS_ENERGYRATING}> \"{epc_rating}\"^^<{XSD_STRING}> ."
        if rooms: insert += f"<{property_iri}> <{OBE_HAS_NUMBER_ROOMS}> \"{rooms}\"^^<{XSD_INTEGER}> . "
        if usage_iri:
            if (isinstance(usage_iri, list)):
                for usage in usage_iri:
                    insert += f"<{property_iri}> <{OBE_HAS_USAGE}> <{usage}> . "
            else:
                insert += f"<{property_iri}> <{OBE_HAS_USAGE}> <{usage_iri}> . "                    
            if usage_label: insert += f"<{property_iri}> <{RDFS_LABEL}> \"{usage_label}\"^^<{XSD_STRING}> . "
        if property_type_iri: insert += f"<{property_iri}> <{OBE_HAS_PROPERTY_TYPE}> <{property_type_iri}> . "
        if built_form_iri: insert += f"<{property_iri}> <{OBE_HAS_BUILT_FORM}> <{built_form_iri}> . "
        if construction_end: insert += f"?end_iri <{TIME_IN_DATETIME_STAMP}> \"{construction_end}\"^^<{XSD_DATETIMESTAMP}> . "
        
        if floor_description: insert += f"?floor <{RDFS_COMMENT}> \"{floor_description}\"^^<{XSD_STRING}> . "
        if roof_description: insert += f"?roof <{RDFS_COMMENT}> \"{roof_description}\"^^<{XSD_STRING}> . "
        if wall_description: insert += f"?wall <{RDFS_COMMENT}> \"{wall_description}\"^^<{XSD_STRING}> . "        
        if windows_description: insert += f"?windows <{RDFS_COMMENT}> \"{windows_description}\"^^<{XSD_STRING}> . "
        if floor_area: insert += f"?area_measure <{OM_NUM_VALUE}> \"{floor_area}\"^^<{XSD_FLOAT}> . "
        insert += f"}} "

        where =f"""WHERE {{
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_LATEST_EPC}> ?lmkkey }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_ENERGYRATING}> ?epc_rating }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_NUMBER_ROOMS}> ?rooms }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_USAGE}> ?usage_iri }} 
                OPTIONAL {{ <{property_iri}> <{RDFS_LABEL}> ?usage_label }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_PROPERTY_TYPE}> ?property_type }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_BUILT_FORM}> ?built_form }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_CONSTRUCTION_DATE}>/<{TIME_HAS_END}> ?end_iri . 
                            ?end_iri <{TIME_IN_DATETIME_STAMP}> ?end_time . }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?floor .
                            ?floor <{RDF_TYPE}> <{OBE_FLOOR}> ;
                                   <{RDFS_COMMENT}> ?floor_description . }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?roof .
                            ?roof <{RDF_TYPE}> <{OBE_ROOF}> ; 
                                  <{RDFS_COMMENT}> ?roof_description . }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?wall .
                            ?wall <{RDF_TYPE}> <{OBE_WALL}> ; 
                                  <{RDFS_COMMENT}> ?wall_description . }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_CONSTRUCTION_COMPONENT}> ?windows .
                            ?windows <{RDF_TYPE}> <{OBE_WINDOWS}> ; 
                                     <{RDFS_COMMENT}> ?windows_description . }}
                OPTIONAL {{ <{property_iri}> <{OBE_HAS_TOTAL_FLOOR_AREA}>/<{OM_HAS_VALUE}> ?area_measure . 
                            ?area_measure <{OM_NUM_VALUE}> ?area . }}
            }}
        """

        query = delete + insert + where
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

    else:
        query = None
    return query


def delete_old_building_elevation(obe_bldg_iris):
    # Delete (potentially) outdated OntoBuiltEnv building elevation triples

    # Create list of IRIs of interest
    values = '> <'.join(obe_bldg_iris)
    values = '<' + values + '>'

    query = f"""        
        DELETE {{
            ?bldg_iri <{OBE_HAS_GROUND_ELEVATION}> ?old_elev .
            ?old_elev <{RDF_TYPE}> ?old_quant_type ;
                        <{OM_HAS_VALUE}> ?old_measure .
            ?old_measure <{RDF_TYPE}> ?old_measure_type ; 
                            <{OM_NUM_VALUE}> ?old_value ;
                            <{OM_HAS_UNIT}> ?old_unit .
            ?old_unit <{RDF_TYPE}> ?old_unit_type ;
                        <{OM_SYMBOL}> ?old_unit_symbol 
        }}
        WHERE {{
                VALUES ?bldg_iri {{ {values} }}
                ?bldg_iri <{OBE_HAS_OCGML_REPRESENTATION}> ?ocgml_bldg .
                OPTIONAL {{ ?bldg_iri <{OBE_HAS_GROUND_ELEVATION}> ?old_elev 
                OPTIONAL {{ ?old_elev <{RDF_TYPE}> ?old_quant_type ;
                                      <{OM_HAS_VALUE}> ?old_measure .
                            ?old_measure <{RDF_TYPE}> ?old_measure_type ;
                                         <{OM_NUM_VALUE}> ?old_value }}
                OPTIONAL {{ ?old_measure <{OM_HAS_UNIT}> ?old_unit .
                            ?old_unit <{RDF_TYPE}> ?old_unit_type ;
                                      <{OM_SYMBOL}> ?old_unit_symbol }}
                }}
        }}
    """

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query


def instantiate_building_elevation(elevation_data):
    # Instantiate building elevation (as retrieved from OntoCityGml instances) 
    # according to OntoBuiltEnv
    # elevation_data: [{'unit': '...', 'obe_bldg': '...', 'elevation': '...'}, ...]

    # Initialise data insert query
    query = f"INSERT DATA {{"

    for d in elevation_data:
        # Create IRIs
        elevation = KB + 'GroundElevation_' + str(uuid.uuid4())
        measure = KB + 'Measure_' + str(uuid.uuid4())
        if d['unit'] == 'm':
            unit = OM_M
        else:
            unit = None
            logger.warn('Building elevation specified in unknown unit, i.e. not metres.')

        # Add triples to instantiate
        query += f"""
            <{d['obe_bldg']}> <{OBE_HAS_GROUND_ELEVATION}> <{elevation}> . 
            <{elevation}> <{RDF_TYPE}> <{OM_HEIGHT}> . 
            <{elevation}> <{OM_HAS_VALUE}> <{measure}> . 
            <{measure}> <{RDF_TYPE}> <{OM_MEASURE}> . 
            <{measure}> <{OM_NUM_VALUE}> "{d['elevation']}"^^<{XSD_FLOAT}>  . 
        """
        if unit:
            query += f"""
                <{measure}> <{OM_HAS_UNIT}> <{unit}> . 
                <{unit}> <{RDF_TYPE}> <{OM_UNIT}> . 
                <{unit}> <{OM_SYMBOL}> "{d['unit']}"^^<{XSD_STRING}> . 
            """

    # Close query
    query += f"}}"

    # Remove unnecessary whitespaces
    query = ' '.join(query.split())

    return query
