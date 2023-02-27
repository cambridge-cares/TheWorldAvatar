################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to provide templates for (frequently)
# required SPARQL queries

import uuid

from agent.datamodel import *

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


#
# EXTERNAL SPARQL QUERIES
#
def ons_county(county_name: str) -> str:
    # Retrieve county IRI from ONS API
    query = f"""
        SELECT DISTINCT *
        WHERE {{
        ?district_iri <{RDF_TYPE}> <{ONS_GEOGRAPHY}> ;
                      <{ONS_MEMBER_OF}> <{ONS_COUNTIES}> ; 
                      <{ONS_NAME}> "{county_name}" .   
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query

#
# INTERNAL SPARQL QUERIES
#
def get_all_flood_warnings() -> str:
    # Retrieve all instantiated flood warnings with latest update times
    query = f"""
        SELECT ?warning_iri ?t1 ?t2 ?t3
        WHERE {{
        ?warning_iri <{RDF_TYPE}> <{RT_FLOOD_ALERT_OR_WARNING}> . 
        OPTIONAL {{ ?warning_iri <{RT_TIME_RAISED}> ?t1 . }}
        OPTIONAL {{ ?warning_iri <{RT_TIME_MESSAGE_CHANGED}> ?t2 . }}
        OPTIONAL {{ ?warning_iri <{RT_TIME_SEVERITY_CHANGED}> ?t3 . }}
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def get_all_flood_areas() -> str:
    # Retrieve all instantiated flood areas with associated locations
    query = f"""
        SELECT ?area_iri ?location_iri
        WHERE {{
        ?area_iri <{RDF_TYPE}> <{RT_FLOOD_AREA}> ; 
                  <{FLOOD_HAS_LOCATION}> ?location_iri . 
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


def get_associated_flood_area(flood_warning_iri: str) -> str:
    # Retrieve flood area associated with flood warning
    query = f"""
        SELECT ?area_iri
        WHERE {{
            <{flood_warning_iri}> ^<{RT_CURRENT_WARNING}> ?area_iri . 
        }}
    """
    # Remove unnecessary whitespaces
    query = ' '.join(query.split())
    return query


#
# INTERNAL SPARQL UPDATES
#
def flood_area_instantiation_triples(area_uri: str, location_iri: str, admin_district_iri: str, 
                                     areal_extend_iri: str, waterbody_iri: str, water_body_type: str,
                                     history_iri: str, area_types: list = [], county_iri: str = None,
                                     label: str = None, area_identifier: str = None, 
                                     water_body_label: str = None, ) -> tuple:
    """
    Create INSERT triples to instantiate single flood area (to be placed inside a SPARQL INSERT DATA block)

    Arguments:
        area_uri (str): IRI of flood area - use URI provided by EA API
        area_types (str): flood area types - use URIs provided by EA API
        county_iri (str): IRI of county as retrieved from ONS API
        location_iri (str): IRI of Location associated with flood area (and potential flood event)
        admin_district_iri (str): IRI of administrative district associated with Location
        areal_extend_iri (str): IRI of polygon describing areal extend of flood area 
                                (i.e. used as 'iri' in PostGIS)
        waterbody_iri (str): IRI of water body associated with flood area
    """

    if area_uri:
        # Retrieve applicable waterbody type
        waterbody_type = WATERBODIES_IRIS[water_body_type]

        # Object properties
        # Define each area as general FloodArea type
        triples = f"""
            <{area_uri}> <{RDF_TYPE}> <{RT_FLOOD_AREA}> . 
            <{area_uri}> <{FLOOD_HAS_LOCATION}> <{location_iri}> . 
            <{area_uri}> <{FLOOD_HAS_ALERT_OR_WARNING_HISTORY}> <{history_iri}> . 
            <{history_iri}> <{RDF_TYPE}> <{FLOOD_FLOOD_ALERT_OR_WARNING_HISTORY}> . 
            <{location_iri}> <{RDF_TYPE}> <{FLOOD_LOCATION}> . 
            <{location_iri}> <{FLOOD_HAS_ADMINISTRATIVE_DISTRICT}> <{admin_district_iri}> . 
            <{admin_district_iri}> <{RDF_TYPE}> <{FLOOD_ADMINISTRATIVE_DISTRICT}> . 
            <{location_iri}> <{FLOOD_HAS_AREAL_EXTENT}> <{areal_extend_iri}> . 
            <{areal_extend_iri}> <{RDF_TYPE}> <{FLOOD_AREAL_EXTENT_POLYGON}> . 
            <{area_uri}> <{FLOOD_ATTACHED_WATERBODY}> <{waterbody_iri}> . 
            <{waterbody_iri}> <{RDF_TYPE}> <{waterbody_type}> . 
        """
        # Add ONS county IRI if available (i.e. could be retrieved unambiguously)
        if county_iri: triples += f"<{admin_district_iri}> <{OWL_SAMEAS}> <{county_iri}> . "
        # Add more specific area types (i.e. AlertArea, WarningArea) as retrieved from EA API
        for t in area_types:
            triples += f"<{area_uri}> <{RDF_TYPE}> <{t}> . "
        
        # Data properties
        if label: triples += f"<{area_uri}> <{RDFS_LABEL}> \"{label}\"^^<{XSD_STRING}> . "
        if area_identifier: triples += f"<{area_uri}> <{FLOOD_HAS_AREA_IDENTIFIER}> \"{area_identifier}\"^^<{XSD_STRING}> . "
        if water_body_label: triples += f"<{waterbody_iri}> <{RDFS_LABEL}> \"{water_body_label}\"^^<{XSD_STRING}> . "
    else:
        triples = ""
    
    # Remove unnecessary whitespaces
    triples = ' '.join(triples.split())
    
    return triples


def flood_warning_instantiation_triples(warning_uri: str = None, area_uri: str = None, flood_event_iri: str = None,
                                        location_iri: str = None, severity: str = None, label: str = None,
                                        message: str = None, timeRaised: str = None, timeMsgChanged: str = None,
                                        timeSevChanged: str = None) -> tuple:
    """
    Create INSERT triples to instantiate single flood warning (to be placed inside a SPARQL INSERT DATA block)

    Arguments:
        warning_uri (str): IRI of flood warning - use URI provided by EA API
        area_uri (str): IRI of flood area - use URI provided by EA API
        flood_event_iri (str): IRI of potential flood event
        location_iri (str): IRI of location of potential flood event
    """

    if warning_uri:
        # Get applicable severity IRI from OntoFlood ABox
        severity_iri = SEVERITY_IRIS[severity.lower()]
        
        # Object properties
        triples = f"""
            <{warning_uri}> <{RDF_TYPE}> <{RT_FLOOD_ALERT_OR_WARNING}> . 
            <{warning_uri}> <{FLOOD_HAS_SEVERITY}> <{severity_iri}> . 
            <{area_uri}> <{RT_CURRENT_WARNING}> <{warning_uri}> . 
            <{warning_uri}> <{FLOOD_WARNS_ABOUT}> <{flood_event_iri}> . 
            <{flood_event_iri}> <{RDF_TYPE}> <{ENVO_FLOOD}> . 
            <{flood_event_iri}> <{FLOOD_HAS_LOCATION}> <{location_iri}> . 
            """
        #TODO: Instantiation of flood event type to be refined in the future, i.e.
        #      envo:riverineflood, envo:coastalflood, etc. instead of just envo:flood
        
        # Data properties
        if label: triples += f"<{warning_uri}> <{RDFS_LABEL}> \"{label}\"^^<{XSD_STRING}> . "
        if message: triples += f"<{warning_uri}> <{RT_MESSAGE}> \"{message}\"^^<{XSD_STRING}> . "
        if timeRaised: triples += f"<{warning_uri}> <{RT_TIME_RAISED}> \"{timeRaised}\"^^<{XSD_DATETIME}> . "
        if timeMsgChanged: triples += f"<{warning_uri}> <{RT_TIME_MESSAGE_CHANGED}> \"{timeMsgChanged}\"^^<{XSD_DATETIME}> . "
        if timeSevChanged: triples += f"<{warning_uri}> <{RT_TIME_SEVERITY_CHANGED}> \"{timeSevChanged}\"^^<{XSD_DATETIME}> . "
    else:
        triples = ""
    
    # Remove unnecessary whitespaces
    triples = ' '.join(triples.split())
    
    return triples


def update_flood_warning(warning_uri: str = None, severity: str = None, label: str = None, 
                         message: str = None, timeRaised: str = None, timeMsgChanged: str = None,
                         timeSevChanged: str = None) -> str:
    # Returns DELETE / INSERT query to update instantiated flood warning

    # Not all flood warning/alert info is considered "updatable", i.e. the following
    # relations are assumed constant (incl. all downstream relations):
    #   - associated flood area
    #   - associated flood event

    if warning_uri:
        # Get applicable severity IRI from OntoFlood ABox
        severity_iri = SEVERITY_IRIS[severity.lower()]

        # Create query building blocks
        delete = f"""
            DELETE {{
                <{warning_uri}> <{FLOOD_HAS_SEVERITY}> ?severity_iri ; 
                                <{RDFS_LABEL}> ?warning_label ; 
                                <{RT_MESSAGE}> ?warning_message ; 
                                <{RT_TIME_RAISED}> ?time_raise ; 
                                <{RT_TIME_MESSAGE_CHANGED}> ?time_message ; 
                                <{RT_TIME_SEVERITY_CHANGED}> ?time_severity ; 
        }}
        """

        insert = f"""
            INSERT {{
        """
        # Object properties
        insert += f"<{warning_uri}> <{FLOOD_HAS_SEVERITY}> <{severity_iri}> . "        
        # Data properties
        if label: insert += f"<{warning_uri}> <{RDFS_LABEL}> \"{label}\"^^<{XSD_STRING}> . "
        if message: insert += f"<{warning_uri}> <{RT_MESSAGE}> \"{message}\"^^<{XSD_STRING}> . "
        if timeRaised: insert += f"<{warning_uri}> <{RT_TIME_RAISED}> \"{timeRaised}\"^^<{XSD_DATETIME}> . "
        if timeMsgChanged: insert += f"<{warning_uri}> <{RT_TIME_MESSAGE_CHANGED}> \"{timeMsgChanged}\"^^<{XSD_DATETIME}> . "
        if timeSevChanged: insert += f"<{warning_uri}> <{RT_TIME_SEVERITY_CHANGED}> \"{timeSevChanged}\"^^<{XSD_DATETIME}> . "
        insert += f"}} "

        where =f"""WHERE {{
                <{warning_uri}> <{RDF_TYPE}> <{RT_FLOOD_ALERT_OR_WARNING}> ; 
                                <{FLOOD_HAS_SEVERITY}> ?severity_iri . 
                OPTIONAL {{ <{warning_uri}> <{RDFS_LABEL}> ?warning_label }}
                OPTIONAL {{ <{warning_uri}> <{RT_MESSAGE}> ?warning_message }}
                OPTIONAL {{ <{warning_uri}> <{RT_TIME_RAISED}> ?time_raise }}
                OPTIONAL {{ <{warning_uri}> <{RT_TIME_MESSAGE_CHANGED}> ?time_message }} 
                OPTIONAL {{ <{warning_uri}> <{RT_TIME_SEVERITY_CHANGED}> ?time_severity }}
            }}
        """

        query = delete + insert + where
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

    else:
        query = None

    return query


def delete_flood_warning(warning_uri: str = None) -> str:
    # Returns DELETE query to delete instantiated flood warning incl. ALL 
    # obsolete downstream relations (also including derivation markup);
    # associated flood areas are not affected, only relation is deleted

    if warning_uri:
        # Create delete
        query = f"""
            DELETE {{
                <{warning_uri}> <{RDF_TYPE}> <{RT_FLOOD_ALERT_OR_WARNING}> ; 
                                <{FLOOD_HAS_SEVERITY}> ?severity_iri ; 
                                <{FLOOD_WARNS_ABOUT}> ?flood_event_iri ;
                                <{RDFS_LABEL}> ?warning_label ; 
                                <{RT_MESSAGE}> ?warning_message ; 
                                <{RT_TIME_RAISED}> ?time_raise ; 
                                <{RT_TIME_MESSAGE_CHANGED}> ?time_message ; 
                                <{RT_TIME_SEVERITY_CHANGED}> ?time_severity . 
                ?area_iri <{RT_CURRENT_WARNING}> <{warning_uri}> . 
                ?flood_event_iri <{RDF_TYPE}> <{ENVO_FLOOD}> ;
                                 <{FLOOD_HAS_LOCATION}> ?location_iri ;  
                                 <{FLOOD_RESULTS_IN}> ?impact_iri ; 
                                 <{FLOOD_AFFECTS}> ?affected_iri . 

                ?impact_iri <{RDF_TYPE}> <{FLOOD_IMPACT}> ; 
                            <{FLOOD_HAS_CLASSIFICATION}> ?impact ; 
                            <{FLOOD_HAS_MONETARY_VALUE}> ?impact_quantity . 
                ?impact_quantity <{RDF_TYPE}> ?impact_quantity_type ;
                                 <{OM_HAS_VALUE}> ?impact_quantity_value . 
                ?impact_quantity_value <{RDF_TYPE}> <{OM_MEASURE}> ;
                                       <{OM_HAS_UNIT}> ?impact_quantity_unit ; 
                                       <{OM_HAS_NUMERICALVALUE}> ?impact_quantity_num_value .

                ?affected_iri <{RDF_TYPE}> ?affected_type ;
                              <{FLOOD_HAS_TOTAL_COUNT}> ?affected_count ;
                              <{FLOOD_HAS_TOTAL_MONETARY_VALUE}> ?quantity .
                ?quantity <{RDF_TYPE}> ?quantity_type ;
                          <{OM_HAS_VALUE}> ?quantity_value . 
                ?quantity_value <{RDF_TYPE}> <{OM_MEASURE}> ;
                                <{OM_HAS_UNIT}> ?quantity_unit ; 
                                <{OM_HAS_NUMERICALVALUE}> ?quantity_num_value .

            }} WHERE {{
                <{warning_uri}> <{RDF_TYPE}> <{RT_FLOOD_ALERT_OR_WARNING}> ; 
                                <{FLOOD_HAS_SEVERITY}> ?severity_iri ;
                                <{FLOOD_WARNS_ABOUT}> ?flood_event_iri . 
                ?area_iri <{RT_CURRENT_WARNING}> <{warning_uri}> . 
                ?flood_event_iri <{RDF_TYPE}> <{ENVO_FLOOD}> ;
                                 <{FLOOD_HAS_LOCATION}> ?location_iri . 
                OPTIONAL {{ <{warning_uri}> <{RDFS_LABEL}> ?warning_label }}
                OPTIONAL {{ <{warning_uri}> <{RT_MESSAGE}> ?warning_message }}
                OPTIONAL {{ <{warning_uri}> <{RT_TIME_RAISED}> ?time_raise }}
                OPTIONAL {{ <{warning_uri}> <{RT_TIME_MESSAGE_CHANGED}> ?time_message }} 
                OPTIONAL {{ <{warning_uri}> <{RT_TIME_SEVERITY_CHANGED}> ?time_severity }} 

                OPTIONAL {{ ?flood_event_iri <{FLOOD_RESULTS_IN}> ?impact_iri . 
                            ?impact_iri <{RDF_TYPE}> <{FLOOD_IMPACT}> ; 
                                        <{FLOOD_HAS_MONETARY_VALUE}> ?impact_quantity . 
                            OPTIONAL {{ ?impact_iri <{FLOOD_HAS_CLASSIFICATION}> ?impact }} 
                            ?impact_quantity <{RDF_TYPE}> ?impact_quantity_type ;
                                             <{OM_HAS_VALUE}> ?impact_quantity_value . 
                            ?impact_quantity_value <{RDF_TYPE}> <{OM_MEASURE}> ;
                                                   <{OM_HAS_UNIT}> ?impact_quantity_unit ; 
                                                    <{OM_HAS_NUMERICALVALUE}> ?impact_quantity_num_value .
                         }}
                OPTIONAL {{ ?flood_event_iri <{FLOOD_AFFECTS}> ?affected_iri . 
                            ?affected_iri <{RDF_TYPE}> ?affected_type ;
                                          <{FLOOD_HAS_TOTAL_COUNT}> ?affected_count ;
                                          <{FLOOD_HAS_TOTAL_MONETARY_VALUE}> ?quantity .
                            ?quantity <{RDF_TYPE}> ?quantity_type ;
                                      <{OM_HAS_VALUE}> ?quantity_value . 
                            ?quantity_value <{RDF_TYPE}> <{OM_MEASURE}> ;
                                            <{OM_HAS_UNIT}> ?quantity_unit ; 
                                            <{OM_HAS_NUMERICALVALUE}> ?quantity_num_value .
                         }}
            }}
        """
        #TODO: include deletion of derivation markup (at end of lifetime of warning)

        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

    else:
        query = None

    return query