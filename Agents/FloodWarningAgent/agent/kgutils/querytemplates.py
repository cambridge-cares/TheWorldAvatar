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


#
# INTERNAL SPARQL UPDATES
#
def flood_area_instantiation_triples(area_uri: str, area_types: list, county_iri: str,
                                     location_iri: str, admin_district_iri: str,
                                     waterbody_iri: str, water_body_type: str, label: str, 
                                     area_identifier: str, water_body_label: str,
                                     history_iri: str) -> tuple:
    """
    Create INSERT triples to instantiate single flood area (to be placed inside a SPARQL INSERT DATA block)

    Arguments:
        area_uri (str): IRI of flood area - use URI provided by EA API
        area_types (str): flood area types - use URIs provided by EA API
        county_iri (str): IRI of county as retrieved from ONS API
        location_iri (str): IRI of Location associated with flood area (and potential flood event)
        admin_district_iri (str): IRI of administrative district associated with Location
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


def flood_warning_instantiation_triples(warning_uri: str, area_uri: str, flood_event_iri: str,
                                        location_iri: str, severity: str, label: str, message: str, 
                                        timeRaised: str, timeMsgChanged: str, timeSevChanged: str) -> tuple:
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