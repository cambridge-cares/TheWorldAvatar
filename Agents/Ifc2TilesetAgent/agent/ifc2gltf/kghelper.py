"""
# Author: qhouyee, picas9dan #

This module retrieves metadata from the knowledge graph.
"""

# Third-party imports
from itertools import count
import pandas as pd
from py4jps import agentlogging

# Self imports
from agent.utils import find_word
from agent.kgutils import *

# Retrieve logger
logger = agentlogging.get_logger("dev")


def create_metadata_query():
    """Creates the SPARQL query for retrieving asset metadata."""
    return """\
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX bot:<https://w3id.org/bot#>
PREFIX ontobim:<https://www.theworldavatar.com/kg/ontobim/>
PREFIX ontobuildingstructure:<https://www.theworldavatar.com/kg/ontobuildingstructure/>
SELECT ?iri ?uid ?name WHERE {
  ?iri ontobim:hasIfcRepresentation ?modelRep.
  ?modelRep rdf:type ontobim:IfcModelRepresentation;
                     ontobim:hasIfcId ?uid;
                     rdfs:label ?name.
  ?iri rdf:type ?class.
  FILTER (
    ?class NOT IN (
      ontobuildingstructure:BuildingStructureComponent,
      ontobuildingstructure:Assembly,
      ontobuildingstructure:Stair,
      ontobuildingstructure:StairFlight,
      ontobuildingstructure:Landing,
      ontobuildingstructure:Railing,
      ontobuildingstructure:StairStructuralComponent,
      ontobuildingstructure:Wall,
      ontobuildingstructure:Door,
      ontobuildingstructure:Window,
      ontobuildingstructure:Column,
      ontobuildingstructure:Ceiling,
      ontobuildingstructure:Floor,
      ontobuildingstructure:Roof,
      ontobuildingstructure:InclinedRoof,
      ontobuildingstructure:FlatRoof,
      bot:Zone,
      bot:Site,
      bot:Building,
      bot:Storey,
      ontobim:Room
    ) 
  )
}
"""


def classify_filename(names: pd.Series):
    """Classifies if assets should be an asset, furniture, or other categories (solar panel, sewage network).

    Args:
        names: A Pandas series of asset labels.

    Returns:
        A Pandas series of asset classes.
    """
    # Initialise a list containing partial names of individual assets to be split
    asset_list = ["Sensor", "Weather Station", "Meter",
                 "Fume Hood", "Explosive Precursor", "Chemistry Robot",
                 "Fridge"]
    
    counter = count(start=1, step=1)

    def _classify(name: str):
        if find_word(asset_list, name):
            return "asset" + str(next(counter))
        if find_word(["Solar Panel"], name):
            return "solarpanel"
        if find_word(["Manhole", "Sewage"], name):
            return "sewagenetwork"
        return "furniture"
    
    return names.apply(_classify)


def retrieve_metadata(query_endpoint: str, update_endpoint: str):
    """Retrieves the asset metadata from the specified endpoint.

    Args:
        query_endpoint: SPARQL Query endpoint.
        update_endpoint: SPARQL Update endpoint.

    Returns:
        A dataframe with headers 'iri', 'uid', 'name', 'file'.
    """
    logger.debug("Initialising KG Client...")
    client = KGClient(query_endpoint, update_endpoint)

    logger.debug("Creating query for execution...")
    query = create_metadata_query()

    logger.debug("Executing query...")
    results = client.execute_query(query)

    logger.debug("Storing results into dataframe...")
    metadata = pd.DataFrame(results, columns=["iri", "uid", "name"])

    logger.debug("Classifying results into their files...")
    metadata["file"] = classify_filename(metadata["name"])
    
    return metadata


def get_building_iri_name(query_endpoint: str, update_endpoint: str) -> str:
    """Retrieves the building IRI and name from the specified endpoint for BIM models.

    Args:
        query_endpoint: SPARQL Query endpoint.
        update_endpoint: SPARQL Update endpoint.

    Returns:
        A tuple (building_iri, building_name), where building_iri is the building IRI 
        and building_name is building name.
    """
    logger.debug("Initialising KG Client...")
    client = KGClient(query_endpoint, update_endpoint)

    query = QueryBuilder() \
        .add_prefix("http://www.w3.org/1999/02/22-rdf-syntax-ns#", RDF_PREFIX) \
        .add_prefix("http://www.w3.org/2000/01/rdf-schema#", RDFS_PREFIX) \
        .add_prefix("https://w3id.org/bot#", BOT_PREFIX) \
        .add_prefix("https://www.theworldavatar.com/kg/ontobim/", BIM_PREFIX) \
        .add_select_var("iri") \
        .add_select_var("name") \
        .add_where_triple("iri", RDF_PREFIX + ":type", BOT_PREFIX + ":Building", 1) \
        .add_where_triple("iri", BIM_PREFIX + ":hasIfcRepresentation", "buildingrep", 5) \
        .add_optional_triple("buildingrep", RDFS_PREFIX + ":label", "name", 5) \
        .build()

    logger.debug("Executing query...")
    results = client.execute_query(query)
     # When there is no results, return empty tuple
    if len(results) == 0:
        return "",""
    else:
        # If there is no name result or name key is an empty string, log a warning 
        if "name" not in results[0] or not results[0]["name"]:
            logger.warning("Detected building instance but name is empty! Please consult the Ifc2OntoBIM Agent's README on how to add the building name...")
            return results[0]["iri"], ""
        # assume that there exists only one building in the KG subgraph for BIM models
        return results[0]["iri"], results[0]["name"]
