"""
# Author: qhouyee #

This module retrieves metadata from the knowledge graph.
"""

# Third party imports
import pandas as pd
import numpy as np
from py4jps import agentlogging

# Self imports
from agent.utils import find_word
from agent.kgutils import *

# Retrieve logger
logger = agentlogging.get_logger("dev")


def create_metadata_query():
    """
    Creates the SPARQL query for retrieving required meta data

    Returns:
        The required sparql query
    """
    return """\
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX bot:<https://w3id.org/bot#>

PREFIX ontobim:<http://www.theworldavatar.com/kg/ontobim/>
PREFIX ontobuildingstructure:<http://www.theworldavatar.com/kg/ontobuildingstructure/>

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
      ontobuildingstructure:FlatRoof
    ) &&
    ?class NOT IN (
      bot:Zone,
      bot:Site,
      bot:Building,
      bot:Storey
    ) &&
    ?class != ontobim:Room
  )
}
"""


def classify_file_name(dataframe: pd.DataFrame):
    """
    Classifies if the asset should be an asset, furniture, or other categories (solar panel).
    File name is appended to the dataframe

    Argument:
        dataframe - dataframe containing the query results
    Returns:
        Dataframe after classification
    """
    # Initialise a new empty column to contain file name
    dataframe["file"] = np.nan
    # Initialise a list containing partial names of individual assets to be split
    assetlist = ["Sensor", "Weather Station", "Meter",
                 "Fume Hood", "Explosive Precursor", "Chemistry Robot",
                 "Fridge"]
    counter = 1  # counter for appending asset name

    for row in range(len(dataframe.index)):
        # If asset is to be split, generate it as an asset file
        if find_word(assetlist, dataframe[NAME_VAR].iloc[row]):
            dataframe.at[row, "file"] = "asset" + str(counter)
            counter = counter + 1
        # If asset is a solar panel, generate it as a solarpanel file
        elif find_word(["Solar Panel"], dataframe[NAME_VAR].iloc[row]):
            dataframe.at[row, "file"] = "solarpanel"
        # If asset is a manhole, generate it as a sewage file
        elif find_word(["Manhole"], dataframe[NAME_VAR].iloc[row]):
            dataframe.at[row, "file"] = "sewagenetwork"
        # Else, all other asset types are generated as a furniture file
        else:
            dataframe.at[row, "file"] = "furniture"
    return dataframe


def retrieve_metadata(query_endpoint: str, update_endpoint: str):
    """
    Retrieves the necessary metadata from the specified endpoint

    Arguments:
        query_endpoint - SPARQL Query endpoint
        update_endpoint - SPARQL Update endpoint
    Returns:
        Processed query results as a dataframe with headers ('iri', 'uid', 'name', 'file')
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
    return classify_file_name(metadata)


def get_building_iri(query_endpoint: str, update_endpoint: str) -> str:
    """
    Retrieves the building IRI from the specified endpoint

    Arguments:
        query_endpoint - SPARQL Query endpoint
        update_endpoint - SPARQL Update endpoint
    Returns:
        Building IRI string
    """
    logger.debug("Initialising KG Client...")
    client = KGClient(query_endpoint, update_endpoint)

    query = QueryBuilder() \
        .add_prefix("http://www.w3.org/1999/02/22-rdf-syntax-ns#", RDF_PREFIX) \
        .add_prefix("https://w3id.org/bot#", BOT_PREFIX) \
        .add_select_var("iri") \
        .add_where_triple("iri", RDF_PREFIX + ":type", BOT_PREFIX + ":Building", 1) \
        .build()

    logger.debug("Executing query...")
    # assume that there exists only one building in the KG subgraph
    return client.execute_query(query)[0]["iri"]
