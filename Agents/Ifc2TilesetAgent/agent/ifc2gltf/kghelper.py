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
logger = agentlogging.get_logger('dev')

def create_query():
    """
    Creates the SPARQL query for retrieving required meta data

    Returns:
        The required sparql query
    """
    builder = QueryBuilder()
    builder.add_prefix("http://www.w3.org/1999/02/22-rdf-syntax-ns#",RDF_PREFIX)
    builder.add_prefix("http://www.w3.org/2000/01/rdf-schema#", RDFS_PREFIX)
    builder.add_prefix("https://w3id.org/bot#", BOT_PREFIX)
    builder.add_prefix("http://www.theworldavatar.com/ontology/ontobim/ontoBIM#", BIM_PREFIX)
    builder.add_prefix("http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#", IFC2_PREFIX)
    builder.add_prefix("https://standards.buildingsmart.org/IFC/DEV/IFC4/ADD2_TC1/OWL#", IFC4_PREFIX)
    builder.add_select_var(IRI_VAR, ID_VAR, NAME_VAR)
    builder.add_where_triple(IRI_VAR, RDF_PREFIX + ":type", BOT_PREFIX+":Element", 1)
    builder.add_where_triple(IRI_VAR, BIM_PREFIX + ":hasIfcId", ID_VAR, 5)
    builder.add_where_triple(IRI_VAR, RDFS_PREFIX + ":label", NAME_VAR, 5)
    # IFC class var is needed for filtering the relevant classes, and should not be selected
    buildingproxy = ":IfcBuildingElementProxy"
    furnishing = ":IfcFurnishingElement"
    flowterminal = ":IfcFlowTerminal"
    builder.add_where_triple(IRI_VAR, RDF_PREFIX + ":type", IFCCLASS_VAR, 5)
    builder.add_values_where(IFCCLASS_VAR, IFC2_PREFIX + buildingproxy,
        IFC2_PREFIX + furnishing, IFC2_PREFIX +flowterminal,IFC4_PREFIX + buildingproxy,
        IFC4_PREFIX + furnishing, IFC4_PREFIX +flowterminal,)
    return str(builder)


def classify_file_name(dataframe):
    """
    Classifies if the asset should be an asset, furniture, or other categories (solar panel).
    File name is appended to the dataframe

    Argument:
        dataframe - dataframe containing the query results
    Returns:
        Dataframe after classification
    """
    # Initialise a new empty column to contain file name
    dataframe['file'] = np.nan
    # Initialise a list containing partial names of individual assets to be split
    assetlist = ["Sensor", "Weather Station", "Meter",
                "Fume Hood", "Explosive Precursor", "Chemistry Robot",
                "Fridge"]
    counter = 1 # counter for appending asset name

    for row in range(len(dataframe.index)):
        # If asset is to be split, generate it as an asset file
        if find_word(assetlist, dataframe[NAME_VAR].iloc[row]):
            dataframe.at[row, 'file'] = "asset"+str(counter)
            counter = counter + 1
        # If asset is a solar panel, generate it as a solarpanel file
        elif find_word(["Solar Panel"], dataframe[NAME_VAR].iloc[row]):
            dataframe.at[row, 'file'] = "solarpanel"
        # If asset is a manhole, generate it as a sewage file
        elif find_word(["Manhole"],dataframe[NAME_VAR].iloc[row]):
            dataframe.at[row, 'file'] = "sewagenetwork"
        # Else, all other asset types are generated as a furniture file
        else:
            dataframe.at[row, 'file'] = "furniture"
    return dataframe


def retrieve_metadata(query_endpoint: str, update_endpoint: str):
    """
    Retrieves the necessary metadata from the specified endpoint

    Arguments:
        query_endpoint - SPARQL Query endpoint
        update_endpoint - SPARQL Update endpoint
    Returns:
        Processed query results as a dataframe
    """
    logger.debug("Initialising KG Client...")
    client = KGClient(query_endpoint, update_endpoint)

    logger.debug("Creating query for execution...")
    query = create_query()

    logger.debug("Executing query...")
    results = client.execute_query(query)

    logger.debug("Storing results into dataframe...")
    metadata = pd.DataFrame.from_dict(results)

    logger.debug("Classifying results into their files...")
    return classify_file_name(metadata)
