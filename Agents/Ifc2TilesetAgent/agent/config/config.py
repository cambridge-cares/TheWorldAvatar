"""
# Author: qhouyee #

This module gives all modules access to the properties.yml.
"""

# Third party imports
import yaml
from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("dev")


def load_properties(path: str):
    """Retrieves properties stored in the YAML filepath.

    Args:
        path: Path to the YAML document.

    Returns:
        A tuple of SPARQL query and update endpoints, solar panel, sewage, and bim tileset metadata.
    """
    with open(path, 'r') as ymlfile:
        properties = yaml.safe_load(ymlfile)

    query_endpoint = properties['query_endpoint']
    update_endpoint = properties['update_endpoint']

    # Retrieve BIM root tileset's default IRI and name if available
    bim_tileset_iri = properties['bim_tileset_iri']
    bim_tileset_name = properties['bim_tileset_name']
    # Verify if there are any IRI and name set
    if bim_tileset_iri != "" and bim_tileset_name != "":
        logger.debug("Detected both sewage IRI and name parameters...")
        bim_tileset = [bim_tileset_iri, bim_tileset_name]
    elif bim_tileset_iri != "":
        logger.debug("Detected only sewage IRI! Missing name parameter...")
        bim_tileset = []
    elif bim_tileset_name != "":
        logger.debug("Detected only sewage name! Missing IRI parameter...")
        bim_tileset = []
    else:  # Return an empty list if unavailable
        logger.debug("No sewage IRI or name parameter detected...")
        bim_tileset = []

    # Retrieve solar panel tileset's default IRI and name if available
    solar_panel_tileset_iri = properties['solar_panel_tileset_iri']
    solar_panel_tileset_name = properties['solar_panel_tileset_name']
    # Verify if there are any IRI and name set
    if solar_panel_tileset_iri != "" and solar_panel_tileset_name != "":
        logger.debug("Detected both solar panel IRI and name parameters...")
        solar_panel_tileset = [
            solar_panel_tileset_iri, solar_panel_tileset_name]
    elif solar_panel_tileset_iri != "":
        logger.debug(
            "Detected only solar panel IRI! Missing name parameter...")
        solar_panel_tileset = []
    elif solar_panel_tileset_name != "":
        logger.debug(
            "Detected only solar panel name! Missing IRI parameter...")
        solar_panel_tileset = []
    else:  # Return an empty list if unavailable
        logger.debug("No solar panel IRI or name parameter detected...")
        solar_panel_tileset = []

    # Retrieve sewage tileset's default IRI and name if available
    sewage_tileset_iri = properties['sewage_tileset_iri']
    sewage_tileset_name = properties['sewage_tileset_name']
    # Verify if there are any IRI and name set
    if sewage_tileset_iri != "" and sewage_tileset_name != "":
        logger.debug("Detected both sewage IRI and name parameters...")
        sewage_tileset = [sewage_tileset_iri, sewage_tileset_name]
    elif sewage_tileset_iri != "":
        logger.debug("Detected only sewage IRI! Missing name parameter...")
        sewage_tileset = []
    elif sewage_tileset_name != "":
        logger.debug("Detected only sewage name! Missing IRI parameter...")
        sewage_tileset = []
    else:  # Return an empty list if unavailable
        logger.debug("No sewage IRI or name parameter detected...")
        sewage_tileset = []
    return query_endpoint, update_endpoint, solar_panel_tileset, sewage_tileset, bim_tileset
