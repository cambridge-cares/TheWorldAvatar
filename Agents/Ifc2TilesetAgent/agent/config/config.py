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
        A tuple of SPARQL query and update endpoints, solar panel and sewage tileset metadata.
    """
    with open(path, 'r') as ymlfile:
        properties = yaml.safe_load(ymlfile)

    query_endpoint = properties['query_endpoint']
    update_endpoint = properties['update_endpoint']
    # Retrieve solar panel tileset's default IRI and name if available
    solar_panel_tileset_iri = properties['solar_panel_tileset_iri']
    solar_panel_tileset_name = properties['solar_panel_tileset_name']
    # Verify if there are any IRI and name set
    if solar_panel_tileset_iri != "" and solar_panel_tileset_name != "":
        logger.debug("Detected both solar panel IRI and name parameters...")
        solar_panel_tileset = [
            solar_panel_tileset_iri, solar_panel_tileset_name]
    elif solar_panel_tileset_iri != "":
        logger.debug("Detected only solar panel IRI! Missing name parameter...")
    elif solar_panel_tileset_name != "":
        logger.debug("Detected only solar panel name! Missing IRI parameter...")
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
    elif sewage_tileset_name != "":
        logger.debug("Detected only sewage name! Missing IRI parameter...")
    else:  # Return an empty list if unavailable
        logger.debug("No sewage IRI or name parameter detected...")
        sewage_tileset = []
    return query_endpoint, update_endpoint, solar_panel_tileset, sewage_tileset
