"""
# Author: qhouyee #

This module gives all modules access to the properties.yml.
"""

# Third party imports
import yaml


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
        solar_panel_tileset = [
            solar_panel_tileset_iri, solar_panel_tileset_name]
    else:  # Return an empty list if unavailable
        solar_panel_tileset = []
    # Retrieve sewage tileset's default IRI and name if available
    sewage_tileset_iri = properties['sewage_tileset_iri']
    sewage_tileset_name = properties['sewage_tileset_name']
    # Verify if there are any IRI and name set
    if sewage_tileset_iri != "" and sewage_tileset_name != "":
        sewage_tileset = [sewage_tileset_iri, sewage_tileset_name]
    else:  # Return an empty list if unavailable
        sewage_tileset = []
    return query_endpoint, update_endpoint, solar_panel_tileset, sewage_tileset
