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
        A tuple of SPARQL query and update endpoints.
    """
    with open(path, 'r') as ymlfile:
        properties = yaml.safe_load(ymlfile)

    query_endpoint = properties['query_endpoint']
    update_endpoint = properties['update_endpoint']

    return query_endpoint, update_endpoint
