"""
# Author: qhouyee #

This module gives all modules access to the properties.yml.
"""

# Third party imports
import yaml


def set_properties(path):
    """
    Retrieves the properties stored in the YAML file
    and set them accordingly to requirements

    Argument:
    path - file path to the YAML document
    """
    with open(path, 'r') as ymlfile:
        properties = yaml.safe_load(ymlfile)

    query_endpoint = properties['query_endpoint']
    update_endpoint = properties['update_endpoint']
    return query_endpoint, update_endpoint
