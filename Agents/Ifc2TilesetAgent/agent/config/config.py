"""
# Author: qhouyee #

This module gives all modules access to the properties.yml.
"""

# Third party imports
import yaml

bbox_root = []
bbox_child = []


def set_properties(path):
    """
    Retrieves the properties stored in the YAML file
    and set them accordingly to requirements

    Argument:
    path - file path to the YAML document
    """
    with open(path, 'r') as ymlfile:
        properties = yaml.safe_load(ymlfile)

    global bbox_root
    bbox_root = set_bbox(
        properties['root_tile']['x_center'], properties['root_tile']['y_center'],
        properties['root_tile']['z_center'], properties['root_tile']['length'],
        properties['root_tile']['width'], properties['root_tile']['height'])

    global bbox_child
    bbox_child = set_bbox(
        properties['child_tile']['x_center'], properties['child_tile']['y_center'],
        properties['child_tile']['z_center'], properties['child_tile']['length'],
        properties['child_tile']['width'], properties['child_tile']['height'])

    query_endpoint = properties['query_endpoint']
    update_endpoint = properties['update_endpoint']
    return query_endpoint, update_endpoint


def set_bbox(x_center, y_center, z_center, length, width, height):
    """
    Defines and set the bounding boxes pf tiles required in generating the tilesets

    Arguments:
        x_center - x-coordinate of the model's center position
        y_center - y-coordinate of the model's center position
        z_center - z-coordinate of the model's center position
        length - x-axis length for the bounding box
        width - y-axis length for the bounding box
        height - z-axis length for the bounding box
    Returns:
    The bounding box coordinates for the tile as a list
    """
    # WIP: Difficulty in creating suitable bounding boxes from model automatically
    return [
        x_center, y_center, z_center,
        length / 2, 0, 0,  # half-length for x
        0, width / 2, 0,   # half-length for y
        0, 0, height / 2   # half-length for z
    ]