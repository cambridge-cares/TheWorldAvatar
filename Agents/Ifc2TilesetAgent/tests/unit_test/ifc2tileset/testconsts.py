"""
# Author: picas9dan #

This file contains test resources for ifc2tileset unit tests.
"""

# Third-party imports
import trimesh
import numpy as np

# Self imports
from tests.unit_test.ifc2tileset.testutils import z_up_to_y_up

expected_content_metadata_schema = {"classes":
                                    {"ContentMetaData":
                                     {"description": "A metadata class for all content including building and individual assets",
                                      "name": "Content metadata",
                                      "properties": {
                                          "name": {
                                              "description": "Name of the asset/building",
                                              "type": "STRING"
                                          },
                                          "iri": {
                                              "description": "Data IRI of the asset/building",
                                              "type": "STRING"
                                          }
                                      }}
                                     }
                                    }

sample_solar_iri = "http://www.example.org/Test/Panel_123"
sample_solar_name = "Solar panels"
sample_sewage_iri = "http://www.example.org/Test/Sewage_123"
sample_sewage_name = "Sewage city network"

def sample_box_gen():
    z_up_coordinates = -4, -6, -10, 6, 4, 10
    y_up_coordinates = z_up_to_y_up(*z_up_coordinates)
    return trimesh.creation.box(bounds=[y_up_coordinates[:3], y_up_coordinates[3:]])


y_up_to_z_up_transform = np.array([
    [1, 0, 0, 0],
    [0, 0, 1, 0],
    [0, -1, 0, 0],
    [0, 0, 0, 1]
])


def sample_cone_gen():
    return trimesh.creation.cone(radius=5, height=20, transform=y_up_to_z_up_transform)


sample_box_bbox = [
    1, -1, 0,
    5, 0, 0,
    0, 5, 0,
    0, 0, 10
]

sample_cone_bbox = [
    0, 0, 10,
    5, 0, 0,
    0, 5, 0,
    0, 0, 10
]

combined_bbox = [
    0.5, -0.5, 5,
    5.5, 0, 0,
    0, 5.5, 0,
    0, 0, 15
]
