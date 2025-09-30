from enum import Enum
import os

kg_script_dir = os.path.join(os.path.dirname(
    os.path.abspath(__file__)), 'script_kg')
class KGScript(Enum):
    Culture = os.path.join(kg_script_dir, 'culture.sparql')
    Facility = os.path.join(kg_script_dir, 'facility.sparql')
    Greenspace = os.path.join(kg_script_dir, 'greenspace.sparql')


class FeatureTableName(Enum):
    Points = {"feature_table_name": "temp_points", "geom_type": "PointZ"}
    Polygons = {"feature_table_name": "temp_polygons", "geom_type": "Geometry"}
