from enum import Enum


class ScriptName(Enum):
    Culture = "culture"
    Facility = "facility"
    Greenspace = "greenspace"


class FeatureTableName(Enum):
    Points = {"feature_table_name": "temp_points", "geom_type": "PointZ"}
    Polygons = {"feature_table_name": "temp_polygons", "geom_type": "Geometry"}
