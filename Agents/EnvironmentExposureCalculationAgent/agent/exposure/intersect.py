import os
from typing import cast
from flask import current_app
from agent.exposure.env_feature_retrieval import fetch_env_features
from agent.exposure.constants import FeatureTableName, KGScript
from agent.stack.ontop_client import OntopClient
from agent.stack.postgis_client import PostGISClient

class Intersect:

    def __init__(self, query_id:str):
        self.query_id = query_id
        
        self.postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
        self.ontop_client = cast(OntopClient, current_app.extensions['ontop_client'])
        
        self.script_dir = os.path.dirname(os.path.abspath(__file__))
        
        
    def intersect(self,):
        """
        Fetch polygon and point type environment features and perform intersection accordingly.
        
        Notes
        -----
        - Results will be stored in both intersect functions.
        """
        
        fetch_env_features([KGScript.Greenspace], save_feature_table=True, feature_table=FeatureTableName.Polygons)
        self.area_intersect()
        
        fetch_env_features([KGScript.Culture], save_feature_table=True, feature_table=FeatureTableName.Points)
        self.point_intersect()
        
        return 0
        
    def area_intersect(self):
        """
        Run point intersection of polygon type env_features and input polygons.
        Results are stored in "env_exposure"."results".
        """
        
        path = os.path.join(self.script_dir, "script_sql", "polygon_intersect.sql")
        with open(path) as file:
            self.postgis_client.execute_update(file.read(), 
                                               {"feature_table_name": FeatureTableName.Polygons.value['feature_table_name']},
                                               {'query_id': self.query_id})
    
    def point_intersect(self):
        """
        Run point intersection of point type env_features and input polygons.
        Results are stored in "env_exposure"."results".
        """
        
        path = os.path.join(self.script_dir, "script_sql", "point_intersect.sql")
        with open(path) as file:
            self.postgis_client.execute_update(file.read(), 
                                               {"feature_table_name": FeatureTableName.Points.value['feature_table_name']},
                                               {'query_id': self.query_id})