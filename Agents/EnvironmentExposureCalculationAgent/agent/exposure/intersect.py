import os
from typing import cast
from flask import current_app
import pandas as pd
from agent.stack.ontop_client import OntopClient
from agent.stack.postgis_client import PostGISClient
from agent.utils.table_name_helper import TableNameHelper

class Intersect:
    def __init__(self, table_name_helper:TableNameHelper):
        self.points_table_name = table_name_helper.get_points_table_name()
        self.table_name_helper = table_name_helper
        
        self.postgis_client = cast(PostGISClient, current_app.extensions['postgis_client'])
        self.ontop_client = cast(OntopClient, current_app.extensions['ontop_client'])
        
        self.script_dir = os.path.dirname(os.path.abspath(__file__))
        
        self.res = pd.DataFrame()
    
    def intersect(self):
        city_building_intersect_sql_path = os.path.join(self.script_dir, "script_sql", "city_building_intersect.sql")
        self.get_result_from_dataset("city_building", city_building_intersect_sql_path)
            
        culture_intersect_sql_path = os.path.join(self.script_dir, "script_sql", "culture_intersect.sql")
        self.get_result_from_dataset("culture", culture_intersect_sql_path)
        
        osm_intersect_sql_path = os.path.join(self.script_dir, "script_sql", "osm_intersect.sql")
        self.get_result_from_dataset("osm", osm_intersect_sql_path)

    def get_result_from_dataset(self, dataset_name:str, sql_path:str):
        with open(sql_path, "r") as file:
            result_table_name = self.table_name_helper.get_table_name(dataset_name)
            table_names = {"point_table_name": self.points_table_name, "result_table_name": result_table_name}
            self.postgis_client.execute_update(file.read(), table_names)
            # self.res = pd.concat([self.res, self.postgis_client.execute_update(file.read(), table_names)], axis=0, ignore_index=True)
            
    def get_result_from_city_building(self,):
        sql_path = os.path.join(self.script_dir, "script_sql", "city_building_intersect.sql")
        result_table_name = self.table_name_helper.get_table_name('city_building')
        with open(sql_path, "r") as file:
            table_names = {"point_table_name": self.points_table_name, "result_table_name": result_table_name}
            self.postgis_client.execute_update(file.read(), table_names)
        
        res = self.postgis_client.execute_query("SELECT * FROM %(result_table_name)s", {"result_table_name": result_table_name})
        
        sparql_path = os.path.join(self.script_dir, "script_kg", "city_building_intersect.sparql")
        # self.ontop_client.performQuery()