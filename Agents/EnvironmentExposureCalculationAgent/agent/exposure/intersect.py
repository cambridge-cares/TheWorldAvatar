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
    
    def intersect(self):
        res = pd.DataFrame()
        
        city_buildings_sql_path = os.path.join(self.script_dir, "script_sql", "city_building_intersect.sql")
        city_buildings_sparql_path = os.path.join(self.script_dir, "script_kg", "city_building.sparql")
        res = pd.concat([res, self.get_result_from_dataset("city_building", city_buildings_sql_path, city_buildings_sparql_path)], 
                        ignore_index=True)
        # self.get_result_from_city_building()
            
        culture_intersect_sql_path = os.path.join(self.script_dir, "script_sql", "culture_intersect.sql")
        culture_intersect_sparql_path = os.path.join(self.script_dir, "script_kg", "culture.sparql")
        res = pd.concat([res, self.get_result_from_dataset("culture", culture_intersect_sql_path, culture_intersect_sparql_path)],
                        ignore_index=True)
        
        # osm_intersect_sql_path = os.path.join(self.script_dir, "script_sql", "osm_intersect.sql")
        # self.get_result_from_dataset("osm", osm_intersect_sql_path)
        return res

    def get_result_from_dataset(self, dataset_name:str, sql_path:str, sparql_path:str):
        with open(sql_path, "r") as file:
            result_table_name = self.table_name_helper.get_table_name(dataset_name)
            table_names = {"point_table_name": self.points_table_name, "result_table_name": result_table_name}
            self.postgis_client.execute_update(file.read(), table_names)
            # self.res = pd.concat([self.res, self.postgis_client.execute_update(file.read(), table_names)], axis=0, ignore_index=True)
           
        res = self.postgis_client.execute_query("SELECT * FROM \"env_exposure\".\"%(result_table_name)s\"", {"result_table_name": result_table_name})
        values = " ".join([f'(<{row[0]}> "{row[1]}")' for _, row in res.iterrows()]) 
        
        with open(sparql_path, "r") as file:
            kg_res = pd.DataFrame(self.ontop_client.performQuery(file.read() % {"inputs": values}))

        return kg_res
            
    # def get_result_from_city_building(self,):
    #     sql_path = os.path.join(self.script_dir, "script_sql", "city_building_intersect.sql")
    #     result_table_name = self.table_name_helper.get_table_name('city_building')
    #     with open(sql_path, "r") as file:
    #         table_names = {"point_table_name": self.points_table_name, "result_table_name": result_table_name}
    #         self.postgis_client.execute_update(file.read(), table_names)
        
    #     res = self.postgis_client.execute_query("SELECT * FROM \"env_exposure\".\"%(result_table_name)s\"", {"result_table_name": result_table_name})
    #     values = " ".join([f'<{iri}>' for iri in res[0]])
        
    #     sparql_path = os.path.join(self.script_dir, "script_kg", "city_building.sparql")
    #     with open(sparql_path, "r") as file:
    #         kg_res = pd.DataFrame(self.ontop_client.performQuery(file.read() % {"inputs": values}))

    #     return kg_res