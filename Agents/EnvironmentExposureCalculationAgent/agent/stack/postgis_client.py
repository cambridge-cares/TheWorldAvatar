import re
from typing import Any
import psycopg2
from pydantic import BaseModel, model_validator
from agent.exceptions import StackException
from twa import agentlogging


class DBConfig(BaseModel):
    user: str
    password: str
    host: str
    port: str
    dbname: str
    url: str
    pg_conf: Any

    @model_validator(mode="before")
    @classmethod
    def set_pg_conf_fields(cls, values):
        """Pre-process pg_conf values before the model is initialized."""
        pg_conf = values.get("pg_conf")
        db_name = values.get("dbname", "postgres")

        # Assign values from pg_conf
        values["user"] = pg_conf.getUsername()
        values["url"] = pg_conf.getJdbcURL(db_name)
        values["port"] = pg_conf.getPort()
        values["password"] = pg_conf.getPassword()

        # Extract host from URL using regex
        pattern = r"//([^:/]+)"
        match = re.search(pattern, values["url"])
        if match:
            values["host"] = match.group(1)
        else:
            raise ValueError("Invalid URL format: cannot extract host")

        values["dbname"] = db_name  # Set dbname explicitly if provided
        return values

class PostGISClient:
    
    logger = agentlogging.get_logger("dev")
    db_config: DBConfig

    def __init__(self, db_config: DBConfig):        
        self.db_config = db_config

    def execute_query(self, query: str, table_mappings: dict, val_params:dict) -> psycopg2.extensions.connection:
        if table_mappings:
            for key, value in table_mappings.items():
                placeholder = f"%({key})s"
                query = query.replace(placeholder, value)
            
        with psycopg2.connect(**self.db_config.model_dump(exclude={'pg_conf', 'url'})) as conn:
            with conn.cursor() as cur:
                cur.execute(query, val_params)
                # results = cur.fetchall()
                # print(results)

    def load_template(self, filepath: str) -> str:
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()
        return content
