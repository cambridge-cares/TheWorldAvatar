import psycopg2
from agent.config.stack_configs import DBConfig
from agent.exceptions import StackException
from twa.jps_singletons import jps_gateway
from twa import agentlogging

class PostGISClient:
    stackClients_view = jps_gateway.createModuleView()
    logger = agentlogging.get_logger("PostGISClient")
    db_config: DBConfig
    
    def __init__(self, db_config: DBConfig):
        self.db_config = db_config
        try:
            # Retrieve JDBC connection properties
            self.conn_props = self.connection_properties()
        except Exception as ex:
            self.logger.error("Unable to retrieve JDBC connection properties.")
            raise StackException("Unable to retrieve JDBC connection properties.") from ex

    
    def execute_query(self, query: str) -> psycopg2.extensions.connection:
        with psycopg2.connect(self.db_config.model_dump()) as conn:
            with conn.cursor() as cur:
                cur.execute(query, (20,))
                results = cur.fetchall()
                print(results)
    
    def load_template(self, filepath: str) -> str:
        with open(filepath, "r", encoding="utf-8") as f:
            content = f.read()
        return content