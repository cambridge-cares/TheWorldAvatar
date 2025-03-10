###########################################
# Authors: Jiying Chen (jc2341@cam.ac.uk) #
# May, 2024                               #
###########################################
import re
from pydantic import BaseModel
from pydantic.dataclasses import dataclass
from twa import agentlogging
from agent.twa.jps_singletons import jps_gateway

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')

@dataclass
class DBConfig(BaseModel):
    user: str
    password: str
    host: str
    port: str
    dbname: str = 'postgres'
    url: str
    
    def __init__(self, pg_conf):
        self.pg_conf = pg_conf
        
        self.user = pg_conf.getUsername()
        self.url = pg_conf.getJdbcURL(self.dbname)
        self.port = pg_conf.getPort()
        self.password = pg_conf.getPassword()
        
        pattern = r"//([^:/]+)"
        match = re.search(pattern, self.url)
        self.host = match.group(1)
        
    def set_db_name(self, db_name):
        self.dbname = db_name
        self.url = self.pg_conf.getJdbcURL(self.dbname)
        
    def model_dump(self, *, mode = 'python', include = None, exclude = None, context = None, by_alias = False, exclude_unset = False, exclude_defaults = False, exclude_none = False, round_trip = False, warnings = True, serialize_as_any = False):
        return super().model_dump(mode=mode, include=include, exclude=exclude, context=context, by_alias=by_alias, exclude_unset=exclude_unset, exclude_defaults=exclude_defaults, exclude_none=exclude_none, round_trip=round_trip, warnings=warnings, serialize_as_any=serialize_as_any)
        

def retrieve_stack_settings():
    """
    Reads settings from Stack clients with detailed error reporting
    """
     # Define global scope for global variables
    global DB_URL, DB_USER, DB_PASSWORD, \
           SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT, ONTOP_URL
    try:
        stack_clients_view = jps_gateway.createModuleView()
        jps_gateway.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.docker.ContainerClient")
        jps_gateway.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
        jps_gateway.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")
        jps_gateway.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig")
        
        # Retrieve endpoint configurations from Stack clients
        containerClient = stack_clients_view.ContainerClient()

        # PostgreSQL/PostGIS container
        pg = stack_clients_view.PostGISEndpointConfig("", "", "", "", "")
        pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())

        # Ontop container (not necessary in this agent)
        ont = stack_clients_view.OntopEndpointConfig("","","","","")
        ont_conf = containerClient.readEndpointConfig("ontop", ont.getClass())

        DB_CONF = DBConfig(pg_conf)
        ONTOP_URL = ont_conf.getUrl()

        return DB_CONF, ONTOP_URL
    except Exception as e:
        logger.error("General Stack client parameter extraction error: {}".format(str(e)))
        raise Exception("General Stack client parameter extraction error: {}".format(str(e)))
    
# Run when module is imported
retrieve_stack_settings()