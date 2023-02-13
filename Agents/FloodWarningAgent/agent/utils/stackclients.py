################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 09 Sep 2022                            #
################################################

# The purpose of this module is to provide functionality to interact with the
# overarching Docker Stack using the Stack Clients wrapped by py4jps

import glob
import jaydebeapi
import json

from agent.errorhandling.exceptions import StackException
from agent.utils.javagateway import stackClientsGw, jpsBaseLibGW
from agent.utils.env_configs import DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, ONTOP_FILE
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, ONTOP_URL

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


class StackClient:
    # Define parent class for all Stack Clients to minimise number of required
    # JAVA resource views and import the required java classes

    # Create ONE JPS_BASE_LIB view
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

    # Create ONE Stack Clients view
    stackClients_view = stackClientsGw.createModuleView()
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.gdal.GDALClient")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.GeoServerClient")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.ontop.OntopClient")


class OntopClient(StackClient):

    def __init__(self, query_endpoint=ONTOP_URL):
        # Initialise OntopClient as RemoteStoreClient
        try:
            self.ontop_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint)
        except Exception as ex:
            logger.error("Unable to initialise OntopClient.")
            raise StackException("Unable to initialise OntopClient.") from ex
    

    def performQuery(self, query):
        """
            This function performs query to Ontop endpoint.
            NOTE: Using Blazegraph with SERVICE keyword and ONTOP_URL seems to resolve
                  several connection issues observed when using Ontop client directly
            Arguments:
                query - SPARQL Query string
        """
        try:
            response = self.ontop_client.execute(query)
        except Exception as ex:
            logger.error("SPARQL query not successful")
            raise StackException("SPARQL query not successful.") from ex
        return json.loads(response)

    
    @staticmethod
    def upload_ontop_mapping():
        # Upload mapping file using default properties from environment variables
        try:
            # Create JAVA path object to mapping file            
            f = OntopClient.stackClients_view.java.io.File(ONTOP_FILE)
            fp = f.toPath()
            # Update ONTOP mapping (requires JAVA path object)
            OntopClient.stackClients_view.OntopClient().updateOBDA(fp)
        except Exception as ex:
            logger.error("Unable to update OBDA mapping.")
            raise StackException("Unable to update OBDA mapping.") from ex


class PostGISClient:
    
    def __init__(self, dburl=DB_URL, dbuser=DB_USER, dbpassword=DB_PASSWORD,
                 database=DATABASE):
        self.dburl = dburl
        self.dbuser = dbuser
        self.dbpassword = dbpassword
        self.database = database
        try:
            # Retrieve JDBC connection properties
            self.conn_props = self.connection_properties()
        except Exception as ex:
            logger.error("Unable to retrieve JDBC connection properties.")
            raise StackException("Unable to retrieve JDBC connection properties.") from ex

    
    def connection_properties(self):
        """
            This function returns dictionary of JDBC connection properties 
            to PostgreSQL database
        """
        conn = []
        conn.append('org.postgresql.Driver')
        conn.append(self.dburl)
        driver_arg ={}
        driver_arg['user'] = self.dbuser
        driver_arg['password'] = self.dbpassword
        conn.append(driver_arg)
        # Get (version independent) path to JDBC driver .jar
        fp = glob.glob('/app/resources/postgresql-*.jar')[0]
        conn.append(fp)
        
        return conn


    def check_table_exists(self, table=LAYERNAME):
        """
            This function checks whether a given table exists in the database
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    curs.execute(f'SELECT COUNT(*) FROM information_schema.tables WHERE table_name = \'{table}\'')
                    res = curs.fetchone()
                    if res[0] == 1:
                        return True
                    else:
                        return False
        except Exception as ex:
            logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException('Unsuccessful JDBC interaction.') from ex


    def check_point_feature_exists(self, lat, lon, feature_type, table=LAYERNAME):
        """
            This function checks whether an identical geo-feature already exists
            in PostGIS table
            Returns True if point already exists, False otherwise
        """
        try:
            #TODO: To be reworked for polygons
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    curs.execute(f'SELECT type=\'{feature_type}\' AND \
                                   ST_Equals(wkb_geometry, ST_SetSRID(ST_POINT({lon},{lat}), 4326)) from {table}')
                    # Fetching the SQL results from the cursor only works on first call
                    # Recurring calls return empty list and curs.execute needs to be run again
                    res = curs.fetchall()
                    # Extract Boolean results from tuples and check for any Trues
                    res = [r[0] for r in res]
                    res = any(res)
                    return res
        except Exception as ex:
            logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException('Unsuccessful JDBC interaction.') from ex


class GdalClient(StackClient):
    
    def __init__(self):
        # Initialise GdalClient with default upload/conversion settings
        try:
            self.client = self.stackClients_view.GDALClient()
            self.orgoptions = self.stackClients_view.Ogr2OgrOptions()
        except Exception as ex:
            logger.error("Unable to initialise GdalClient.")
            raise StackException("Unable to initialise GdalClient.") from ex


    def uploadGeoJSON(self, geojson_string, database=DATABASE, table=LAYERNAME):
        """
            Calls StackClient function with default upload settings
        """
        self.client.uploadVectorStringToPostGIS(database, table, geojson_string,
                                                self.orgoptions, True)


class GeoserverClient(StackClient):

    def __init__(self):

        # Initialise Geoserver with default settings
        try:
            self.client = self.stackClients_view.GeoServerClient()
            self.vectorsettings = self.stackClients_view.GeoServerVectorSettings()
        except Exception as ex:
            logger.error("Unable to initialise GeoServerClient.")
            raise StackException("Unable to initialise GeoServerClient.") from ex   


    def create_workspace(self, workspace=GEOSERVER_WORKSPACE):
        self.client.createWorkspace(workspace)

    
    def create_postgis_layer(self, geoserver_workspace=GEOSERVER_WORKSPACE,
                                   geoserver_layer=LAYERNAME,
                                   postgis_database=DATABASE):
        """
            Calls StackClient function with default settings
            Please note: Postgis database table assumed to have same name as
                         Geoserver layer
        """
        self.client.createPostGISLayer(None, geoserver_workspace, postgis_database,
                                       geoserver_layer, self.vectorsettings)


def create_geojson_for_postgis(station_iri: str, station_name: str, station_type: str,
                               station_subtype: str, lat: float, long: float, kg_endpoint: str):
    """
    Create GeoJSON object for upload to PostGIS database
    Needs to contain at least the following properties for FeatureInfoAgent to work:
        "name" - human readable name of the feature 
        "iri" - full IRI of the feature as represented in the knowledge graph
        "endpoint" - URL of the Blazegraph namespace containing data on the feature,
                     i.e. from where shall FeatureInfoAgent retrieve data about "iri"
    Further properties can be added as needed, i.e. for styling purposes
    """
    #TODO: To be reworked for flood polygons
    # Initialise GeoJSON
    geojson = {'type': 'Feature'}

    # Define properties
    props = {
        'iri': station_iri,
        'name': station_name,
        'endpoint': kg_endpoint,
        'geom_iri': station_iri + '/geometry',
        'type': station_type,
        'subtype': station_subtype,
    }

    # Define geometry
    geom = {
        'type': 'Point',
        'coordinates': [float(long), float(lat)]
    }

    # Construct GeoJSON
    geojson['properties'] = props
    geojson['geometry'] = geom
    
    return json.dumps(geojson)
