################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 26 Sep 2022                            #
################################################

# The purpose of this module is to provide functionality to 
# interact with the StacClients

import glob
import jaydebeapi
import json

from py4jps import agentlogging
from agent.errorhandling.exceptions import StackException
from agent.kgutils.javagateway import stackClientsGw, jpsBaseLibGW
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, ONTOP_URL
from agent.utils.env_configs import DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, ONTOP_FILE


# Initialise logger
logger = agentlogging.get_logger("prod")

# create a JVM module view and use it to import the required java classes
jpsBaseLibView = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")

stackClientsView = stackClientsGw.createModuleView()
stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.*")


class OntopClient:
    
    def __init__(self, query_endpoint=ONTOP_URL):

        try:
            # Initialise OntopClient as RemoteStoreClient
            self.ontop_client = jpsBaseLibView.RemoteStoreClient(query_endpoint)
        except Exception as ex:
            logger.error("Unable to initialise OntopClient.")
            raise StackException("Unable to initialise OntopClient.") from ex
    
    def performQuery(self, query):
        """
            This function performs query to Ontop endpoint.
            Arguments:
                query - SPARQL Query string
        """
        try:
            response = self.ontop_client.execute(query)
        except Exception as ex:
            logger.error("SPARQL query not successful.")
            raise StackException("SPARQL query not successful.") from ex
        return json.loads(response)

    
    @staticmethod
    def upload_ontop_mapping():
        # Initialise ONTOP client and upload mapping file using default properties
        # from environment variables

        try:
            # Create JAVA path object to mapping file
            f = stackClientsView.java.io.File(ONTOP_FILE)
            fp = f.toPath()
            # Update ONTOP mapping (requires JAVA path object)
            stackClientsView.OntopClient().updateOBDA(fp)
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


    def check_polygon_feature_exists(self, geojson_geometry, feature_type, table=LAYERNAME):
        """
            This function checks whether an identical geo-feature already exists in PostGIS table
            
            Arguments:
                geojson_geometry - GeoJSON Geometry fragments, i.e. only content of
                                   'geometry' field of entire GeoJSON, e.g.
                                   '{"type": "Polygon", 
                                             "coordinates": [[[0.4391965267561903, 52.7499954541537], 
                                                              [0.43934282344250064, 52.75000143684585], 
                                                              ... ]]
                                    }'

            Returns True if point already exists, False otherwise
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    curs.execute(f'SELECT type=\'{feature_type}\' AND \
                                   ST_Equals(wkb_geometry, ST_SetSRID(ST_GeomFromGeoJSON(\'{geojson_geometry}\'), 4326)) from {table}')
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


class GdalClient:
    
    def __init__(self):

        try:
            self.client = stackClientsView.GDALClient()
            self.orgoptions = stackClientsView.Ogr2OgrOptions()
        except Exception as ex:
            logger.error("Unable to initialise GdalClient.")
            raise StackException("Unable to initialise GdalClient.") from ex


    def uploadGeoJSON(self, geojson_string, database=DATABASE, table=LAYERNAME):
        """
            Calls StackClient function with default upload settings
        """
        self.client.uploadVectorStringToPostGIS(database, table, geojson_string,
                                                self.orgoptions, True)


class GeoserverClient:

    def __init__(self):

        try:
            self.client = stackClientsView.GeoServerClient()
            self.vectorsettings = stackClientsView.GeoServerVectorSettings()
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
