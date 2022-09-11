################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 09 Sep 2022                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

import glob
import jaydebeapi
import json


#import agentlogging
from metoffice.errorhandling.exceptions import StackException
from metoffice.kgutils.javagateway import stackClientsGw, jpsBaseLibGW
from metoffice.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, ONTOP_URL
from metoffice.utils.env_configs import DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, ONTOP_FILE

# Initialise logger
#logger = agentlogging.get_logger("prod")


class OntopClient:

    def __init__(self, query_endpoint=ONTOP_URL):

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

        try:
            # Initialise OntopClient as RemoteStoreClient
            self.ontop_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint)
        except:
            #logger.error("Unable to initialise OntopClient.")
            raise StackException("Unable to initialise OntopClient.")
    
    def performQuery(self, query):
        """
            This function performs query to Ontop endpoint.
            Arguments:
                query - SPARQL Query string
        """
        try:
            response = self.ontop_client.execute(query)
        except:
            #logger.error("SPARQL query not successful")
            raise StackException("SPARQL query not successful")
        return json.loads(response)

    
    @staticmethod
    def upload_ontop_mapping():
        # Initialise ONTOP client and upload mapping file using default properties
        # from environment variables

        # Create a JVM module view and use it to import the required java classes
        stackClientsView = stackClientsGw.createModuleView()
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.ontop.OntopClient")

        try:
            # Create JAVA path object to mapping file
            f = stackClientsView.java.io.File(ONTOP_FILE)
            fp = f.toPath()
            # Update ONTOP mapping (requires JAVA path object)
            stackClientsView.OntopClient().updateOBDA(fp)
        except:
            #logger.error("Unable to update OBDA mapping.")
            raise StackException("Unable to update OBDA mapping.")


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
        except:
            #logger.error("Unable to retrieve JDBC connection properties.")
            raise StackException("Unable to retrieve JDBC connection properties.")

    
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
            #logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException(f'Unsuccessful JDBC interaction: {ex}')


    def check_point_feature_exists(self, lat, lon, feature_type, table=LAYERNAME):
        """
            This function checks whether an identical geo-feature already exists
            in PostGIS table
            Returns True if point already exists, False otherwise
        """
        try:
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
            #logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException(f'Unsuccessful JDBC interaction: {ex}')

    def get_feature_iris_in_circle(self, lat: float, lon: float, radius: float,
                                   table=LAYERNAME):
        """
            Retrieves IRIs of GeoFeatures within circle from given table

            Arguments:
                lat, lon - center coordinates in EPSG:4326
                radius - circle radius in km
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    # PostGIS documentation:
                    #   For geodetic coordinates, X is longitude and Y is latitude
                    #   Radium for ST_DWithin in [m]
                    curs.execute(f'SELECT iri from {table} WHERE \
                                   ST_DWithin(wkb_geometry, \
                                    ST_GeomFromText(\'POINT({lon} {lat})\', 4326)::geography, \
                                    {radius*1000})')
                    res = curs.fetchall()
                    # Extract results from tuples
                    res = [r[0] for r in res]
                    return res
        except Exception as ex:
            #logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException(f'Unsuccessful JDBC interaction: {ex}')


class GdalClient:
    
    def __init__(self):

        # Create a JVM module view and use it to import the required java classes
        stackClientsView = stackClientsGw.createModuleView()
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.gdal.GDALClient")
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions")
        try:
            self.client = stackClientsView.GDALClient()
            self.orgoptions = stackClientsView.Ogr2OgrOptions()
        except:
            #logger.error("Unable to initialise GdalClient.")
            raise StackException("Unable to initialise GdalClient.")


    def uploadGeoJSON(self, geojson_string, database=DATABASE, table=LAYERNAME):
        """
            Calls StackClient function with default upload settings
        """
        self.client.uploadVectorStringToPostGIS(database, table, geojson_string,
                                                self.orgoptions, True)


class GeoserverClient:

    def __init__(self):

        # Create a JVM module view and use it to import the required java classes
        stackClientsView = stackClientsGw.createModuleView()
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.geoserver.GeoServerClient")
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings")
        try:
            self.client = stackClientsView.GeoServerClient()
            self.vectorsettings = stackClientsView.GeoServerVectorSettings()
        except:
            #logger.error("Unable to initialise GeoServerClient.")
            raise StackException("Unable to initialise GeoServerClient.")    


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


def create_geojson_for_postgis(station_iri: str, station_name: str, 
                               station_type: str, lat: float, long: float, ):
    """

    """
    # Initialise GeoJSON
    geojson = {'type': 'Feature'}

    # Define properties
    props = {
        'iri': station_iri,
        'name': station_name,
        'geom_iri': station_iri + '/geometry',
        'type': station_type,
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
