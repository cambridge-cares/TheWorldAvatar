################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Feb 2023                            #
################################################

# The purpose of this module is to provide functionality to interact with the
# overarching Docker Stack using the Stack Clients wrapped by py4jps

import glob
import jaydebeapi
import json

from agent.errorhandling.exceptions import StackException
from agent.kgutils.javagateway import stackClientsGw, jpsBaseLibGW
from agent.utils.env_configs import DATABASE, LAYERNAME, GEOSERVER_WORKSPACE
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT

# Initialise logger
from py4jps import agentlogging
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
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.postgis.PostGISClient")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.GeoServerClient")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings")


class PostGISClient(StackClient):
    
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
        NOTE: This function is necessary as stack clients' PostGISClient does not provide
              any general functionality to execute SQL queries; hence, they need to be
              defined here and executed using jaydebeapi 
              (which requires all JDBC properties incl. driver)
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


def create_geojson_for_postgis(building_iri : str, value_estimate : float = None,
                               kg_endpoint: str = QUERY_ENDPOINT) -> str:
    """
    Create (Geo)JSON string for upload to PostGIS database

    Initially, the GeoJSON needed to contain at least "name", "iri", and "endpoint"
    for FeatureInfoAgent to work (i.e. be able to retrieve data from PostGIS)
    Strictly, those are no longer needed, but kept for consistency with previous implementations
    Further properties can be added as needed, i.e. for styling purposes

    Arguments:
        building_iri {str} -- IRI of building/property to be uploaded to PostGIS
        value_estimate {float} -- Market value estimate of building (in GBP)
    """

    if building_iri:
        # Define properties
        props = {
            # Initially required by DTVF (potentially outdated, but kept for reference)
            'iri': building_iri,
            'name': building_iri.split('/')[-1].replace('>',''),
            'endpoint': kg_endpoint
        }
        # Property value estimate (upload required for styling purposes)
        try:
            props['price']: float(value_estimate)
        except Exception:
            props['price']: None

        geojson = props.dumps(props)
    else:
        geojson = None

    # Return (Geo)JSON string    
    return geojson
