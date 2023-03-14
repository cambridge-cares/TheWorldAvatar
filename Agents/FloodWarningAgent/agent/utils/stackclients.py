################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Feb 2023                            #
################################################

# The purpose of this module is to provide functionality to interact with the
# overarching Docker Stack using the Stack Clients wrapped by py4jps

import geojson
import glob
import jaydebeapi
import json
import re
from shapely.geometry import shape, MultiPolygon

from agent.datainstantiation.ea_data import retrieve_flood_area_polygon
from agent.errorhandling.exceptions import StackException
from agent.utils.env_configs import DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, \
                                    ONTOP_FILE, BUILDINGS_TABLE
from agent.utils.javagateway import stackClientsGw, jpsBaseLibGW
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD, ONTOP_URL

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


    def check_flood_area_exists(self, flood_area_iri, polygon_iri, table=LAYERNAME):
        """
        This function checks whether the flood area already exists in the database
        Returns True if area already exists, False otherwise
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    curs.execute(f'SELECT iri=\'{flood_area_iri}\' AND \
                                   geom_iri=\'{polygon_iri}\' from {table}')
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
        

    def get_buildings_within_floodarea(self, flood_area_iri: str, 
                                       buildings_table: str = BUILDINGS_TABLE,
                                       flood_area_table: str = LAYERNAME):
        """
        This function uses PostGIS' geospatial capabilities to retrieve the IRIs
        of all buildings which have a footprint (i.e. polygon) within the polygon
        of a given flood area

        flood_area_iri - IRI of flood area of interest
        buildings_table - Name of table containing building footprints
        flood_area_table - Name of table containing flood area polygons
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    curs.execute(f'SELECT DISTINCT {buildings_table}.iri \
                                   FROM {buildings_table}, {flood_area_table} \
                                   WHERE ST_Within({buildings_table}.wkb_geometry, {flood_area_table}.wkb_geometry) \
                                   AND {flood_area_table}.area_uri=\'{flood_area_iri}\'')
                    # Fetching the SQL results from the cursor only works on first call
                    # Recurring calls return empty list and curs.execute needs to be run again
                    res = curs.fetchall()
                    # Extract IRI results from list of tuples
                    res = [r[0] for r in res]
                    return res
        except Exception as ex:
            logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException('Unsuccessful JDBC interaction.') from ex


    def set_flood_area_activity(self, activity: bool, area_uri: str, table=LAYERNAME):
        """
        This function sets the 'activity' of a flood area in the database to allow
        for visualisation of areas associated with active flood warnings only (later)

        activity: Boolean value whether a flood area is currently affected or not
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                # Create a cursor object ...
                with conn.cursor() as curs:
                    # ... and execute the SQL query
                    curs.execute(f"UPDATE {table} SET active=? WHERE area_uri=?", (activity, area_uri))
                    # Get the number of rows affected by the update
                    num_rows = curs.rowcount
                    return num_rows
        except Exception as ex:
            logger.error(f'Error updating field: {ex}')
            raise StackException('Error updating PostGIS field') from ex


    def set_flood_area_severity(self, severity: int, area_uri: str, table=LAYERNAME):
        """
        This function sets the 'severity' of a flood area in the database to allow
        for additional visualisation styling based on severity level

        severity: Integer score (1...4) indicating severity level of flood warning
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                # Create a cursor object ...
                with conn.cursor() as curs:
                    # ... and execute the SQL query
                    curs.execute(f"UPDATE {table} SET severity=? WHERE area_uri=?", (severity, area_uri))
                    # Get the number of rows affected by the update
                    num_rows = curs.rowcount
                    return num_rows
        except Exception as ex:
            logger.error(f'Error updating field: {ex}')
            raise StackException('Error updating PostGIS field') from ex


class GdalClient(StackClient):
    
    def __init__(self):
        # Initialise GdalClient with default upload/conversion settings
        try:
            self.client = self.stackClients_view.GDALClient()
            self.orgoptions = self.stackClients_view.Ogr2OgrOptions()
            # PostGIS "requires" geometry type for newly created layer/column 
            # --> when new table gets created using 'uploadVectorStringToPostGIS'
            #     this seems to be set to geometry type of first Feature (likely POLYGON)
            #     Subsequently, the upload will fail for MULTIPOLYGONS
            # 
            # 'orgoptions' can be amended to create table which supports Polygons and MultiPolygons:
            # self.orgoptions.addOtherOption("-nlt", 'PROMOTE_TO_MULTI')
            # 
            # However, calling this method from Python via py4jps doesn't seem to work
            # (see: https://github.com/cambridge-cares/TheWorldAvatar/issues/609)
            # 
            # Further details:
            # https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
            # https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-nlt
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


def create_geojson_for_postgis(areal_extend_iri: str, label: str, polygon_uri: str, 
                               area_uri: str, kg_endpoint: str, area_types: list =[], 
                               water_body_type: str = None) -> str:
    """
    Create GeoJSON string for upload to PostGIS database

    Initially, the GeoJSON needed to contain at least "name", "iri", and "endpoint"
    for FeatureInfoAgent to work (i.e. be able to retrieve data from PostGIS)
    This is no longer the case, but these properties are kept for consistency with
    previous agent implementations, i.e. 
    https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/MetOfficeAgent/agent/kgutils/stackclients.py

    Further properties can be added as needed, i.e. for styling purposes
    """

    # Retrieve GeoJSON polygon from EA API
    logger.info("Retrieving FloodArea GeoJSON polygon from EA API ...")
    ea_geojson = retrieve_flood_area_polygon(polygon_uri)

    # Define properties
    props = {
        'iri': areal_extend_iri,
        'name': label,
        'endpoint': kg_endpoint,
        'area_uri': area_uri,
        'geom_iri': polygon_uri,
        # NOTE: 'active' property is used to filter out inactive flood areas
        #        for visualisation using PostGIS / GeoServer later --> initialised as 
        #       'True' as missing areas are only created for active flood warnings
        'active': True,
        # Initialise 'severity' property (populated later)
        'severity': None
    }
    # Add optional properties
    area_type = []
    for t in area_types:
        area_type.extend(re.findall(r'AlertArea|WarningArea', t, re.IGNORECASE))
    area_type = list(set(area_type))
    area_type = None if len(area_type) != 1 else area_type[0]
    if area_type: props['area_type'] = area_type
    if water_body_type: props['waterbody'] = water_body_type

    # Create additional properties for GeoJSON
    #NOTE: This assumes that the polygon is a FeatureCollection with only one feature
    #      (which is in line with design/assumption in 'retrieve_flood_area_polygon')
    props.update(ea_geojson['features'][0]['properties'])

    #NOTE: Some of the GeoJSON polygons returned from the EA API are of type Polygon,
    #      while others are of type MultiPolygon. PostGIS (by default) only supports
    #      one type. Hence, all Polygons are "upgraded" to MultiPolygons.
    #      This is a workaround as the ogr2ogr options do not seem to work when called
    #      via py4jps (see NOTE in GDALClient init)
    # Extract geometry and convert to a Shapely polygon
    shapely_polygon = shape(ea_geojson['features'][0]['geometry'])
    if shapely_polygon.geom_type == 'Polygon':
        # Create a MultiPolygon from the Shapely polygon
        multipolygon = MultiPolygon([shapely_polygon])
    else:
        multipolygon = shapely_polygon
    # Convert the MultiPolygon to a GeoJSON string
    postgis_geojson = geojson.dumps(geojson.Feature(geometry=multipolygon, properties=props))
    
    return postgis_geojson
