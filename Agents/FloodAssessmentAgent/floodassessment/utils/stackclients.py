################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Mar 2023                            #
################################################

# The purpose of this module is to provide functionality to interact with PostGIS 
# as part of the overarching Docker Stack using the Stack Clients wrapped by py4jps

import glob
import jaydebeapi

from floodassessment.kg_operations.javagateway import stackClientsGw, jpsBaseLibGW
from floodassessment.utils.env_configs import DATABASE#, WARNINGS_TABLE, BUILDINGS_TABLE
from floodassessment.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD

FLOODAREA_TABLE = 'test'
POPULATION_TABLE = 'population'

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
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.postgis")


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
            raise RuntimeError("Unable to retrieve JDBC connection properties.") from ex

    
    def connection_properties(self):
        """
        This function returns dictionary of JDBC connection properties to PostgreSQL database
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


    def get_number_of_affected_population(self, flood_area_iri: str, 
                                          population_raster_table: str = POPULATION_TABLE,
                                          flood_area_table: str = FLOODAREA_TABLE):
        """
        This function uses PostGIS' geospatial capabilities to calculate the sum of
        the population within a specific flood area polygon, i.e. uses summary statistics
        of the population raster data within a given polygon area
        ()

        flood_area_iri - IRI of flood area of interest
        population_raster_table - Name of table containing population raster data
        flood_area_table - Name of table containing flood area polygons
        """
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                with conn.cursor() as curs:
                    # ST_SummaryStats: calculate summary statistics of the raster data
                    # ST_Clip: clip the population raster data to the extent of the flood warning geometry
                    # ST_Transform: transform the flood warning geometry to the same SRS as the population raster data
                    curs.execute(f'SELECT \
                                   SUM((ST_SummaryStats(ST_Clip({population_raster_table}.rast, ST_Transform({flood_area_table}.wkb_geometry, ST_SRID({population_raster_table}.rast)), TRUE))).sum) \
                                   FROM \
                                       {population_raster_table}, {flood_area_table} \
                                   WHERE \
                                       {flood_area_table}.area_uri=\'{flood_area_iri}\';')
                    # Fetching the SQL results from the cursor only works on first call
                    # Recurring calls return empty list and curs.execute needs to be run again
                    res = curs.fetchall()
                    # Extract IRI results from list of tuples
                    res = [r[0] for r in res]
                    return res
        except Exception as ex:
            logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise RuntimeError('Unsuccessful JDBC interaction.') from ex
        

