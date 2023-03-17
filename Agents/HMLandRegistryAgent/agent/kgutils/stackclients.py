################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Feb 2023                            #
################################################

# The purpose of this module is to provide functionality to interact with the
# overarching Docker Stack using the Stack Clients wrapped by py4jps

import glob
import jaydebeapi

from agent.errorhandling.exceptions import StackException
from agent.kgutils.javagateway import stackClientsGw, jpsBaseLibGW
from agent.utils.env_configs import DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, \
                                    BUILDINGS_TABLE
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
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.postgis.PostGISClient")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.GeoServerClient")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings")
    stackClientsGw.importPackages(stackClients_view, "com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder")
    # Include required third-party packages
    stackClientsGw.importPackages(stackClients_view, "it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.VTGeometryEncoder")


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
        TODO: In next iteration, check whether this can be replaced by using
              `PostGISClient::getRemoteStoreClient` methods which returns a
              pre-configured RemoteRDBStoreClient object to make SQL requests
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
        

    def create_table(self, table=LAYERNAME, 
                     column_definitions={'iri': 'VARCHAR(150)',
                                         'name': 'VARCHAR(150)',
                                         'endpoint': 'VARCHAR(150)',
                                         'price': 'DOUBLE PRECISION'}):
        """
        This function creates the given table with the specified columns
        in the database and

        """

        # Create column definitions string
        col_def_str = ''
        for col, col_type in column_definitions.items():
            col_def_str += f'{col} {col_type},'
        col_def_str = col_def_str[:-1]

        # Create SQL update
        try:
            with jaydebeapi.connect(*self.conn_props) as conn:
                # Create a cursor object ...
                with conn.cursor() as curs:
                    # ... and create data base table
                    sql_update = f'CREATE TABLE IF NOT EXISTS \"{table}\" ({col_def_str})'
                    curs.execute(sql_update)
                    # ... and create a unique constraint on the iri column
                    # NOTE: Create "unique" constraint name to avoid potential issues with other constraints
                    sql_update = f'ALTER TABLE \"{table}\" ADD CONSTRAINT {table}_iri_unique UNIQUE (iri)'
                    curs.execute(sql_update)
        except Exception as ex:
            logger.error(f'Unsuccessful JDBC interaction: {ex}')
            raise StackException('Unsuccessful JDBC interaction.') from ex
        
    
    def upload_property_value(self, building_iri:str, table=LAYERNAME,
                              value_estimate:str=None,
                              kg_endpoint:str=QUERY_ENDPOINT):
        """
        This function uploads the latest market value of a property to PostGIS
        (appends new row if 'building_iri' is not yet in the table, otherwise 
         'price' field gets updated/overwritten)
        
        Initially, "name", "iri", and "endpoint" were required for the FeatureInfoAgent
        to work (i.e. be able to retrieve data from PostGIS). Strictly, those are no 
        longer needed, but kept for consistency with previous implementations
        Further properties can be added as needed, i.e. for styling purposes

        Arguments:
            building_iri {str} -- IRI of building/property to be uploaded to PostGIS
            value_estimate {str} -- Market value estimate of building (in GBP)
        """
        
        if building_iri:
            # Condition data to upload (incl. columns (initially?) required by DTVF)
            props = {
                'iri': building_iri,
                'endpoint': kg_endpoint,
                'name': None,
                'price': None
            }
            try:
                props['name'] = building_iri.split('/')[-1].replace('>','')
            except Exception:
                pass
            # Property value estimate (upload required for styling purposes)
            try:
                props['price'] = float(value_estimate)
            except Exception:
                pass
            
            # Prepare SQL update string (remove keys with empty values first)
            props = {k: v for k,v in props.items() if v is not None}
            cols = f"( {', '.join(props.keys())} )"
            vals = "VALUES ("
            for v in props.values():
                if isinstance(v, str):
                    vals += f"'{v}', "
                elif isinstance(v, float):
                    vals += f"{v}, "
            vals = vals[:-2]
            vals += ")"

            # Create SQL update
            try:
                with jaydebeapi.connect(*self.conn_props) as conn:
                    # Create a cursor object ...
                    with conn.cursor() as curs:
                        # ... and execute the SQL query
                        # When DO UPDATE is specified, a special virtual table called EXCLUDED
                        # is available for use within the UPDATE clause. The table contains 
                        # the values suggested in the original INSERT command (i.e. the ones 
                        # conflicting with the existing table values)
                        sql_update = f"INSERT INTO \"{table}\" {cols} {vals} \
                                       ON CONFLICT (iri) DO UPDATE SET \
                                        price = EXCLUDED.price"
                        curs.execute(sql_update)
                        if curs.rowcount != 1:
                            raise StackException(f'Expected to update 1 row; however {curs.rowcount} rows affected.')
            except Exception as ex:
                logger.error(f'Unsuccessful JDBC interaction: {ex}')
                raise StackException('Unsuccessful JDBC interaction.') from ex


class GeoserverClient(StackClient):

    def __init__(self, sql_view=None, table_name=LAYERNAME, escape_sql=False,
                 geom_name='wkb_geometry', geom_type='MultiPolygon', geom_srid='4326'):
        """
        Initialise Geoserver client with default settings (i.e. settings required to
        created virtual table with building footprints and market value estimates)
        For details, please see:
        https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-data-uploader/README.md#geoserver-options
        Example for virtual table, please see:
        https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/examples/datasets/inputs/config/cropmap.json

        Arguments:
            sql_view {str} -- SQL query used to construct virtual table
            table_name {str} -- Name of virtual table to create
            escape_sql {bool} -- Whether to escape SQL query
            geom_name {str} -- Name of column with the geometry
            geom_type {str} -- Geometry type of geometry column
            geom_srid {str} -- SRID EPSG code
        """
        
        if not sql_view:
            # Create default SQL query to create virtual table
            sql_view = self.sql_query_to_create_virtual_table()
        
        # Initialise Geoserver with default settings
        try:
            self.client = self.stackClients_view.GeoServerClient()
            self.vectorsettings = self.stackClients_view.GeoServerVectorSettings()
            
            # Create geometry object (to ensure proper representation of geometry in GeoServer)
            # https://github.com/geosolutions-it/geoserver-manager/blob/master/src/main/java/it/geosolutions/geoserver/rest/encoder/metadata/virtualtable/VTGeometryEncoder.java
            geometry = self.stackClients_view.VTGeometryEncoder(geom_name, geom_type, geom_srid)
            
            # Create virtual table and set properties
            # https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-clients/src/main/java/com/cmclinnovations/stack/clients/geoserver/UpdatedGSVirtualTableEncoder.java
            self.virtualtableencoder = self.stackClients_view.UpdatedGSVirtualTableEncoder()
            self.virtualtableencoder.setName(table_name)
            self.virtualtableencoder.setSql(sql_view)
            self.virtualtableencoder.setEscapeSql(escape_sql)
            self.virtualtableencoder.setGeometry(geometry)
            
            # Add virtual table settings to vector settings
            # https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-clients/src/main/java/com/cmclinnovations/stack/clients/geoserver/GeoServerVectorSettings.java
            self.vectorsettings.setVirtualTable(self.virtualtableencoder)

        except Exception as ex:
            logger.error("Unable to initialise GeoServerClient.")
            raise StackException("Unable to initialise GeoServerClient.") from ex


    def sql_query_to_create_virtual_table(self, sales_table=LAYERNAME, 
                                          bldgs_table=BUILDINGS_TABLE):
        """
        Return SQL query/view to create virtual table for GeoServer
        NOTE: This requires the EPC Instantiation Agent to be run first (which is 
              the natural sequence anyway) AND relies on hardcoded column names
        """
        query = f"""SELECT {bldgs_table}.iri, 
                           {bldgs_table}.wkb_geometry, 
                           {bldgs_table}.\"building height\",
                           {sales_table}.price
                    FROM 
                        {bldgs_table}, 
                        {sales_table}
                    WHERE
                        {bldgs_table}.iri = {sales_table}.iri
                    """
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())
        return query


    def create_workspace(self, workspace=GEOSERVER_WORKSPACE):
        self.client.createWorkspace(workspace)

    
    def create_postgis_layer(self, geoserver_workspace=GEOSERVER_WORKSPACE,
                                   geoserver_layer=LAYERNAME,
                                   postgis_database=DATABASE):
        """
        Calls StackClient function with default settings to create VIRTUAL TABLE
        Please note: Postgis database table assumed to have same name as
                     Geoserver layer
        """
        self.client.createPostGISLayer(None, geoserver_workspace, postgis_database,
                                       geoserver_layer, self.vectorsettings)
