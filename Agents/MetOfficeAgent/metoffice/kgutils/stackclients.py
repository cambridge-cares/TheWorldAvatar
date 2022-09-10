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
from metoffice.kgutils.javagateway import stackClientsGw
from metoffice.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD
from metoffice.utils.env_configs import DATABASE, ONTOP_FILE

# Initialise logger
#logger = agentlogging.get_logger("prod")


class OntopClient:
    
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
        self.conn_props = self.connection_properties()

    
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

    def check_table_exists(self, table: str):
        """
            This functions
        """
        try:
            with jaydebeapi.connect(*p.conn_props) as conn:
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
        


# test execution
o = OntopClient.upload_ontop_mapping()

p = PostGISClient()

e = p.check_table_exists('000355e7-9369-4424-9702-9283b0da369e')

print('')
