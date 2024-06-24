import geopandas as gpd
from sqlalchemy import create_engine
import logging

from GeoSegmentAgent.stack_utils.stack_configs import QUERY_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK
from GeoSegmentAgent.stack_utils.stack_configs import DB_UPDATE_URL_STACK, DB_UPDATE_USER_STACK, DB_UPDATE_PASSWORD_STACK
from GeoSegmentAgent.stack_utils.stack_configs import DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK


def upload_to_postgis(shapefile_path, table_name):
    logging.info('uploading to postgis')
    logging.info('shapefile_path: %s' % shapefile_path)
    username = DB_UPDATE_USER_STACK
    password = DB_UPDATE_PASSWORD_STACK
    logging.info('username: %s' % username)
    logging.info('password: %s' % password)
    logging.info('DB_UPDATE_URL_STACK: %s' % DB_UPDATE_URL_STACK)


    # Load the shapefile
    gdf = gpd.read_file(shapefile_path)

    # Create the connection string
    # For this example, I'll assume a PostgreSQL database with PostGIS installed
    # on localhost, port 5432, with the name 'mydatabase' and user 'myuser' with password 'mypassword'
    #database_connection_url = 'postgresql://myuser:mypassword@localhost:5432/mydatabase'
    database_connection_url = f"postgresql://{username}:{password}@geo-postgis:5432/geosegment"


    # Create a SQLAlchemy engine
    engine = create_engine(database_connection_url)

    # Write the data
    # 'mytable' is the table name you want to create in your database
    gdf.to_postgis(table_name, engine)


