# Retrieve elevation data from POSTGIS and export it to GeoTiff format for use with AERMAP

from flask import Blueprint, request
import agentlogging
import sqlalchemy
import rasterio
from rasterio.io import MemoryFile
import psycopg2

ROUTE = "/getElevationRaster"

get_elevation_raster_bp = Blueprint('get_elevation_raster_bp', __name__)
logger = agentlogging.get_logger("dev")

@get_elevation_raster_bp.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to generate .tiff file used as input to AERMAP")

    drivername = request.args["drivername"]
    username = request.args["username"]
    password = request.args["password"]
    host = request.args["host"]
    logger.info(host)
    drivername='psycopg2'

    url = sqlalchemy.engine.url.URL(drivername=drivername,
                                    database='postgres',
                                    username=username,
                                    password=password,
                                    host=host,
                                    port=5432)
    engine = sqlalchemy.create_engine(host)
    sql = "SELECT ST_AsGDALRaster(rast, 'GTiff') AS tiff FROM elevation"
    with engine.connect() as cnxn:
        result = cnxn.execute(sql)
    with MemoryFile(result.fetchall()[0][0].tobytes()) as memfile:
        ds = memfile.open()
    with rasterio.open('vis_data/PirmasensElevation.tif', 'w', **ds.profile) as dst:
        dst.write(ds.read())
    
    return 200
