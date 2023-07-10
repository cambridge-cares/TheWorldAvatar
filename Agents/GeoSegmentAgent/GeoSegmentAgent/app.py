from flask import Flask, request, json
import logging

from samgeo import tms_to_geotiff
from samgeo.text_sam import LangSAM

import geopandas as gpd
from sqlalchemy import create_engine


SAM = None

# Create the Flask app object
app = Flask(__name__)

from .utils.stack_configs import QUERY_ENDPOINT_STACK
from .utils.stack_configs import DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK
# from .utils.upload_postgis import upload_to_postgis
from .utils.stackclients import GdalClient, GeoserverClient, OntopClient, PostGISClient, create_geojson_for_postgis

'''
@app.route('/upload')
def upload():
    upload_to_postgis("segmented_houses.shp", "123table")
'''

@app.route('/setup')
def setup():
    global SAM
    if SAM is not None:
        return 'The SAM model has already been initialized'
    SAM = LangSAM(model_type='vit_b')
    return 'The SAM model has been initialized'

@app.route('/')
def default():
    global SAM
    if SAM is None:
        SAM = LangSAM(model_type='vit_b')

    logging.info('request.args: %s' % request.args)
    if not check_request_parameters(request.args):
        return 'Wrong request parameters'

    if SAM is None:
        return 'The SAM model has not been initialized'

    bbox = [-118.4932, 34.0404, -118.4903, 34.0417]

    tms_to_geotiff(output="Image.tif", bbox=bbox, zoom=19, source="Satellite", overwrite=True)

    text_prompt = "building rooftops"

    SAM.predict("Image.tif", "houses", box_threshold=0.3, text_threshold=0.3)
    logging.info('predicted')
    SAM.show_anns(
        cmap='Greys_r',
        add_boxes=False,
        alpha=1,
        title='Automatic Segmentation of Houses',
        blend=False,
        output='segmented_houses.tif',
    )

    # SAM.raster_to_vector("segmented_houses.tif", "segmented_houses.shp")
    SAM.raster_to_vector("segmented_houses.tif", "segmented_houses.geojson")
    logging.info('returning polygons')

    # Initialise relevant Stack Clients and parameters
    # postgis_client = PostGISClient(dburl=DB_QUERY_URL_STACK, dbuser=DB_QUERY_USER_STACK, dbpassword=DB_QUERY_PASSWORD_STACK, database="geosegment")
    gdal_client = GdalClient()
    geoserver_client = GeoserverClient()

    # Open the file and load it into a Python dictionary
    with open('segmented_houses.geojson', 'r') as f:
        data = json.load(f)

    # Convert the Python dictionary to a JSON-formatted string
    geojson_string = str(json.dumps(data))

    # Now geojson_string contains the GeoJSON data as a string

    gdal_client.uploadGeoJSON(geojson_string=geojson_string, database="postgres", table="segmented_houses")


    geoserver_client.create_workspace(workspace="geosegment")
    geoserver_client.create_postgis_layer(geoserver_workspace="geosegment", geoserver_layer="segmented_houses", postgis_database="postgres")

    #return send_file("./segmented_houses.shp")
    return geojson_string
    #return "Successfully finished the segmentation"


def check_request_parameters(request_arg):
    if request_arg is None:
        return False
    if request_arg == '':
        return False
    return True



'''

    use this to replace "uploadGeoJSON" for other shapefiles

    # Load the shapefile
    gdf = gpd.read_file("segmented_houses.geojson")

    # Define your database parameters
    username = "postgres" # replace with your actual username
    password = "postgis" # replace with your actual password

    # Create the connection string
    database_connection_url = f"postgresql://{username}:{password}@geo-postgis:5432/geosegment"

    # Create a SQLAlchemy engine
    engine = create_engine(database_connection_url)

    # Write the data
    # 'mytable' is the table name you want to create in your database
    gdf.to_postgis('segmented_houses', engine)
'''