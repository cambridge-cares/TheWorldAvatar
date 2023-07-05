from flask import Flask, jsonify, request, json, send_file
import logging

from samgeo import tms_to_geotiff
from samgeo.text_sam import LangSAM

SAM = None

# Create the Flask app object
app = Flask(__name__)

from .stack_utils.stack_configs import QUERY_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK
from .stack_utils.stack_configs import DB_UPDATE_URL_STACK, DB_UPDATE_USER_STACK, DB_UPDATE_PASSWORD_STACK
from .stack_utils.stack_configs import DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK

@app.route('/setup')
def setup():
    global SAM
    if SAM is not None:
        return 'The SAM model has already been initialized'
    SAM = LangSAM(model_type='vit_b')
    return 'The SAM model has been initialized'

@app.route('/')
def default():
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

    SAM.raster_to_vector("segmented_houses.tif", "segmented_houses.shp")
    logging.info('returning polygons')

    return send_file("/segmented_houses.tif", attachment_filename="segmented_houses.tif")




def check_request_parameters(request_arg):
    if request_arg is None:
        return False
    if request_arg == '':
        return False
    return True