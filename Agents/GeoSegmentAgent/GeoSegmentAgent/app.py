from flask import Flask, jsonify, request, json
import logging

import pkg_resources
installed_packages = pkg_resources.working_set
installed_packages_list = sorted(["%s==%s" % (i.key, i.version) for i in installed_packages])
print(installed_packages_list)

from samgeo import tms_to_geotiff
from samgeo.text_sam import LangSAM


# Create the Flask app object
app = Flask(__name__)

@app.route('/')
def default():
    logging.info('request.args: %s' % request.args)
    if not check_request_parameters(request.args):
        return 'Wrong request parameters'

    bbox = [-118.4932, 34.0404, -118.4903, 34.0417]

    tms_to_geotiff(output="Image.tif", bbox=bbox, zoom=19, source="Satellite", overwrite=True)

    #Initialize LangSAM class
    sam = LangSAM()
    text_prompt = "building rooftops"

    sam.predict("Image.tif", "houses", box_threshold=0.3, text_threshold=0.3)

    sam.show_anns(
        cmap='Greys_r',
        add_boxes=False,
        alpha=1,
        title='Automatic Segmentation of Houses',
        blend=False,
        output='segmented_houses.tif',
    )

    sam.raster_to_vector("segmented_houses.tif", "segmented_houses.shp")
    logging.info('returning polygons')

    return send_file("/segmented_houses.tif", attachment_filename="segmented_houses.tif")


def check_request_parameters(request_arg):
    if request_arg is None:
        return False
    if request_arg == '':
        return False
    return True