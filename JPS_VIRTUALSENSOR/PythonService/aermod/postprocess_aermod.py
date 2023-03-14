from matplotlib.mlab import csd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure
import geojsoncontour
from io import StringIO
from flask import Blueprint, request, jsonify
from pyproj import Transformer
import requests
import agentlogging
import json

ROUTE = "/getAermodGeoJSON"

get_aermod_geojson_bp = Blueprint('get_aermod_geojson_bp', __name__)
logger = agentlogging.get_logger("dev")

@get_aermod_geojson_bp.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to process AERMOD dispersion matrix")
    aermod_output_url = request.args["dispersionMatrix"]
    srid = request.args["srid"]

    dispersion_file = requests.get(aermod_output_url, auth=requests.auth.HTTPBasicAuth('fs_user','fs_pass'))

    # download file from url
    return get_aermod_geojson(dispersion_file.text, srid)

# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
def get_aermod_geojson(aermod_output, srid):
    aermod_output_buffer = StringIO(aermod_output)
    data = pd.read_csv(aermod_output_buffer, delim_whitespace=True, skiprows=range(0,8), header=None, names=['X','Y','AVERAGE CONC', 'ZELEV', 'ZHILL','ZFLAG','AVE','GRP','RANK','NET ID','DATE(CONC)'])
    x_all = data['X']
    y_all = data['Y']

    x_set = sorted(set(x_all))
    y_set = sorted(set(y_all))

    x_matrix = np.empty((len(x_set), len(y_set)))
    y_matrix = np.empty((len(x_set), len(y_set)))

    transformer = Transformer.from_crs("epsg:" + srid, "epsg:4326")

    for i in range(len(x_set)):
        for j in range (len(y_set)):
            x_index = x_set.index(x_set[i])
            y_index = y_set.index(y_set[j])
            lat,lon = transformer.transform(x_set[i], y_set[j])
            x_matrix[x_index,y_index] = lon
            y_matrix[x_index,y_index] = lat

    conc_list = data['AVERAGE CONC']
    conc_matrix = np.empty((len(x_set), len(y_set)))

    for i in range(len(x_all)):
        x_index = x_set.index(x_all[i])
        y_index = y_set.index(y_all[i])
        conc_matrix[x_index,y_index] = conc_list[i]

    contour_level = 30
    fig, ax = plt.subplots()

    crf = ax.contourf(x_matrix, y_matrix, np.log10(conc_matrix), levels=contour_level,cmap=plt.cm.jet)
    # cbar = fig.colorbar(crf,ax)
    try :
       fig2,ax2 = plt.subplots()
       ax2.colorbar(crf,ax2)
       ax2.remove()
       plt.savefig("/var/www/html/colourbar.png")
    except Exception as e :
       print(e)
    
    geojsonstring = geojsoncontour.contourf_to_geojson(contourf = crf, fill_opacity = 0.5)
    return jsonify(json.loads(geojsonstring)), 200