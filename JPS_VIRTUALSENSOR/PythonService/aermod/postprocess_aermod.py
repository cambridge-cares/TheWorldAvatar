from matplotlib.mlab import csd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure
import geojsoncontour
from io import StringIO
from flask import Blueprint, request, jsonify
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

    dispersion_file = requests.get(aermod_output_url, auth=requests.auth.HTTPBasicAuth('fs_user','fs_pass'))

    # download file from url
    return get_aermod_geojson(dispersion_file.text)

# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
def get_aermod_geojson(aermod_output):
    aermod_output_buffer = StringIO(aermod_output)
    data = pd.read_csv(aermod_output_buffer, delim_whitespace=True, skiprows=range(0,8), header=None, names=['X','Y','AVERAGE CONC', 'ZELEV', 'ZHILL','ZFLAG','AVE','GRP','RANK','NET ID','DATE(CONC)'])
    x_all = data['X']
    y_all = data['Y']

    x_set = sorted(set(x_all))
    y_set = sorted(set(y_all))

    x_matrix = np.empty((len(x_set), len(y_set)))
    y_matrix = np.empty((len(x_set), len(y_set)))

    for i in range(len(x_set)):
        for j in range (len(y_set)):
            x_index = x_set.index(x_set[i])
            y_index = y_set.index(y_set[j])
            x_matrix[x_index,y_index] = x_set[i]
            y_matrix[x_index,y_index] = y_set[j]

    conc_list = data['AVERAGE CONC']
    conc_matrix = np.empty((len(x_set), len(y_set)))

    for i in range(len(x_all)):
        x_index = x_set.index(x_all[i])
        y_index = y_set.index(y_all[i])
        conc_matrix[x_index,y_index] = conc_list[i]

    contour_level = 30
    fig, ax = plt.subplots()

    contourf = ax.contourf(x_matrix, y_matrix, conc_matrix, levels=contour_level,cmap=plt.cm.jet)
    geojsonstring = geojsoncontour.contourf_to_geojson(contourf = contourf, fill_opacity = 0.5)
    return jsonify(json.loads(geojsonstring)), 200