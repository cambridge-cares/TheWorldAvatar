import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import geojsoncontour
from io import StringIO
from flask import Blueprint, request, jsonify, Response
from pyproj import Transformer
import requests
import agentlogging
import json
import os

ROUTE = "/getAermodGeoJSON"

get_aermod_geojson_bp = Blueprint('get_aermod_geojson_bp', __name__)
logger = agentlogging.get_logger("dev")


@get_aermod_geojson_bp.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to process AERMOD dispersion matrix")
    aermod_output_url = request.args["dispersionMatrix"]
    srid = request.args["srid"]
    # download file from url
    dispersion_file = requests.get(
        aermod_output_url, auth=requests.auth.HTTPBasicAuth('fs_user', 'fs_pass'))

    return get_aermod_geojson(dispersion_file.text, srid)


# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
def get_aermod_geojson(aermod_output, srid):
    aermod_output_buffer = StringIO(aermod_output)
    data = pd.read_csv(aermod_output_buffer, delim_whitespace=True, skiprows=range(0, 8), header=None, names=[
                       'X', 'Y', 'AVERAGE CONC', 'ZELEV', 'ZHILL', 'ZFLAG', 'AVE', 'GRP', 'NUM HRS', 'NET ID'])
    x_all = data['X']
    y_all = data['Y']

    x_set = sorted(set(x_all))
    y_set = sorted(set(y_all))

    x_matrix = np.empty((len(x_set), len(y_set)))
    y_matrix = np.empty((len(x_set), len(y_set)))

    transformer = Transformer.from_crs("epsg:" + srid, "epsg:4326")

    for i in range(len(x_set)):
        for j in range(len(y_set)):
            lat, lon = transformer.transform(x_set[i], y_set[j])
            x_matrix[i, j] = lon
            y_matrix[i, j] = lat

    conc_list = data['AVERAGE CONC']
    conc_matrix = np.empty((len(x_set), len(y_set)))

    average_conc = sum(conc_list)/len(conc_list)
    logger.info('Average concentration = ' + str(average_conc))

    if (average_conc / 1e5 > 1):
        use_g = True
    else:
        use_g = False

    elev_list = data['ZELEV']
    elev_matrix = np.empty((len(x_set), len(y_set)))

    for i in range(len(conc_list)):
        x_index = x_set.index(data['X'][i])
        y_index = y_set.index(data['Y'][i])
        if (use_g):
            conc_value = conc_list[i] / 1e6
        else:
            conc_value = conc_list[i]
        conc_matrix[x_index, y_index] = conc_value
        elev_matrix[x_index, y_index] = elev_list[i]

    contour_level = 30
    _, ax = plt.subplots()

    contourf = ax.contourf(x_matrix, y_matrix, conc_matrix,
                           levels=contour_level, cmap=plt.cm.jet)

    contourf_elev = ax.contourf(x_matrix, y_matrix, elev_matrix,
                                levels=contour_level, cmap=plt.cm.jet)

    plt.colorbar(contourf)
    ax.remove()
    if (use_g):
        plt.title("Concentration (g/m$^3$)")
    else:
        plt.title("Concentration ($\mu$g/m$^3$)")
    plt.savefig("colorbar.png", bbox_inches='tight', transparent=True, dpi=300)

    files = {'colorbar': open('colorbar.png', 'rb')}

    response = requests.post(os.environ['FILE_SERVER'] + 'colorbar/colorbar.png',
                             files=files, auth=requests.auth.HTTPBasicAuth('fs_user', 'fs_pass'))

    url = response.headers.get('colorbar')
    logger.info(url)

    geojsonstring = geojsoncontour.contourf_to_geojson(
        contourf=contourf, fill_opacity=0.5)

    geojsonstring_elev = geojsoncontour.contourf_to_geojson(
        contourf=contourf_elev, fill_opacity=0.5)

    response = {'contourgeojson': json.loads(
        geojsonstring), 'colourbar': url, 'contourgeojson_elev': json.loads(geojsonstring_elev)}

    return jsonify(response), 200
