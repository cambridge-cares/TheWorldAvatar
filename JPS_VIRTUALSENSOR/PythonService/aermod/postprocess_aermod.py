from matplotlib.mlab import csd
import pandas as pd
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure
from matplotlib.colors import LinearSegmentedColormap
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
    concLog = np.log10(conc_matrix)

    crf = ax.contourf(x_matrix, y_matrix, concLog, levels=contour_level,cmap=plt.cm.jet)
    # cbar = fig.colorbar(crf,ax)
    try :
       cm = 1/2.54
       plt.figure(figsize=(10*cm, 20*cm))
       concBar = np.log10(np.asarray(conc_list))
       concBar = np.expand_dims(concBar,0)
       img = plt.imshow(concBar, cmap=plt.cm.jet)
       plt.gca().set_visible(False)
       cax = plt.axes([0.1, 0.2, 0.1, 1.0])
       plt.colorbar(orientation="vertical", cax=cax)
    #    plt.title(r"Logarithm of CO$_{2}$ concentrations ($\mu$g/m$^3$)")
       plt.savefig("/var/www/html/colourbar.png", dpi=300, bbox_inches='tight')
    except Exception as e :
       print(e)
    
    geojsonstring = geojsoncontour.contourf_to_geojson(contourf = crf, fill_opacity = 0.5)
    return jsonify(json.loads(geojsonstring)), 200