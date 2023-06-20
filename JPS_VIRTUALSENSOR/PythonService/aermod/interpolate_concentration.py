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
from flask import Blueprint, request, jsonify, Response
from pyproj import Transformer
import requests
import agentlogging
import json
from scipy.spatial.distance import cdist

ROUTE = "/getInterpolatedConc"

get_concentration = Blueprint('get_concentration', __name__)
logger = agentlogging.get_logger("dev")


@get_concentration.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to process AERMOD dispersion matrix")
    aermod_output_url = request.args["dispersionMatrix"]
    xp = request.args["xp"]
    yp = request.args["yp"]

    # download file from url
    dispersion_file = requests.get(
        aermod_output_url, auth=requests.auth.HTTPBasicAuth('fs_user', 'fs_pass'))
    conc = get_conc(dispersion_file.text, xp, yp)
    return json.dumps({"result": conc})


# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
def get_conc(aermod_output, xp, yp):
    aermod_output_buffer = StringIO(aermod_output)
    data = pd.read_csv(aermod_output_buffer, delim_whitespace=True, skiprows=range(0, 8), header=None, names=[
                       'X', 'Y', 'AVERAGE CONC', 'ZELEV', 'ZHILL', 'ZFLAG', 'AVE', 'GRP', 'NUM HRS', 'NET ID'])
    conc_all = data['AVERAGE CONC']
    points = [(x, y) for x, y in zip(data['X'], data['Y'])]
    result = conc_all[cdist([(xp, yp)], points).argmin()]

    return result
