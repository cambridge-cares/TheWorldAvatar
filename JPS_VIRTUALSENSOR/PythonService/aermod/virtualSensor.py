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

ROUTE = "/virtualSensor"

virtualSensor = Blueprint('virtualSensor', __name__)
logger = agentlogging.get_logger("dev")

@virtualSensor.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to process virtual sensor data")
    aermod_output_url = request.args["dispersionMatrix"]
    srid = request.args["srid"]

    dispersion_file = requests.get(aermod_output_url, auth=requests.auth.HTTPBasicAuth('fs_user','fs_pass'))

    # download file from url
    return processVirtualSensorData(dispersion_file.text, srid)

# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
def processVirtualSensorData(aermod_output, srid):
    aermod_output_buffer = StringIO(aermod_output)
    data = pd.read_csv(aermod_output_buffer,delim_whitespace=True, skiprows=range(0,8), header=None, names=['X','Y','AVERAGE CONC', 'ZELEV', 'ZHILL','ZFLAG','AVE','GRP','DATE'])
    dateTime = data['DATE']
    uniqueDateTime = set(dateTime)
    numberSensors = round(dateTime.size/len(uniqueDateTime))

    

    res = []

    fig,axs = plt.subplots(numberSensors)
    transformer = Transformer.from_crs("epsg:" + srid, "epsg:4326")
    for i in range(numberSensors):
        indexList = list(range(i,dateTime.size,numberSensors))
        singleSensorData = data.iloc[indexList]
        pollutantConcentration = singleSensorData['AVERAGE CONC']
        time = singleSensorData['DATE']
        height = singleSensorData['ZFLAG'][i]
        time = list(range(1,len(uniqueDateTime)+1))
        lat,lon = transformer.transform(singleSensorData['X'][i], singleSensorData['Y'][i])
        sensorData = {"longitude":lon,"latitude":lat,"concentrations":list(pollutantConcentration)}
        res.append(sensorData)
        ax = axs[i]
        ax.plot(time,pollutantConcentration)
        titleString = "Concentration at latitude = " + str(round(lat,2)) + ", longitude  = " + \
        str(round(lon,2)) + " , height = " + str(round(height,2)) 
        ax.set_title(titleString)
        ax.set_xlabel("Time (hours)",fontsize = 12, fontweight = "bold")
        ax.set_ylabel("Concentration ($\mu$g/m$^3$)",fontsize = 12, fontweight = "bold")

    plt.tight_layout()
    plt.savefig("/vis_data/virtualSensor.png", dpi=300, bbox_inches='tight')    
    return jsonify(res),200