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

ROUTE = "/getElevationGeoJSON"

get_elevation_geojson_bp = Blueprint('get_elevation_geojson_bp', __name__)
logger = agentlogging.get_logger("dev")

@get_elevation_geojson_bp.route(ROUTE, methods=['GET'])
def api():
    logger.info("Received request to process receptor.dat file used as input to AERMOD")
    aermod_output_url = request.args["dispersionMatrix"]
    srid = request.args["srid"]

    dispersion_file = requests.get(aermod_output_url, auth=requests.auth.HTTPBasicAuth('fs_user','fs_pass'))

    # download file from url
    return get_aermod_geojson(dispersion_file.text, srid)

# This script is only valid for a 1 hour simulation because this file shows the maximum concentration at each receptor
def get_aermod_geojson(aermod_output, srid):
    splitResult = aermod_output.splitlines()
    lines = [line.strip() for line in splitResult if 'XYINC' in line or 'ELEV ' in line]

    xyinc = np.array(lines[0].split()[1:]).astype(float)
    nx = np.round(xyinc[1]).astype(int)
    ny = np.round(xyinc[4]).astype(int)
    xl = xyinc[0]
    dx = xyinc[2]
    yl = xyinc[3]
    dy = xyinc[-1]
    xh = xl + (nx-1)*dx
    yh = yl + (ny-1)*dy

    xp = np.linspace(xl,xh,num = nx)
    yp = np.linspace(yl,yh,num = ny)

    transformer = Transformer.from_crs("epsg:" + srid, "epsg:4326")

    xv, yv = np.meshgrid(xp, yp)

    for j in range(len(yp)):
        for i in range(len(xp)):
            lat,lon = transformer.transform(xp[i],yp[j])
            xv[i,j] = lon
            yv[i,j] = lat

    elevData = np.zeros((nx,ny))
    currentRow = 1
    ind = 0
    tmp2 = []
    for line in lines[1:]:
        tmp = line.split()[3:]
        if (int(tmp[0]) == currentRow):
            tmp2.extend(tmp[1:])
        else:
            elevData[ind] = np.array(tmp2).astype(float)
            tmp2.clear()
            tmp2.extend(tmp[1:])
            ind += 1
            currentRow = int(tmp[0])
    elevData[ind] = np.array(tmp2).astype(float)

    fig,ax = plt.subplots()
    contour_level = 30
    crf = ax.contourf(xv, yv, elevData, levels = contour_level, cmap = plt.cm.jet)
    cbar = fig.colorbar(crf)

    try :
       cm = 1/2.54
       plt.figure(figsize=(10*cm, 20*cm))
       concBar = elevData.flatten()
       concBar = np.expand_dims(concBar,0)
       img = plt.imshow(concBar, cmap=plt.cm.jet)
       plt.gca().set_visible(False)
       cax = plt.axes([0.1, 0.2, 0.1, 1.0])
       plt.colorbar(orientation="vertical", cax=cax)
       cax.tick_params(axis='y', which='major', labelsize=28)
       plt.savefig("/var/www/html/elevation.png", dpi=300, bbox_inches='tight')
    except Exception as e :
       print(e)
    
    geojsonstring = geojsoncontour.contourf_to_geojson(contourf = crf, fill_opacity = 0.5)
    return jsonify(json.loads(geojsonstring)), 200