import pandas as pd
import numpy as np
import re, json
import matplotlib.pyplot as plt
from io import StringIO
from pyproj import Proj, transform, Transformer
import geojsoncontour
import os

def postprocessEpisode(outputPath,crsName):
    concFileName = "3D_instantanous_mainconc_center.dat"
    filePath = os.path.join(outputPath,concFileName)
    with open(filePath) as f:
        content = re.sub(r'[ ]+',',',re.sub(r'[ ]+\n','\n',f.read()))
        f.close()
    content = StringIO(content)
    data = pd.read_csv(content, delimiter=',')

    # collect coordinates
    x_all = data["X(m)"]
    y_all = data["Y(m)"]
    z_all = data["Z(m)"]

    x_set = sorted(set(x_all))
    y_set = sorted(set(y_all))
    z_set = sorted(set(z_all))

    # create a matrix for each pollutant, then find where the input is located within the matrix
    pollutants = list(data.columns)[7:]

    # create matrices for x y dimensions, they're repeated over each value of height
    x_at_z1 = x_all.loc[z_all == z_set[0]]
    y_at_z1 = y_all.loc[z_all == z_set[0]]
    x_matrix = np.empty((len(x_set),len(y_set)))
    y_matrix = np.empty((len(x_set),len(y_set)))
    
    transformer = Transformer.from_crs(crsName, "epsg:4326")

    for i in range(len(x_at_z1)) :
        x_index = x_set.index(x_at_z1[i])
        y_index = y_set.index(y_at_z1[i])
        lat,lon=transformer.transform(x_at_z1[i], y_at_z1[i])
        x_matrix[x_index,y_index] = lon
        y_matrix[x_index,y_index] = lat

    # initialise result
    pol_result = {}

    for pol in pollutants:
        # initialise array
        pol_matrix = np.empty((len(x_set),len(y_set),len(z_set)))

        # collect data in a list first
        pol_list = data[pol]
        pol_max = pol_list[0]
        pol_min = pol_list[0]
        # transform list into a matrix
        for i in range(len(pol_list)) :
            x_index = x_set.index(x_all[i])
            y_index = y_set.index(y_all[i])
            z_index = z_set.index(z_all[i])
            pol_matrix[x_index,y_index,z_index] = pol_list[i]

        pol_result[pol] = pol_matrix
    
    fig, ax = plt.subplots()
    levels = 20

    hydrocarbons = ["C2H6", "HCHO", "CH3CHO", "C2H4", "PAN", "nC4H10", "CH3COC2H5", "C3H6", "oXylene", "isoprene"]
    nox_species = ["NO","NO2"]
    others = ["NO","NO2","O3", "SO2","PM2.5","PM10"]

    result = {} # final json object to write for visualisation
    result["dz"] = z_set

    # sum hydrocarbon concentrations
    hc = np.empty((len(x_set),len(y_set),len(z_set)))
    for species in hydrocarbons:
        hc += pol_result[species]
    
    # collect geojson layers for each height for hc
    result["HC"] = []
    for i in range(len(z_set)):
        CS = ax.contourf(x_matrix, y_matrix, hc[:,:,i], levels=levels,cmap=plt.cm.jet)
        geojsonstring = geojsoncontour.contourf_to_geojson(contourf=CS,fill_opacity=0.5)
        geojson = json.loads(geojsonstring)
        result["HC"].append(geojson)
    
    # sum nox concentrations
    nox = np.empty((len(x_set),len(y_set),len(z_set)))
    for species in nox_species:
        nox += pol_result[species]
    
    # collect geojson layers for each height for nox
    result["NOx"] = []
    for i in range(len(z_set)):
        CS = ax.contourf(x_matrix, y_matrix, nox[:,:,i], levels=levels,cmap=plt.cm.jet)
        geojsonstring = geojsoncontour.contourf_to_geojson(contourf=CS,fill_opacity=0.5)
        geojson = json.loads(geojsonstring)
        result["NOx"].append(geojson)

    for pol in others:
        result[pol] = []
        for i in range(len(z_set)):
            CS = ax.contourf(x_matrix, y_matrix, pol_result[pol][:,:,i], levels=levels,cmap=plt.cm.jet)
            
            geojsonstring = geojsoncontour.contourf_to_geojson(contourf = CS, fill_opacity = 0.5)
            
            geojson = json.loads(geojsonstring)
            result[pol].append(geojson)

    jsonFile = os.path.join(outputPath,"contour.geojson")
    with open(jsonFile,'w') as f:
        json.dump(result,f)
