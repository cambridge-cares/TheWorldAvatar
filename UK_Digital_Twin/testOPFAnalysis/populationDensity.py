import folium, csv
import matplotlib as mpl
import matplotlib.colors as colors
from matplotlib.colors import BoundaryNorm
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm


def read_single_csv(input_path):
    import pandas as pd
    df_chunk=pd.read_csv(input_path,chunksize=1000)
    res_chunk=[]
    for chunk in df_chunk:
        res_chunk.append(chunk)
    res_df=pd.concat(res_chunk)
    res_df = res_df.values.tolist()
    return res_df

def colourPicker(valueList):
    # Define a colormap
    cmap = plt.cm.get_cmap('coolwarm') 
    # Normalize the values to the range [0, 1] to map them to colors
    norm = plt.Normalize(vmin=valueList.min(), vmax=valueList.max())
    # Generate the colour list according to the values
    colourList = [colors.to_hex(c) for c in cmap(norm(valueList))] ## the normal ordering of the colour map
    ##  colourList = [colors.to_hex(c) for c in cmap(1-norm(valueList))] ## reverse the colour map  
    return colourList

def colourBarCreator(upperbound, lowerbound, filePath, cmapName = 'coolwarm'):
    fig, ax = plt.subplots(figsize=(0.5, 12))
    fig.subplots_adjust(bottom=0.5)

    cmap = plt.cm.get_cmap(cmapName)    
    norm=plt.Normalize(lowerbound, upperbound)
    sample_values = np.linspace(lowerbound, upperbound, 101)
   
    cbar = mpl.colorbar.ColorbarBase(ax, 
                                    cmap = cmap,
                                    norm = norm,
                                    boundaries = sample_values)
    cbar.set_label("Population density (People/$\mathregular{km^2}$)")
    fig.set_size_inches(0.5, 12)

    plt.savefig(filePath + 'populationDensityLegend.png', dpi = 200, bbox_inches = "tight", transparent = True)

    return 

def pdGeo(dataPath, fileName, resolutionLevel:str):
    if not dataPath[-1] == '/':
        dataPath += '/' + fileName
    else:
        dataPath += fileName

    ## load the data form csv
    data = read_single_csv(dataPath)
    ##assign the hex color values
    valueList = np.array(data)[:,2]
    valueList = np.log(valueList + 1)
    colourList = colourPicker(valueList)
    ## normalise the values for marker size
    valueList =  valueList/(valueList.max()-valueList.min())

    geojson_file = """
    {
        "type": "FeatureCollection",
        "features": ["""
    # iterating over features (rows in results array)
    for i in range(len(data)):
        # creating point feature 
        feature = """{
            "type": "Feature",
            "properties": {
            "marker-color": "%s",
            "marker-size": %s
            },
            "geometry": {
            "type": "Point",
            "coordinates": [
                %s,
                %s
            ]
            }
        },"""%(colourList[i], round(float(valueList[i]), 2), round(float(data[i][1]), 5), round(float(data[i][0]), 5))
        # adding new line 
        geojson_file += '\n'+feature

    # removing last comma as is last line
    geojson_file = geojson_file[:-1]
    # finishing file end 
    end_geojson = """
        ]
    }
    """
    geojson_file += end_geojson
    # saving as geoJSON
    geojson_written = open('/mnt/d/wx243/FromAW/populationData/UKPoplation_%s.geojson'%resolutionLevel,'w')
    geojson_written.write(geojson_file)
    geojson_written.close() 
    print('---GeoJSON written successfully---')
    return

def hexColourCodeFinder(data):
    # Define a color map
    cmap = colors.LinearSegmentedColormap.from_list('', ['red', 'green', 'blue'])
    # Assign hex color codes to values using the color map
    hex_codes = colors.to_hex(cmap(float(data)))
    return hex_codes

def visualisePointOnUKMap(dataPath):
    # Create a map of the UK
    map_uk = folium.Map(location=[54.7024, -3.2768], zoom_start=6)
    counter  = 0
    with open(dataPath, 'r') as f:
        reader = csv.reader(f)
        next(reader)
        for row in reader:
            latlon = [float(row[0]), float(row[1])]
            colour = hexColourCodeFinder(float(row[2]))
            folium.Marker(location=list(latlon), icon=folium.Icon(color=colour)).add_to(map_uk)
            counter += 1
            if counter > 100:
                break
        print('All points are added into the map.')
        # Save the map as an HTML file
        map_uk.save('/mnt/d/wx243/FromAW/populationData/uk_map.html')
        print('The map has been saved successfully.')
        
    return 

def visualisePoint(dataPath):
    import matplotlib.pyplot as plt
    import seaborn as sns
    import numpy as np
    datap = read_single_csv(dataPath)#[:1000]

    x = np.array(datap)[:,0]
    y = np.array(datap)[:,1]
    z = np.array(datap)[:,2]

    x = x.tolist()
    y = y.tolist()
    z = z.tolist()


    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.scatter(y, x, s = 0.1 , c=z, marker = 's', alpha=0.3, cmap='GnBu') #s=[float(v)/1000 for v in z],
    ax.set_xlabel('Latitude')
    ax.set_ylabel('Longitude')
    ax.set_title('UK population')
    ax.set_aspect(1.2648)

    #cbar = plt.colorbar()
    #cbar.set_label('Population/person')
    plt.savefig('/mnt/d/wx243/FromAW/populationData/populationUK.png',dpi = 2400, bbox_inches='tight')      
    return 
if __name__ == '__main__':
    dataPath = '/mnt/d/wx243/FromTWA/populationUK2019/'
    fileName = 'reduced_file_grid_size_1km.csv'
    # fileName = 'reduced_file_grid_size_10km.csv'
    ## fileName = 'reduced_file_grid_size_5km.csv'
    resolutionLevel = '1km'
    ## visualisePointOnUKMap(dataPath)
    ## visualisePoint(dataPath)
    ## pdGeo(dataPath, fileName, resolutionLevel)
    colourBarCreator(20663, 0, dataPath)







        