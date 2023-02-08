import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.OWLfileStorer import readFile

data = readFile("C:\\Users\\wx243\\Documents\\TheWorldAvatar\\UK_Digital_Twin\\Data files\\PopulationDensity\\2022\\population_gbr.csv")
del data[0]

for d in data:
    d[2] = float(d[2].strip("\n"))

def pdGeo(data, class_label): 
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
                "Population": %s,
                "marker-color": "#ceb69c",
                "marker-size": "small",
                "marker-symbol": "marker"
                },
                "geometry": {
                "type": "Point",
                "coordinates": [
                    %s,
                    %s
                ]
                }
            },"""%(data[i][2], float(data[i][1]), float(data[i][0]))
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
        geojson_written = open(class_label+'.geojson','w')
        geojson_written.write(geojson_file)
        geojson_written.close() 
        print('---GeoJSON written successfully---')
        return

pdGeo(data, 'populationdensity')