
def geoJSON_decommissioned(DecommissionedNuclearSitesLocation, fileName):
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for de_key in DecommissionedNuclearSitesLocation.keys():
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "marker-color": "",
              "marker-size": "medium",
              "name": "%s",
              "status": "DecommissionedNuclearSitesLocation"
            },
            "geometry": {
              "type": "Point",
              "coordinates": [
                %s,
                %s
              ]
            }
          },"""%(de_key, DecommissionedNuclearSitesLocation[de_key][1], DecommissionedNuclearSitesLocation[de_key][0])
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
    geojson_written = open('/mnt/d/wx243/FromAW/CandidateSitsForSMR/%s.geojson'%fileName,'w')
    geojson_written.write(geojson_file)
    geojson_written.close() 
    return
if __name__ == '__main__': 
    DecommissionedNuclearSitesLocation = {'Wylfa': [53.4159603, -4.4902244], 'Oldbury': [51.6473724, -2.5721401], 
                                          'Sellafield':[54.4205, -3.4975], 'Trawsfynydd': [52.925567,-3.9507508],
                                          'Dungeness_B': [50.9138436, 0.9596944], 'Hunterston_B':[55.7214775,-4.8969607]}
    de_coal = {'Aberthaw_B': [51.38731, -3.4049], "Cottam":[53.304, -0.7815], "Uskmouth": [51.54907, -2.97053], "Fiddlers": [53.37234, -2.68912]}
    de_oil = {"Five_Oaks_1":[51.04471, -0.4439]}
    de_gas = {"Castleford": [53.7383, -1.39844], "Sandbach": [53.16542, -2.40665], "Thornhill": [53.67562, -1.6596]}
    
    
    # fileName = "decommissionedNuclear"
    fileName = "de_gas"
    geoJSON_decommissioned(de_gas, fileName)
    print('geojson is created successfully')