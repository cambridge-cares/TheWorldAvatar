import folium 
from datetime import datetime 
import requests
import numpy as np 
import pandas as pd 
from location_search import loc_search

date = datetime.now()

attr='Contains OS data © Crown copyright and database right {}'.format(date.year)

m = folium.Map(location=[52.213730,0.105027],zoom_start=16)

try:
    locations = pd.read_excel(r'PopulatingTools\Mapping and Location Tools\location.xlsx')
except:
    print('LOCATION.XLSX FILE NOT FOUND, CREATING FROM LIST.CSV')
    loc_search()
    locations = pd.read_excel(r'PopulatingTools\Mapping and Location Tools\location.xlsx')

locations = locations.to_numpy()[:,1:]
titles = locations[0,:]
i = 0
while True:
    try:
        if titles[i] != titles[i]:
            titles = np.delete(titles,i)
            i -= 1
        i += 1
    except:
        break
locations = locations[1:,:]
c = ['red', 'darkgreen', 'lightblue', 'purple','orange','darkred','lightred',\
     'beige','darkblue', 'blue', 'cadetblue', 'darkpurple', 'black', \
         'pink', 'green', 'lightgreen', 'gray', 'white', 'lightgray']
location_index = np.arange(0,len(locations[0,:]),3)

overall_location = np.array([[0,0]])
for j in location_index:

    class_location = np.array([[0,0]])

    for i in range(len(locations[:,1+j])):
        loc = locations[i,j+1:j+3]
        if loc[0] != loc[0]:
            break
        class_location = np.append(class_location,[loc],axis=0)
    class_location = class_location[1:,:]
    for i in range(len(class_location)):   
        folium.Marker([class_location[i,0],class_location[i,1]],\
            popup=titles[int(j/3)],\
                icon=folium.Icon(color=c[j])).add_to(m)
    # gmap.scatter(class_location[:,0],class_location[:,1],s=2000,c=c[int(j/3)]\
    #     ,marker=True,title=titles[int(j/3)],alpha=1)

    overall_location = np.append(overall_location,class_location,axis=0)
overall_location = overall_location[1:,:]


NTS = 'PopulatingTools\Mapping and Location Tools\pipenetwork.geojson'
NTS_layer = folium.GeoJson(NTS).add_to(m)
folium.LayerControl().add_to(m)

start_end = pd.read_csv(r'PopulatingTools\Mapping and Location Tools\pipe_start_end.csv').to_numpy()[:,1:]

for i in range(len(start_end[:,0])):
    locations = np.array([start_end[i,:2],start_end[i,2:4]])
    locations = locations * 0.0001
    folium.PolyLine(locations).add_to(m)

m.save(r'PopulatingTools\Mapping and Location Tools\folium_map.html')