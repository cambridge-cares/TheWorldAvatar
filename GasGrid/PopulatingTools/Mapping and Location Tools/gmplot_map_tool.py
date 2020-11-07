import googlemaps
import numpy as np
import gmplot
import pandas as pd 
from location_search import loc_search

def map_creation():

    api_key ='AIzaSyAqfIR6zaWjhNXODG8vycZpA4olcoMD3w0'

    gmaps = googlemaps.Client(key=api_key)
    gmap = gmplot.GoogleMapPlotter(54.842595,-2.5051270,6)
    gmap.apikey = api_key

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
    c = ['seagreen','orange','gold','w','navy','hotpink','skyblue']
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

        gmap.scatter(class_location[:,0],class_location[:,1],s=2000,c=c[int(j/3)]\
            ,marker=True,title=titles[int(j/3)],alpha=1)

        overall_location = np.append(overall_location,class_location,axis=0)
    overall_location = overall_location[1:,:]



    ''' TEST CODE FOR HOW CONNECTIONS WILL BE VISUALISED
        WITH TEST AS EUCLIDIAN DISTANCE TO COMPARE WITH ACTUAL NTS'''
    node_num = len(overall_location)
    # creating distance matrix
    distances = np.zeros((node_num,node_num))
    for i in range(node_num):
        for j in range(node_num):
            diff = overall_location[i] - overall_location[j]
            distances[i,j] = np.linalg.norm(diff)
            # calculating distances
            if distances[i,j] == 0.0 or distances[i,j] == 0.0:
                distances[i,j] = 100 # if ontop then set high
    # plot lines between the 5 closest nodes that aren't 0 
    for i in range(node_num):
        for j in range(node_num):
            for k in range(5):
                if distances[i,j] == np.partition(distances[i],k)[k]:
                    gmap.plot([overall_location[i,0],overall_location[j,0]],[overall_location[i,1],overall_location[j,1]],alpha=0.5,ew=3)



    gmap.draw('PopulatingTools\Mapping and Location Tools\gmplot_map.html')
    return 

map_creation()