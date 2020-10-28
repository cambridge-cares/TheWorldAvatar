import googlemaps
import numpy as np
import gmplot
import pandas as pd 



gmaps = googlemaps.Client(key='AIzaSyAqfIR6zaWjhNXODG8vycZpA4olcoMD3w0')
gmap = gmplot.GoogleMapPlotter(54.842595,-2.5051270,6)
gmap.apikey = 'AIzaSyAqfIR6zaWjhNXODG8vycZpA4olcoMD3w0'



locations = pd.read_excel(r'C:\Users\trs53\Dropbox\Documents\Cambridge\PopulatingTools\location.xlsx')
locations = locations.to_numpy()[1:,1:]
intakes = locations[:,1:3]
compressors = locations[:,4:6]
offtakes = locations[:,7:]

for i in range(len(intakes)):
    intakes[i,0] = float(intakes[i,0])
    intakes[i,1] = float(intakes[i,1])
    if intakes[i,0] != intakes[i,0]:
        intakes = intakes[:i,:]
        break

for i in range(len(offtakes)):
    offtakes[i,0] = float(offtakes[i,0])
    offtakes[i,1] = float(offtakes[i,1])
    if offtakes[i,0] != offtakes[i,0]:
        offtakes = offtakes[:i,:]
        break

for i in range(len(compressors)):
    compressors[i,0] = float(compressors[i,0])
    compressors[i,1] = float(compressors[i,1])
    if compressors[i,0] != compressors[i,0]:
        compressors = compressors[:i,:]
        break


gmap.scatter(offtakes[:,0],offtakes[:,1],c='w',marker=False,s=200)
gmap.scatter(compressors[:,0],compressors[:,1],c='k',marker=False,s=200)
gmap.scatter(intakes[:,0],intakes[:,1],c='r',marker=False,s=200)

gmap.scatter(offtakes[:,0],offtakes[:,1],c='w')
gmap.scatter(compressors[:,0],compressors[:,1],c='k')
gmap.scatter(intakes[:,0],intakes[:,1],c='r')
gmap.draw('map.html')