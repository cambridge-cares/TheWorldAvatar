import googlemaps
import numpy as np
import gmplot
import pandas as pd 


'''
TODO 
- ADD OFFTAKES 
- CONVERT PLOTTING TO WORK FROM LOCATION FILE
'''

gmaps = googlemaps.Client(key='AIzaSyAqfIR6zaWjhNXODG8vycZpA4olcoMD3w0')
gmap = gmplot.GoogleMapPlotter(54.842595,-2.5051270,6)
gmap.apikey = 'AIzaSyAqfIR6zaWjhNXODG8vycZpA4olcoMD3w0'

names = pd.read_csv(r'C:\Users\trs53\Dropbox\Documents\Cambridge\PopulatingTools\list.csv',usecols=[0,1,2])
names = names.to_numpy()
c=['r','k','b']
index = [0,3,6]
for j in index:
    if j != j:
        break
    lat_store = []
    lng_store = []
    k = 0 
    for i in names[:,j]:
        if i != i:
            break
        long_lat = gmaps.geocode(i+' ,UK')
        lat_store.append(long_lat[0]['geometry']['location']['lat'])
        lng_store.append(long_lat[0]['geometry']['location']['lng'])
        lng_lat_add = np.array([lat_store,lng_store]).T 
        added_len = 229 - (k+1)
        k += 1

 
    added_zeros = np.array([['',''] for i in range(added_len)])
    if added_len != 0:
        lng_lat_add = np.append(lng_lat_add,added_zeros,axis=0)

    names = np.insert(names,[j+1],lng_lat_add,axis=1)
    gmap.scatter(lat_store,lng_store,c=c[int(j/3)],size=10)


names = pd.DataFrame(names)
names.to_excel('location.xlsx')
gmap.draw('map.html')