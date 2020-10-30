import googlemaps
import numpy as np
import gmplot
from tqdm import tqdm
import pandas as pd 


def loc_search():
    gmaps = googlemaps.Client(key='AIzaSyAqfIR6zaWjhNXODG8vycZpA4olcoMD3w0')
    try:
        names = pd.read_csv(r'PopulatingTools\Mapping and Location Tools\list.csv',header=None)
    except:
        print('LIST.CSV NOT FOUND, PLEASE CREATE WITH COLUMNS OF PLACENAMES OF A PARTICULAR CLASS')
        return
    names = names.to_numpy()
    max_names = len(names[:,0])-2
    index = np.arange(0,len(names[1,:])*3,3)
    for j in index:
        if j != j:
            break
        lat_store = ['']
        lng_store = ['']
        k = 0 
        print('SEARCHING FOR NAMES OF CLASS: '+str((int(j/3))+1))
        for i in tqdm(names[1:,j]):
            if i != i:
                break
            long_lat = gmaps.geocode(i+' ,UK')
            lat_store.append(long_lat[0]['geometry']['location']['lat'])
            lng_store.append(long_lat[0]['geometry']['location']['lng'])
            added_len = max_names+1 - (k+1)
            k += 1

        lng_lat_add = np.array([lat_store,lng_store]).T 
        added_zeros = np.array([['',''] for i in range(added_len)])
        if added_len > 1:
            lng_lat_add = np.append(lng_lat_add,added_zeros,axis=0)
        if j == index[-1]:
            names = np.concatenate((names,lng_lat_add),axis=1)
        else:
            names = np.insert(names,[j+1],lng_lat_add,axis=1)


    names = pd.DataFrame(names)
    names.to_excel(r'PopulatingTools\Mapping and Location Tools\location.xlsx')
    return 
