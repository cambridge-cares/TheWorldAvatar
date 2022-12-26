from agent.datamodel.spec import *
import numpy as np
import netCDF4 as nc
from tqdm import tqdm
from shapely.geometry import Point, MultiPoint
from shapely.ops import nearest_points
from shapely import wkt
import shapely.speedups
shapely.speedups.enable()
from agent.datamodel.iris import *
import cProfile
import pandas

def read_all_temperature(limit = False):
        
    def month_num(month_str):
        '''
        Converts a string of month name to that month's number 
        Starting at 0
        '''
        months = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
        month_ends = {'January':31,'February':28,'March':31,'April':30,'May':31,'June':30,'July':30,'August':31,'September':30,'October':29,'November':30,'December':31}
        month = months[month_str]
        month_end = month_ends[month_str]
        return month, month_end

    def read_nc(var_name,loc=True):
        '''
        Given a variable in the HadUK Grid dataset, reads the file
        and returns the grid of observations.
        '''
        fn = './Data/Climate Files/'+var_name+'_hadukgrid_uk_1km_mon_202001-202012.nc'
        ds = nc.Dataset(fn)
        var_grid = ds.variables[var_name][:]
        if loc == True:
            lon = ds.variables['longitude'][:]
            lat = ds.variables['latitude'][:]
            ds.close()
            return lon, lat, var_grid
        else:
            ds.close()
            return var_grid

    def gridded_data_to_array(lon,lat,nc_vars,month,centroids=True):
        if centroids == True:
            overall_centroids = []
        var_store = np.array([[0 for i in range(len(nc_vars))]])
        for i in range(len(nc_vars)):
            nc_vars[i] = nc_vars[i].filled(np.nan)
        for i in tqdm(range(len(lat[:,0]))):
            for j in range(len(lon[0,:])):
                nc_var_temp = [0 for i in range(len(nc_vars))]
                for k in range(len(nc_vars)):
                    nc_var_temp[k] = nc_vars[k][month,i,j]
                if nc_var_temp[0] > -1e8:
                    if centroids == True:
                        point = Point([lon[i,j],lat[i,j]])
                        centroid = point
                        overall_centroids.append(centroid)
                    var_store = np.append(var_store,[nc_var_temp],axis=0)
        var_store = var_store[1:,:]
        if centroids == True:
            return overall_centroids, var_store
        else:
            return var_store

    def get_treated_shape():
        # Get geodata
        usage_vals = call_pickle('./Data/pickle_files/shapes_array')
        # preassigning centroid array
        centroids = np.zeros((len(usage_vals),1),dtype='object')
        i = 0 
        del_index = [] 
        # iterating over WKT Literals
        for area in usage_vals: 
            # loading WKT
            try:
                wkt_repr = wkt.loads(area[-1])
                usage_vals[i,-1] = wkt_repr
                # calculating and storing centroid
                centroids[i,:] = [wkt_repr.centroid]
                i += 1 
            except TypeError: 
                del_index.append(i)
                usage_vals[i,-1] = 'None'
                centroids[i,:] = ['None']
                i += 1 
        # concatenating results 
        LSOA = np.concatenate((usage_vals,centroids),axis=1)
        LSOA = np.delete(LSOA,del_index,axis=0)

        return LSOA
    
    df = call_pickle('./Data/pickle_files/df_all_results')
    already_there_LSOA = df['LSOA_code'].tolist()

    LSOA = get_treated_shape()
    lon,lat,tas = read_nc('tas',loc=True)
    tasmin = read_nc('tasmin',loc=False)
    tasmax = read_nc('tasmax',loc=False)
    months = ['January','February','March','April','May','June','July','August','September','October','November','December']
    clim_vars = ['tasmin','tas','tasmax']

    nc_vars_full = [] 
    for i in range(len(months)):
        month_str = months[i]
        month,month_end = month_num(month_str)
        if i == 0:
            grid_loc,nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month,centroids=True)
        if i == 8:
            nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month,centroids=False)
            for j in range(6):
                nc_vars_add=np.append(nc_vars_add,[[12,14,16]],axis=0)
                j = j +1
        else:
            nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month,centroids=False)
        nc_vars_full.append(nc_vars_add)

    full_grid = MultiPoint(points=list(grid_loc))

    print('Computing associated points...')

    temp_result_dict = {}
    for i in tqdm(range(int(len(LSOA)))):
        if LSOA[i,0] not in already_there_LSOA:
            assoc_mask = [full_grid.geoms[j].within(LSOA[i,1]) for j in range(len(grid_loc))]
            if any(assoc_mask) == False:
                assoc_point = nearest_points(full_grid,LSOA[i,2])[0]
                for j in range(len(grid_loc)):
                    if assoc_point == grid_loc[j]:
                        assoc_mask[j] = True 
                        break

            for month_it in range(len(months)):
                LSOA_vars_full = nc_vars_full[month_it][assoc_mask,:]

                post_proc_vars = np.zeros(3)
                for k in range(3):

                    if clim_vars[k] == 'tasmin':
                        post_proc_vars[k] = np.min(LSOA_vars_full[:,k])
                    elif clim_vars[k] == 'tas': 
                        post_proc_vars[k] = np.mean(LSOA_vars_full[:,k])
                    else:
                        post_proc_vars[k] = np.max(LSOA_vars_full[:,k])

                LSOA_vars = post_proc_vars
                LSOA_IRI = LSOA[i,0]
                month_str = str(month_it+1)
                month_str_len = len(month_str)
                if month_str_len == 1:
                    month_str = '0'+month_str

                startUTC = '2020-'+month_str+'-01T12:00:00'
                for var in range(len(LSOA_vars)):
                    clim_var = clim_vars[var]
                    temp_result_dict.setdefault(LSOA_IRI, {}).setdefault(startUTC, {})[clim_var] = round(LSOA_vars[var], 3)
                    parse_to_file(temp_result_dict)
                    save_pickle_variable(temp_result_dict)

    return temp_result_dict

cProfile.runctx('read_all_temperature(limit = False)', globals(), locals(), '-')