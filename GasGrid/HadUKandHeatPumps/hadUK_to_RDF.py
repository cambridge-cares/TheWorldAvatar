import netCDF4 as nc
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, MultiPolygon, Point, LineString
from shapely.ops import polygonize
import uuid 
from scipy.spatial import Voronoi, voronoi_plot_2d
from pyproj import Proj

month_str = 'March'

fn = 'HadUK Files/tasmin_hadukgrid_uk_1km_mon_201901-201912.nc'
months = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
month = months[month_str]

ds = nc.Dataset(fn)
tasmin = ds.variables['tasmin'][:]
proj_x = ds.variables['projection_x_coordinate'][:]
proj_y = ds.variables['projection_y_coordinate'][:]
lon = ds.variables['longitude'][:]
lat = ds.variables['latitude'][:]
ds.close()

original_shape = np.shape(lon)
lon = lon.flatten()
lat = lat.flatten()
p = Proj(init='EPSG:27700')
proj_y,proj_x = p(lon,lat)

proj_x = np.reshape(proj_x,original_shape)
proj_y = np.reshape(proj_y,original_shape)


proj_y,proj_x = proj_x[:,0],proj_y[0,:]

x_len = len(proj_x)
y_len = len(proj_y)
dx = proj_x[1] - proj_x[0]
dy = proj_y[1] - proj_y[0]

fn = 'HadUK Files/tas_hadukgrid_uk_1km_mon_201901-201912.nc'
ds = nc.Dataset(fn)
tas = ds.variables['tas'][:]
ds.close()

fn = 'HadUK Files/tasmax_hadukgrid_uk_1km_mon_201901-201912.nc'
ds = nc.Dataset(fn)
tasmax = ds.variables['tasmax'][:]
ds.close()

# fig,ax = plt.subplots(1,3,figsize=(12,4))
# for i in range(3):
#     ax[i].set_xlabel('Longitude')
#     ax[i].set_ylabel('Latitude')

# plt.suptitle('HadUK 1km resolution dataset: '+month_str+' 2019')
# rain_plot = ax[0].imshow(tasmin[0,:,:],origin='lower')
# fig.colorbar(rain_plot,ax=ax[0])
# ax[0].title.set_text('Minimum air temperature (C)')
# temp_plot = ax[1].imshow(tas[0,:,:],origin='lower')
# fig.colorbar(temp_plot,ax=ax[1])
# ax[1].title.set_text('Mean air temperature (C)')
# sun_plot = ax[2].imshow(tasmax[0,:,:],origin='lower')
# fig.colorbar(sun_plot,ax=ax[2])
# ax[2].title.set_text('Maximum air temperature (C)')
# plt.show()

'''
FIRST GETTING MEASUREMENT COORDINATES 
AND CONVERTING EACH ONE FROM A POINT TO AN AREA.
'''
overall_centroids = []
overall_polygons = []
overall_tasmin = []
overall_tas = []
overall_tasmax = []

for i in tqdm(range(len(proj_x))):
    for j in range(len(proj_y)):
        tasmin_val = float(tasmin[month,j,i])
        tas_val = float(tas[month,j,i])
        tasmax_val = float(tasmax[month,j,i])
        if tas_val > -1000:
            point = [proj_x[i],proj_y[j]]
            point = p(point[1],point[0],inverse=True)
            centroid = Point(point).wkt
            x1 = proj_x[i] - (dx/2)
            x2 = proj_x[i] + (dx/2)
            y1 = proj_y[j] - (dy/2)
            y2 = proj_y[j] + (dy/2)
            polygon_list = [[x1,y1],[x1,y2],[x2,y2],[x2,y1],[x1,y1]]
            polygon_list = [p(i[1],i[0],inverse=True) for i in polygon_list]
            polygon = Polygon(polygon_list).wkt
            overall_centroids.append(centroid)
            overall_polygons.append(polygon)
            overall_tasmin.append(tasmin_val)
            overall_tas.append(tas_val)
            overall_tasmax.append(tasmax_val)


def COP_heating(Th,Tc,n):
    return n*(273.15+Th)/(Th-Tc)

t_c = np.linspace(-10,20,100)
COP_store = COP_heating(35.0,t_c,0.5)

# plt.figure(figsize=(10,4))
# plt.title('Heat pump coefficient of performance for an assumed $\eta$ of 0.5\
#      \n and heating temperature of 35C.')
# plt.grid()
# plt.xlabel('Outside Air Temperature (C)')
# plt.ylabel('COP')
# plt.plot(t_c,COP_store,linewidth=2.5,c='k')
# plt.show()

COP_tas = COP_heating(35.0,np.array(overall_tas),0.5)
COP_tasmin = COP_heating(35.0,np.array(overall_tasmin),0.5)
COP_tasmax = COP_heating(35.0,np.array(overall_tasmax),0.5)

# plt.figure(figsize=(10,4))
# plt.grid()
# plt.title('Histogram of COP frequency across the UK in '+month_str+' 2019')
# plt.hist(COP_tas,bins=200,color='k',alpha=0.3,label='Mean Air Temp')
# plt.hist(COP_tasmin,bins=200,color='tab:blue',alpha=0.3,label='Min Air Temp')
# plt.hist(COP_tasmax,bins=200,color='tab:orange',alpha=0.3,label='Max Air Temp')
# plt.legend()
# plt.ylabel('frequency')
# plt.xlabel('COP')
# plt.show()

