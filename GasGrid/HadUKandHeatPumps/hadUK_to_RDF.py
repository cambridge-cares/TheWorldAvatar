import netCDF4 as nc
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, MultiPolygon
import uuid 

month_str = 'March'

fn = 'HadUK Files/tasmin_hadukgrid_uk_1km_mon_201901-201912.nc'
months = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
month = months[month_str]

ds = nc.Dataset(fn)
lon = ds.variables['longitude'][:][month]
lat = ds.variables['latitude'][:][month]
tasmin = ds.variables['tasmin'][:]
ds.close()

fn = 'HadUK Files/tas_hadukgrid_uk_1km_mon_201901-201912.nc'
ds = nc.Dataset(fn)
tas = ds.variables['tas'][:]
ds.close()

fn = 'HadUK Files/tasmax_hadukgrid_uk_1km_mon_201901-201912.nc'
ds = nc.Dataset(fn)
tasmax = ds.variables['tasmax'][:]
ds.close()

fig,ax = plt.subplots(1,3,figsize=(12,4))
for i in range(3):
    ax[i].set_xlabel('Longitude')
    ax[i].set_ylabel('Latitude')

plt.suptitle('HadUK 1km resolution dataset: '+month_str+' 2019')
rain_plot = ax[0].imshow(tasmin[0,:,:],origin='lower')
fig.colorbar(rain_plot,ax=ax[0])
ax[0].title.set_text('Minimum air temperature (C)')
temp_plot = ax[1].imshow(tas[0,:,:],origin='lower')
fig.colorbar(temp_plot,ax=ax[1])
ax[1].title.set_text('Mean air temperature (C)')
sun_plot = ax[2].imshow(tasmax[0,:,:],origin='lower')
fig.colorbar(sun_plot,ax=ax[2])
ax[2].title.set_text('Maximum air temperature (C)')
plt.show()

d_lon = lon[1] - lon[0]
d_lat = lat[1] - lat[0]

lon_array = [lon[0] - d_lon]
lat_array = [lat[0] - d_lat]
for i in range(len(lat)):
    lon_array.append(lon_array[i]+d_lon)
for i in range(len(lon)):
    lat_array.append(lon_array[i]+d_lon)

overall_polygons = []
overall_tasmin = []
overall_tas = []
overall_tasmax = []
for i in tqdm(range(len(lat_array)-1)):
    polygon_list = []
    tas_list = []
    tasmin_list = []
    tasmax_list = []
    for j in range(len(lon_array)-1):
        tasmin_val = float(tasmin[month,j,i])
        tas_val = float(tas[month,j,i])
        tasmax_val = float(tasmax[month,j,i])
        if tas_val > -1000:
            lon_1 = lon_array[i]
            lon_2 = lon_array[i+1]
            lat_1 = lat_array[j]
            lat_2 = lat_array[j+1]
            area = Polygon([[lon_1,lat_1],[lon_1,lat_2],[lon_2,lat_2],[lon_2,lat_1],[lon_1,lat_1]])
            polygon_list = np.append(polygon_list,area)
            tasmin_list.append(tasmin_val)
            tas_list.append(tas_val)
            tasmax_list.append(tasmax_val)
    overall_polygons = np.append(overall_polygons,polygon_list)
    overall_tasmin = np.append(overall_tasmin,tasmin_list)
    overall_tas = np.append(overall_tas,tas_list)
    overall_tasmax = np.append(overall_tasmax,tasmax_list)

def COP_heating(Th,Tc,n):
    return n*(273.15+Th)/(Th-Tc)

t_c = np.linspace(-10,20,100)
COP_store = COP_heating(35,t_c,0.5)

plt.figure(figsize=(10,4))
plt.title('Heat pump coefficient of performance for an assumed $\eta$ of 0.5\
     \n and heating temperature of 35C.')
plt.grid()
plt.xlabel('Outside Air Temperature (C)')
plt.ylabel('COP')
plt.plot(t_c,COP_store,linewidth=2.5,c='k')
plt.show()

COP_tas = COP_heating(35,overall_tas,0.5)
COP_tasmin = COP_heating(35,overall_tasmin,0.5)
COP_tasmax = COP_heating(35,overall_tasmax,0.5)

plt.figure(figsize=(10,4))
plt.grid()
plt.title('Histogram of COP frequency across the UK in '+month_str+' 2019')
plt.hist(COP_tas,bins=200,color='k',alpha=0.3,label='Mean Air Temp')
plt.hist(COP_tasmin,bins=200,color='tab:blue',alpha=0.3,label='Min Air Temp')
plt.hist(COP_tasmax,bins=200,color='tab:orange',alpha=0.3,label='Max Air Temp')
plt.legend()
plt.ylabel('frequency')
plt.xlabel('COP')
plt.show()

