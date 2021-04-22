import netCDF4 as nc
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, MultiPolygon
import uuid 

month_str = 'March'

fn = 'rainfall_hadukgrid_uk_1km_mon_201901-201912.nc'
months = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
month = months[month_str]

ds = nc.Dataset(fn)
lon = ds.variables['longitude'][:][month]
lat = ds.variables['latitude'][:][month]
rain = ds.variables['rainfall'][:]
ds.close()

fn = 'sun_hadukgrid_uk_1km_mon_201901-201912.nc'
ds = nc.Dataset(fn)
sun = ds.variables['sun'][:]
ds.close()

fn = 'tas_hadukgrid_uk_1km_mon_201901-201912.nc'
ds = nc.Dataset(fn)
tas = ds.variables['tas'][:]
ds.close()

fig,ax = plt.subplots(1,3,figsize=(12,4))
for i in range(3):
    ax[i].set_xlabel('Longitude')
    ax[i].set_ylabel('Latitude')

plt.suptitle('HadUK 1km resolution dataset: '+month_str+' 2019')
rain_plot = ax[0].imshow(rain[0,:,:],origin='lower')
fig.colorbar(rain_plot,ax=ax[0])
ax[0].title.set_text('Total precipitation amount (mm)')
temp_plot = ax[1].imshow(tas[0,:,:],origin='lower')
fig.colorbar(temp_plot,ax=ax[1])
ax[1].title.set_text('Mean air temperature (C)')
sun_plot = ax[2].imshow(sun[0,:,:],origin='lower')
fig.colorbar(sun_plot,ax=ax[2])
ax[2].title.set_text('Sunshine hours (hrs)')
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
overall_rain = []
overall_sun = []
overall_temp = []
for i in tqdm(range(len(lat_array)-1)):
    polygon_list = []
    rain_list = []
    temp_list = []
    sun_list = []
    for j in range(len(lon_array)-1):
        rain_val = float(rain[month,j,i])
        temp_val = float(tas[month,j,i])
        sun_val = float(sun[month,j,i])
        if rain_val > 0.000001:
            lon_1 = lon_array[i]
            lon_2 = lon_array[i+1]
            lat_1 = lat_array[j]
            lat_2 = lat_array[j+1]
            area = Polygon([[lon_1,lat_1],[lon_1,lat_2],[lon_2,lat_2],[lon_2,lat_1],[lon_1,lat_1]])
            polygon_list = np.append(polygon_list,area)
            rain_list.append(rain_val)
            temp_list.append(temp_val)
            sun_list.append(sun_val)
    overall_polygons = np.append(overall_polygons,polygon_list)
    overall_rain = np.append(overall_rain,rain_list)
    overall_temp = np.append(overall_temp,temp_list)
    overall_sun = np.append(overall_sun,sun_list)

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

COP_areas = COP_heating(35,overall_temp,0.5)

plt.figure(figsize=(10,4))
plt.grid()
plt.title('Histogram of COP frequency across the UK in '+month_str+' 2019')
plt.hist(COP_areas,bins=200,color='k')
plt.ylabel('frequency')
plt.xlabel('COP')
plt.show()

