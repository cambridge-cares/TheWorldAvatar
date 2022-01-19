import numpy as np 
import matplotlib.pyplot as plt

# requires this geojsoncontourf package
import geojsoncontour

# creating bounds for x/y data
x_bounds = [-2,1]
y_bounds = [51,53]

# data points per axis
d_num = 100

# creating matrix of data to plot 
z_data = np.random.normal(0,1,(d_num,d_num))

# creating data mesh 
x_data = np.linspace(x_bounds[0],x_bounds[1],d_num)
y_data = np.linspace(y_bounds[0],y_bounds[1],d_num)
x_mesh,y_mesh = np.meshgrid(x_data,y_data)

# defining contour plot
levels = 20 
contourf = plt.contourf(x_mesh,y_mesh,z_data,levels,alpha=0.5)

geojson = geojsoncontour.contourf_to_geojson(
    contourf = contourf,
    fill_opacity = 0.5
)

# creating and saving file 
# I save this locally then upload online, maybe not reccomended.

geo_file = open("contour.geojson","w+")
geo_file.write(geojson)
geo_file.close() 



