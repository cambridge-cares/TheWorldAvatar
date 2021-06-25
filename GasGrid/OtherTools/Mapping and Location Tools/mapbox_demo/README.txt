The general concept that I present
for plotting contoured data on a map is as follows:

1) Within the contour_test.py' file I produce a set of test data
    this consists of an x_grid, y_grid, and matrix of values (z_data) which 
    I could traditionally plot using the matplotlib contourf function.

    As opposed to plotting this, I convert this contour plot into a .geojson file using 
    the geojsoncontour Python library. 

2) This leaves me with a .geojson file containing the levels and colours that I need
    to plot the data (I upload this to my personal website for the time-being).
    Then I move over to Javascript and Mapbox where I can plot this geojson data. 

3) Using the Mapbox API and an account that I've made, I plot a simple UK map, 
    over which I overlay the .geojson file hosted on my personal website, and fill this 
    with the color and opacity specified in Python when I produced the file in the first place.

4) Mapbox notes: will have to use your own access token, with which you get 50,000
free 'loads' which is fairly substantial. Good alternatives to Mapbox 
    are 'Leaflet' which will allow to do exactly the same (just without 3D/buildings).

5) I've enabled 3D topography and buildings in this file so you can use right click + drag 
    to change the pitch, and if you zoom in you will be able to see buildings. 

    


