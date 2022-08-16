```run_all.py``` will generate all figures for the parameters specified in the ```calculation_parameters``` folder. Including a coefficient of performance equation as well as % gas uptake and existing boiler efficiency. 

To draw the outline of the UK, a folder in this directory is required named 'GB_shapefile' with the respective shapefile therein. It can be found in Vienna within the code for preprint 280 (or near this, to do with heat pumps).

Each figure generation code also has the option to generate the figures using temporary python pickle files as opposed to queries. 
To change this switch the ```Testing``` variable to ```False```. Additionally the appropriate namespace will have to be provided. 