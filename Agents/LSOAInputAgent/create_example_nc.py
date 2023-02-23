import netCDF4 as nc
import numpy as np

# ----------- Function to create a simple nc file ----------------- #
# Create a new netCDF file
filename = 'test_tas_nc.nc'
with nc.Dataset(filename, 'w') as ds:
    # Define dimensions
    x_dim = ds.createDimension('longitude', 3)
    y_dim = ds.createDimension('latitude', 3)

    # Define variable
    var = ds.createVariable('tas', 'f8', ('longitude', 'latitude'))
    lon_var = ds.createVariable('longitude', 'f8', ('longitude',))
    lat_var = ds.createVariable('latitude', 'f8', ('latitude',))

    # Write data to variable
    data = np.random.randn(3, 3)
    var[:] = data

print(f'NetCDF file "{filename}" successfully created.')

def read_nc(fn, var_name,loc=True):
        '''
        Given a variable in the specified year of HadUK Grid dataset, reads the file
        and returns the grid of observations.
        '''
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

# Test by reading the nc files
lon, lat, var_grid = read_nc('./test_tas_nc.nc','tas',True)
var_grid = read_nc('./test_tas_nc.nc','tas',False)
a = 1