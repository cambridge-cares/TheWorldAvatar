import netCDF4 as nc
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, MultiPolygon, Point, LineString
from shapely.ops import polygonize
from shapely import wkt, geometry, ops
import imageio
import rdflib
import time
from rdflib.namespace import DC, DCTERMS, DOAP, FOAF, SKOS, OWL, RDF, RDFS, VOID, XMLNS, XSD
import os

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
    fn = 'HadUK Files/'+var_name+'_hadukgrid_uk_1km_mon_201901-201912.nc'
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


def gridded_data_to_array(lon,lat,nc_vars,month):
    overall_centroids = []
    overall_polygons = []
    var_store = np.array([[0 for i in range(len(nc_vars))]])
    for i in range(len(nc_vars)):
        nc_vars[i] = nc_vars[i].filled(np.nan)
    for i in tqdm(range(len(lat[:,0]))):
        for j in range(len(lon[0,:])):
            nc_var_temp = [0 for i in range(len(nc_vars))]
            for k in range(len(nc_vars)):
                nc_var_temp[k] = nc_vars[k][month,i,j]
            if nc_var_temp[0] > -1e8:
                point = [lat[i,j],lon[i,j]]
                centroid = point
                if i == len(lat[:,0]):
                    y1 = lat[i,j] - ((lat[i,j]-lat[i-1,j])/2)
                    y2 = lat[i,j] + ((lat[i,j]-lat[i-1,j])/2)
                    x1 = lon[i,j] - ((lon[i,j]-lon[i,j-1])/2)
                    x2 = lon[i,j] + ((lon[i,j+1]-lon[i,j])/2)
                if j == len(lat[0,:]):
                    y1 = lat[i,j] - ((lat[i,j]-lat[i-1,j])/2)
                    y2 = lat[i,j] + ((lat[i+1,j]-lat[i,j])/2)
                    x1 = lon[i,j] - ((lon[i,j]-lon[i,j-1])/2)
                    x2 = lon[i,j] + ((lon[i,j]-lon[i,j-1])/2)
                else:
                    y1 = lat[i,j] - ((lat[i,j]-lat[i-1,j])/2)
                    y2 = lat[i,j] + ((lat[i+1,j]-lat[i,j])/2)
                    x1 = lon[i,j] - ((lon[i,j]-lon[i,j-1])/2)
                    x2 = lon[i,j] + ((lon[i,j+1]-lon[i,j])/2)

                polygon_list = [[x1,y1],[x1,y2],[x2,y2],[x2,y1],[x1,y1]]
                polygon = Polygon(polygon_list).wkt
                overall_centroids.append(centroid)
                overall_polygons.append(polygon)
                var_store = np.append(var_store,[nc_var_temp],axis=0)
    var_store = var_store[1:,:]
    return overall_centroids, overall_polygons, var_store

def COP_heating(Th,Tc,n):
    return n*(273.15+Th)/(Th-Tc) 


def construct_graph(month_str):
    month,month_end = month_num(month_str)
    lon,lat,tasmin = read_nc('tasmin',loc=True) 
    tasmax = read_nc('tasmax',loc=False) 
    tas = read_nc('tas',loc=False) 
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
    plt.savefig('OutputImages/'+month_str+'_TemperaturePlot.png')

    points,polygons,nc_vars = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month)
    overall_tasmin,overall_tas,overall_tasmax = nc_vars[:,0],nc_vars[:,1],nc_vars[:,2]


    t_c = np.linspace(-10,20,100)
    COP_store = COP_heating(35.0,t_c,0.5)

    plt.figure(figsize=(10,4))
    plt.title('Heat pump coefficient of performance for an assumed $\eta$ of 0.5\
         \n and heating temperature of 35C.')
    plt.grid()
    plt.xlabel('Outside Air Temperature (C)')
    plt.ylabel('COP')
    plt.plot(t_c,COP_store,linewidth=2.5,c='k')
    plt.savefig('OutputImages/HeatPumpCOP.png')

    COP_tas = COP_heating(35.0,np.array(overall_tas),0.5)
    COP_tasmin = COP_heating(35.0,np.array(overall_tasmin),0.5)
    COP_tasmax = COP_heating(35.0,np.array(overall_tasmax),0.5)

    plt.figure(figsize=(10,4))
    plt.grid()
    plt.title('Histogram of COP frequency across the UK in '+month_str+' 2019')
    plt.hist(COP_tas,bins=200,color='k',alpha=0.3,label='Mean Air Temp')
    plt.hist(COP_tasmin,bins=200,color='tab:blue',alpha=0.3,label='Min Air Temp')
    plt.hist(COP_tasmax,bins=200,color='tab:orange',alpha=0.3,label='Max Air Temp')
    plt.legend()
    plt.ylabel('frequency')
    plt.xlabel('COP')
    plt.savefig('OutputImages/'+month_str+'_COPHistogram.png')


    g = rdflib.Graph()
    hada = rdflib.Namespace('http://www.theworldavatar.com/kb/ontohadukgrid/hadgridabox/')
    had  = rdflib.Namespace('http://www.theworldavatar.com/ontology/ontohadukgrid/ontohadukgrid.owl#') 
    geo = rdflib.Namespace('http://www.w3.org/2003/01/geo/wgs84_pos#')
    om = rdflib.Namespace('http://www.ontology-of-units-of-measure.org/resource/om-2/')
    geos = rdflib.Namespace('http://www.opengis.net/ont/geosparql')

    g.add((hada['HadUK1kmGrid'],RDF.type,had['ClimateGrid']))
    g.add((hada['tas'],RDF.type,had['ClimateVariable']))
    g.add((hada['tasmin'],RDF.type,had['ClimateVariable']))
    g.add((hada['tasmax'],RDF.type,had['ClimateVariable']))

    print('Creating Graph')
    for i in tqdm(range(len(points))):
        g.add((hada['grid_point'+str(i)],RDF.type,had['ClimateGridPoint']))
        g.add((hada['point_boundary'+str(i)],RDF.type,had['ClimateGridPointBoundary']))
        g.add((hada['HadUK1kmGrid'],had['hasGridPoint'],hada['grid_point'+str(i)]))
        g.add((hada['grid_point'+str(i)],had['hasPointBoundary'],hada['point_boundary'+str(i)]))
        g.add((hada['point_boundary'+str(i)],geos.asWKT,rdflib.Literal(polygons[i])))
        g.add((hada['grid_point'+str(i)],had['hasLongitude'],rdflib.Literal(str(points[i][1]),datatype=geo.lon)))
        g.add((hada['grid_point'+str(i)],had['hasLatitude'],rdflib.Literal(str(points[i][0]),datatype=geo.lat)))


        g.add((hada['tas_point'+str(i)+'obs_'+month_str],RDF.type,had['ClimateGridObservation']))
        g.add((hada['tas_point'+str(i)+'obs_'+month_str],had['hasVariable'],hada['tas']))
        g.add((hada['tas_point'+str(i)+'obs_'+month_str],had['hasPeriodStart'],rdflib.Literal('2019-'+str(month)+'-01T00:00:00-00:00')))
        g.add((hada['tas_point'+str(i)+'obs_'+month_str],had['hasPeriodStart'],rdflib.Literal('2019-'+str(month)+'-'+str(month_end)+'T00:00:00-00:00')))
        g.add((hada['tas_point'+str(i)+'obs_measurement_'+month_str],RDF.type,om['Measure']))
        g.add((hada['tas_point'+str(i)+'obs_measurement_'+month_str],om['hasNumericalValue'],rdflib.Literal(overall_tas[i])))
        g.add((hada['tas_point'+str(i)+'obs_measurement_'+month_str],om['hasUnit'],om['degreeCelsius']))
        g.add((hada['tas_point'+str(i)+'obs_quan_'+month_str],RDF.type,om['Temperature']))
        g.add((hada['tas_point'+str(i)+'obs_quan_'+month_str],om['hasPhenomenon'],hada['tas_point'+str(i)+'obs']))
        g.add((hada['tas_point'+str(i)+'obs_quan_'+month_str],om['hasValue'],hada['tas_point'+str(i)+'obs_measurement_'+month_str]))
        g.add((hada['grid_point'+str(i)],had['hasObservation'],hada['tas_point'+str(i)+'obs']))

        g.add((hada['tasmin_point'+str(i)+'obs_'+month_str],RDF.type,had['ClimateGridObservation']))
        g.add((hada['tasmin_point'+str(i)+'obs_'+month_str],had['hasVariable'],hada['tasmin']))
        g.add((hada['tasmin_point'+str(i)+'obs_'+month_str],had['hasPeriodStart'],rdflib.Literal('2019-'+str(month)+'-01T00:00:00-00:00')))
        g.add((hada['tasmin_point'+str(i)+'obs_'+month_str],had['hasPeriodStart'],rdflib.Literal('2019-'+str(month)+'-'+str(month_end)+'T00:00:00-00:00')))
        g.add((hada['tasmin_point'+str(i)+'obs_measurement_'+month_str],RDF.type,om['Measure']))
        g.add((hada['tasmin_point'+str(i)+'obs_measurement_'+month_str],om['hasNumericalValue'],rdflib.Literal(overall_tasmin[i])))
        g.add((hada['tasmin_point'+str(i)+'obs_measurement_'+month_str],om['hasUnit'],om['degreeCelsius']))
        g.add((hada['tasmin_point'+str(i)+'obs_quan_'+month_str],RDF.type,om['Temperature']))
        g.add((hada['tasmin_point'+str(i)+'obs_quan_'+month_str],om['hasPhenomenon'],hada['tasmin_point'+str(i)+'obs']))
        g.add((hada['tasmin_point'+str(i)+'obs_quan_'+month_str],om['hasValue'],hada['tasmin_point'+str(i)+'obs_measurement_'+month_str]))
        g.add((hada['grid_point'+str(i)],had['hasObservation'],hada['tasmin_point'+str(i)+'obs']))

        g.add((hada['tasmax_point'+str(i)+'obs_'+month_str],RDF.type,had['ClimateGridObservation']))
        g.add((hada['tasmax_point'+str(i)+'obs_'+month_str],had['hasVariable'],hada['tasmax']))
        g.add((hada['tasmax_point'+str(i)+'obs_'+month_str],had['hasPeriodStart'],rdflib.Literal('2019-'+str(month)+'-01T00:00:00-00:00')))
        g.add((hada['tasmax_point'+str(i)+'obs_'+month_str],had['hasPeriodStart'],rdflib.Literal('2019-'+str(month)+'-'+str(month_end)+'T00:00:00-00:00')))
        g.add((hada['tasmax_point'+str(i)+'obs_measurement_'+month_str],RDF.type,om['Measure']))
        g.add((hada['tasmax_point'+str(i)+'obs_measurement_'+month_str],om['hasNumericalValue'],rdflib.Literal(overall_tasmax[i])))
        g.add((hada['tasmax_point'+str(i)+'obs_measurement_'+month_str],om['hasUnit'],om['degreeCelsius']))
        g.add((hada['tasmax_point'+str(i)+'obs_quan_'+month_str],RDF.type,om['Temperature']))
        g.add((hada['tasmax_point'+str(i)+'obs_quan_'+month_str],om['hasPhenomenon'],hada['tasmax_point'+str(i)+'obs']))
        g.add((hada['tasmax_point'+str(i)+'obs_quan_'+month_str],om['hasValue'],hada['tasmax_point'+str(i)+'obs_measurement_'+month_str]))
        g.add((hada['grid_point'+str(i)],had['hasObservation'],hada['tasmax_point'+str(i)+'obs']))

    g.serialize(destination=month_str+'_2019_hadukabox.nt',format='nt')

    return


def just_plot():
    months = ['January','February','March','April','May','June','July','August','September','November','December']
    images=[]
    for month_str in months:
        month,month_end = month_num(month_str)
        lon,lat,tasmin = read_nc('tasmin',loc=True) 
        tasmax = read_nc('tasmax',loc=False) 
        tas = read_nc('tas',loc=False) 
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
        plt.savefig('OutputImages/'+month_str+'_TemperaturePlot.png')

        points,polygons,nc_vars = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month)
        overall_tasmin,overall_tas,overall_tasmax = nc_vars[:,0],nc_vars[:,1],nc_vars[:,2]

        COP_tas = COP_heating(35.0,np.array(overall_tas),0.5)
        COP_tasmin = COP_heating(35.0,np.array(overall_tasmin),0.5)
        COP_tasmax = COP_heating(35.0,np.array(overall_tasmax),0.5)

        plt.figure(figsize=(10,4))
        plt.grid()
        plt.title('Histogram of COP frequency across the UK in '+month_str+' 2019')
        plt.hist(COP_tas,bins=200,color='k',alpha=0.3,label='Mean Air Temp')
        plt.hist(COP_tasmin,bins=200,color='tab:blue',alpha=0.3,label='Min Air Temp')
        plt.hist(COP_tasmax,bins=200,color='tab:orange',alpha=0.3,label='Max Air Temp')
        plt.xlim(3.5,16)
        plt.ylim(0,8000)
        plt.legend()
        plt.ylabel('frequency')
        plt.xlabel('COP')
        plt.savefig('OutputImages/'+month_str+'_COPHistogram.png')
        images.append(imageio.imread('OutputImages/'+month_str+'_COPHistogram.png'))
        os.remove('OutputImages/'+month_str+'_COPHistogram.png')

    imageio.mimsave('OutputImages/SeasonalCOP.gif', images)
    return 

months = ['January','February','March','April','May','June','July','August','September','October','November','December']
for month in months:
    construct_graph(month)