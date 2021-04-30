import netCDF4 as nc
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, MultiPolygon, Point, LineString, MultiPoint
from shapely.ops import polygonize, nearest_points
from shapely import wkt, geometry, ops
import shapely.speedups
shapely.speedups.enable()
import imageio
import rdflib
import uuid 
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
    var_store = np.array([[0 for i in range(len(nc_vars))]])
    for i in range(len(nc_vars)):
        nc_vars[i] = nc_vars[i].filled(np.nan)
    for i in tqdm(range(len(lat[:,0]))):
        for j in range(len(lon[0,:])):
            nc_var_temp = [0 for i in range(len(nc_vars))]
            for k in range(len(nc_vars)):
                nc_var_temp[k] = nc_vars[k][month,i,j]
            if nc_var_temp[0] > -1e8:
                point = Point([lon[i,j],lat[i,j]])
                centroid = point
                overall_centroids.append(centroid)
                var_store = np.append(var_store,[nc_var_temp],axis=0)
    var_store = var_store[1:,:]
    return overall_centroids, var_store


def LSOA_polygon_query(limit):
    '''
    Querying the KG for all regions gas-usages in 2019 
    '''
    if limit == False:
        limit = str(100000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>

    SELECT ?s ?geom
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography.
    OPTIONAL{ ?s gsp:hasGeometry ?o.
              ?o gsp:asWKT ?geom}
    }
    '''
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/blazegraph"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
    # Querying using SPARQLWrapper for now
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting Gas Usage Query...')
    start = time.time()
    ret = sparql.query().convert()
    end = time.time()
    print('Finished in a time of ',np.round(end-start,3),' seconds')

    # parsing JSON into an array 
    values = ret['results']['bindings']
    head = ret['head']['vars']
    res_array = np.zeros((len(values)+1,len(head)),dtype='object')
    res_array[0,:] = head
    i = 1
    for row in values:
        j = 0 
        for val in row.values():
            res_array[i,j] = val['value']
            j += 1 
        i += 1 

    usage_vals = res_array[1:,:]

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
    usage_vals = np.concatenate((usage_vals,centroids),axis=1)
    usage_vals = np.delete(usage_vals,del_index,axis=0)
    return usage_vals

LSOA = LSOA_polygon_query(limit=False)
#LSOA Columns are LSOA IRI, Boundary, Centroid
lon,lat,tas = read_nc('tas',loc=True)
tasmin = read_nc('tasmin',loc=False)
tasmax = read_nc('tasmax',loc=False)
month,month_end = month_num('January')
grid_loc,nc_vars = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month)
clim_vars = ['tasmin','tas','tasmax']
full_grid = MultiPoint(points=list(grid_loc))

print('Computing associated points...')
for i in tqdm(range(int(len(LSOA)))):
    assoc_mask = [grid_loc[j].within(LSOA[i,1]) for j in range(len(grid_loc))]
    if any(assoc_mask) == False:
        assoc_point = nearest_points(full_grid,LSOA[i,2])[0]
        for j in range(len(grid_loc)):
            if assoc_point == grid_loc[j]:
                assoc_mask[j] = True 
                break
    LSOA_vars_full = nc_vars[assoc_mask,:]
    LSOA_vars = np.mean(LSOA_vars_full,axis=0)
    LSOA_IRI = LSOA[i,0]
    month_str = str(month+1)
    month_str_len = len(month_str)
    if month_str_len == 1:
        month_str = '0'+month_str

    LSOA_code = LSOA_IRI.split('/')[-1]
    startUTC = '2019-'+month_str+'-01T00:00:00'
    endUTC = '2019-'+month_str+'-'+str(month_end)+'T00:00:00'

    query = '''
    PREFIX xsd:      <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf:      <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX clima:    <http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/>
    PREFIX ons:      <http://statistics.data.gov.uk/id/statistical-geography/>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX gas:      <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    INSERT DATA
    { 
    '''
    for i in range(len(LSOA_vars)):
        meas_uuid = uuid.uuid4()
        clim_var = clim_vars[i]
        temp_uuid = uuid.uuid4()
        val_uuid = uuid.uuid4()

        query += '''
        clima:%s rdf:type clim:ClimateMeasurement.
        ons:%s clim:hasClimateMeasurement clima:%s .
        clima:%s rdf:type clim:ClimateVariable .
        clima:%s gas:hasStartUTC "%s"^^xsd:dateTime ;
                clim:hasClimateVariable  clima:%s ;
                gas:hasEndUTC "%s"^^xsd:dateTime .
        clima:%s rdf:type om:Temperature ;
                om:hasPhenomenon clima:%s ;
                om:hasValue      clima:%s .
        clima:%s rdf:type om:Measure ;
                om:hasUnit om:degreeCelsius ;
                om:hasNumericalValue %s .
            '''%(meas_uuid,
            LSOA_code,
            meas_uuid,
            clim_var,
            meas_uuid,
            startUTC,
            clim_var,
            endUTC,
            temp_uuid,
            meas_uuid,
            val_uuid,
            val_uuid,
            LSOA_vars[i])
    query += '''
    }
    '''
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/blazegraph"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'

    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    ret = sparql.query()
