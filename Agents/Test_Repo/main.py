from kgutils.querytemplates import *
import pickle
import datetime
from rdflib import Graph, Literal, URIRef, XSD
import uuid
from tqdm import tqdm
from SPARQLWrapper import SPARQLWrapper, JSON, POST
from tqdm import tqdm 
import numpy as np 
from datetime import datetime
import uuid 
import time
from rdflib.namespace import XSD
import pickle
import rasterio
from netCDF4 import Dataset
from rasterio.crs import CRS
import logging
from datamodel.iris import *

logging.basicConfig(filename='rasterio.log', level=logging.DEBUG)

def reformat_dates(input_dict):
    '''
    For some unknown reason, some of the date time are stored unstandardly
    This function is used to reformat the datetime
    '''
    # Create a new dictionary to store the reformatted dates
    output_dict = {}
    
    # Iterate over the keys and values in the input dictionary
    for key, value in input_dict.items():
        # Check if the key is in the 'YYYY-MM-DDTHH:MM:SS' format
        if len(key) == 19:
            # Parse the date and time
            dt = datetime.datetime.strptime(key, '%Y-%m-%dT%H:%M:%S')
            # Reformat the date and time as 'YYYY-MM-DDTHH:MM:SS.000Z'
            key = dt.strftime('%Y-%m-%dT%H:%M:%S.000Z')
        # Add the key-value pair to the output dictionary
        output_dict[key] = value

    return output_dict

def call_pickle(pathname):
    '''
  This module is to retrieve the result of the a pickle file under the pathname you specified
  could be useful to retrieve the result of a pickle file
  '''
    infile = open(pathname,'rb')
    results = pickle.load(infile)
    infile.close()

    return results
# Keyword: 'Electricity'/'Gas'/'Fuel poverty'/'Temperature'/'ONS output area'
# query = output_query_template('Fuel poverty','2020')
# parse_to_file(query,'Fuel poverty')

# update = climate_temperature_update_template('E1000001','meas_uuid','tas','2020-01-01T12:00:00Z','2020-01-31T12:00:00Z','temp_uuid','val_uuid',10)
# parse_to_file(update,'sample_temp_update')

def generate_temp_rdf():
    temp_dict = call_pickle('./Data/temp_dict in function get_all_data')
    g = Graph()
    for key_1, value_1 in tqdm(temp_dict.items()):
        for key, value in temp_dict[key_1].items():
            for key_3, value_3 in temp_dict[key_1][key].items():
                region = key_1
                clim_var = key_3
                meas_uuid = CLIMA + 'Measurement_' + str(uuid.uuid4())
                temp_uuid = CLIMA + 'Temperature_' + str(uuid.uuid4())
                val_uuid = CLIMA + 'Value_' + str(uuid.uuid4())
                start_time = key
                region = URIRef(region)
                clim_var = URIRef(clim_var)
                temp_uuid = URIRef(temp_uuid)
                val_uuid = URIRef(val_uuid)
                meas_uuid = URIRef(meas_uuid)

                CLIMB_HASMEASURE_a = URIRef(CLIMB_HASMEASURE)
                RDF_TYPE_a = URIRef(RDF_TYPE)
                CLIMB_CLIMBVARIABLE_a = URIRef(CLIMB_CLIMBVARIABLE)
                CLIMB_CLIMATEMEASUREMENT_a = URIRef(CLIMB_CLIMATEMEASUREMENT)
                COMP_HAS_STARTUTC_a = URIRef(COMP_HAS_STARTUTC)
                CLIMB_HASVAR_a = URIRef(CLIMB_HASVAR)
                OM_HAS_PHENO_a = URIRef(OM_HAS_PHENO)
                OM_HAS_VALUE_a = URIRef(OM_HAS_VALUE)
                OM_MEASURE_a = URIRef(OM_MEASURE)
                OM_HAS_UNIT_a = URIRef(OM_HAS_UNIT)
                OM_DEGREE_C_a = URIRef(OM_DEGREE_C)
                OM_HAS_NUMERICALVALUE_a = URIRef(OM_HAS_NUMERICALVALUE)
                OM_TEMPERATURE_a = URIRef(OM_TEMPERATURE)

                # Add triples to the graph
                g.add((region, CLIMB_HASMEASURE_a, meas_uuid))
                g.add((clim_var, RDF_TYPE_a, CLIMB_CLIMBVARIABLE_a))
                g.add((meas_uuid, RDF_TYPE_a, CLIMB_CLIMATEMEASUREMENT_a))
                g.add((meas_uuid, COMP_HAS_STARTUTC_a, Literal(start_time, datatype=XSD_DATETIME)))
                # g.add((meas_uuid, COMP_HAS_ENDUTC, Literal(end_time, datatype=XSD.DATETIME)))
                g.add((meas_uuid, CLIMB_HASVAR_a, Literal(clim_var, datatype=XSD_STRING)))
                g.add((temp_uuid, RDF_TYPE_a, OM_TEMPERATURE_a))
                g.add((temp_uuid, OM_HAS_PHENO_a, meas_uuid))
                g.add((temp_uuid, OM_HAS_VALUE_a, val_uuid))
                g.add((val_uuid, RDF_TYPE_a, OM_MEASURE_a))
                g.add((val_uuid, OM_HAS_UNIT_a, OM_DEGREE_C_a))
                g.add((val_uuid, OM_HAS_NUMERICALVALUE_a, Literal(value_3, datatype=XSD_FLOAT)))

    g.serialize(destination='./Data/temp.rdf',format="application/rdf+xml")

def consump_figure_using_tom_code():
    def standard_query(query, namespace, limit):
        if limit == False:
            limit = str(10000000000)
        limit = str(limit)
        # clearing terminal

        LOCAL_KG_SPARQL = namespace
        # Querying using SPARQLWrapper for now
        sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
        sparql.setMethod(POST)  # POST query, not GET
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        print("Starting Query...")
        start = time.time()
        ret = sparql.query().convert()
        end = time.time()
        print("Finished in a time of ", np.round(end - start, 3), " seconds")
        # parsing JSON into an array
        values = ret["results"]["bindings"]
        head = ret["head"]["vars"]
        res_array = np.zeros((len(values) + 1, len(head)), dtype="object")
        res_array[0, :] = head
        i = 1
        print("Parsing result of length " + str(len(res_array)))
        for row in tqdm(values):
            j = 0
            for val in row.values():
                res_array[i, j] = val["value"]
                j += 1
            i += 1
        usage_vals = res_array[1:, :]
        return usage_vals
    
    namespace = 'http://localhost:3845/ontop/ui/sparql'

    query = output_query_template('Gas','2020')
    usage_vals = standard_query(query, namespace, limit = False)
    
    print(usage_vals)

def convert_NetCDF_to_GeoTiff():
        # Open the NetCDF file using netCDF4
    nc_file = Dataset("C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_hadukgrid_uk_1km_mon_202001-202012.nc", "r")

    # Get the variables for time, longitude and latitude
    time_var = nc_file.variables["time"]
    lon_var = nc_file.variables["longitude"]
    lat_var = nc_file.variables["latitude"]
    lon_masked = np.ma.masked_array(lon_var[:], mask=np.ma.getmask(lon_var[:]))
    lat_masked = np.ma.masked_array(lat_var[:], mask=np.ma.getmask(lat_var[:]))
    time_masked = np.ma.masked_array(time_var[:], mask=np.ma.getmask(time_var[:]))

    # Read the dimensions of the variables
    time_len = len(time_var)
    lon_len = len(lon_var)
    lat_len = len(lat_var)

    # Read the data from the variable
    data = nc_file.variables["tas"][:]

    # Close the NetCDF file
    nc_file.close()
    # Define the CRS using WKT
    wkt = """
        GEOGCRS["WGS 84",    DATUM["World Geodetic System 1984",        ELLIPSOID["WGS 84",6378137,298.257223563,            LENGTHUNIT["metre",1]],
        ID["EPSG",6326]],
    CS[ellipsoidal,2],
        AXIS["geodetic latitude (Lat)",north,            ORDER[1],
            ANGLEUNIT["degree",0.0174532925199433]],
        AXIS["geodetic longitude (Lon)",east,            ORDER[2],
            ANGLEUNIT["degree",0.0174532925199433]],
    ID["EPSG",4326]]
    """

    # Create a CRS object from the WKT string
    crs = CRS.from_wkt(wkt)
    # Create a new GeoTIFF file
    with rasterio.open(
        "C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_uk_temp_2020.tif",
        "w",
        driver="GTiff",
        height=lat_len,
        width=lon_len,
        count=time_len,
        dtype=data.dtype,
        crs=crs,
        transform=rasterio.transform.from_bounds(
            lat_masked.min(), lon_masked.min(), lat_masked.max(), lon_masked.max(), lon_len, lat_len
        )
    ) as dst:
        # Write the data to the GeoTIFF file
        for i in range(time_len):
            dst.write(data[i], i + 1)
            
convert_NetCDF_to_GeoTiff()
#generate_temp_rdf()
#consump_figure_using_tom_code()