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
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.crs import CRS
from netCDF4 import Dataset
import netCDF4 as nc
from rasterio.crs import CRS
import logging
from datamodel.iris import *
import csv
import geopandas as gpd
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST

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
    start_end_dict = {
        "2020-01-01T12:00:00Z":"2020-01-31T12:00:00Z",
        "2020-02-01T12:00:00Z":"2020-02-28T12:00:00Z",
        "2020-03-01T12:00:00Z":"2020-03-31T12:00:00Z",
        "2020-04-01T12:00:00Z":"2020-04-30T12:00:00Z",
        "2020-05-01T12:00:00Z":"2020-05-31T12:00:00Z",
        "2020-06-01T12:00:00Z":"2020-06-30T12:00:00Z",
        "2020-07-01T12:00:00Z":"2020-07-31T12:00:00Z",
        "2020-08-01T12:00:00Z":"2020-08-31T12:00:00Z",
        "2020-09-01T12:00:00Z":"2020-09-30T12:00:00Z",
        "2020-10-01T12:00:00Z":"2020-10-31T12:00:00Z",
        "2020-11-01T12:00:00Z":"2020-11-30T12:00:00Z",
        "2020-12-01T12:00:00Z":"2020-12-31T12:00:00Z"
    }
    temp_dict = call_pickle('./Data/temp_dict in function get_all_data')
    #g = Graph()
    for key_1, value_1 in tqdm(temp_dict.items()):
        for key, value in temp_dict[key_1].items():
            for key_3, value_3 in temp_dict[key_1][key].items():
                region = key_1
                clim_var = key_3
                meas_uuid = CLIMA + 'ClimateMeasurement_' + str(uuid.uuid4())
                temp_uuid = CLIMA + 'Temperature_' + str(uuid.uuid4())
                val_uuid = CLIMA + 'Value_' + str(uuid.uuid4())
                start_time = key[0:10] + "T"+ key[11:19] + "Z"
                end_time = start_end_dict[start_time]

                query_string = f"""
                    INSERT DATA {{<{region}> <{CLIMB_HASMEASURE}>  <{meas_uuid}> .
                        <{meas_uuid}> <{COMP_HAS_STARTUTC}> "{start_time}"^^<{XSD_DATETIME}>;
                                       <{RDF_TYPE}> <{CLIMB_CLIMATEMEASUREMENT}> ;
                            <{COMP_HAS_ENDUTC}> "{end_time}"^^<{XSD_DATETIME}>;
                            <{CLIMB_HASVAR}> "{clim_var}"^^<{XSD_STRING}> ;
                            <{OM_HAS_NUMERICALVALUE}> "{value_3}"^^<{XSD_FLOAT}>.
                                }}
                """
                DEF_NAMESPACE = "heatpump"
                LOCAL_KG = "http://localhost:3846/blazegraph"
                LOCAL_KG_SPARQL = LOCAL_KG + "/namespace/" + DEF_NAMESPACE + "/sparql"

                sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
                sparql.setMethod(POST)  # POST query, not GET
                sparql.setQuery(query_string)
                ret = sparql.query()

                ## region = URIRef(region)
                # clim_var = URIRef(clim_var)
                # temp_uuid = URIRef(temp_uuid)
                # val_uuid = URIRef(val_uuid)
                # meas_uuid = URIRef(meas_uuid)

                # CLIMB_HASMEASURE_a = URIRef(CLIMB_HASMEASURE)
                # RDF_TYPE_a = URIRef(RDF_TYPE)
                # CLIMB_CLIMBVARIABLE_a = URIRef(CLIMB_CLIMBVARIABLE)
                # CLIMB_CLIMATEMEASUREMENT_a = URIRef(CLIMB_CLIMATEMEASUREMENT)
                # COMP_HAS_STARTUTC_a = URIRef(COMP_HAS_STARTUTC)
                # COMP_HAS_ENDUTC_a = URIRef(COMP_HAS_ENDUTC)
                # CLIMB_HASVAR_a = URIRef(CLIMB_HASVAR)
                # OM_HAS_PHENO_a = URIRef(OM_HAS_PHENO)
                # OM_HAS_VALUE_a = URIRef(OM_HAS_VALUE)
                # OM_MEASURE_a = URIRef(OM_MEASURE)
                # OM_HAS_UNIT_a = URIRef(OM_HAS_UNIT)
                # OM_DEGREE_C_a = URIRef(OM_DEGREE_C)
                # OM_HAS_NUMERICALVALUE_a = URIRef(OM_HAS_NUMERICALVALUE)
                # OM_TEMPERATURE_a = URIRef(OM_TEMPERATURE)

                # # Add triples to the graph
                # g.add((region, CLIMB_HASMEASURE_a, meas_uuid))
                # g.add((clim_var, RDF_TYPE_a, CLIMB_CLIMBVARIABLE_a))
                # g.add((meas_uuid, RDF_TYPE_a, CLIMB_CLIMATEMEASUREMENT_a))
                # g.add((meas_uuid, COMP_HAS_STARTUTC_a, Literal(start_time, datatype=XSD_DATETIME)))
                # g.add((meas_uuid, COMP_HAS_ENDUTC_a, Literal(end_time, datatype=XSD_DATETIME)))
                # g.add((meas_uuid, CLIMB_HASVAR_a, Literal(clim_var, datatype=XSD_STRING)))
                # g.add((temp_uuid, RDF_TYPE_a, OM_TEMPERATURE_a))
                # g.add((temp_uuid, OM_HAS_PHENO_a, meas_uuid))
                # g.add((temp_uuid, OM_HAS_VALUE_a, val_uuid))
                # g.add((val_uuid, RDF_TYPE_a, OM_MEASURE_a))
                # g.add((val_uuid, OM_HAS_UNIT_a, OM_DEGREE_C_a))
                # g.add((val_uuid, OM_HAS_NUMERICALVALUE_a, Literal(value_3, datatype=XSD_FLOAT)))
                # for subj, pred, obj in g:
                #     print(subj, pred, obj)

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

def get_the_metadat_of_nc_file():
    # Open the NetCDF file
    nc_file = Dataset("C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_hadukgrid_uk_1km_mon_202001-202012.nc", "r")

    # Print the global attributes
    print("Global attributes:")
    for attr_name in nc_file.ncattrs():
        print(f"\t{attr_name}: {getattr(nc_file, attr_name)}")

    # Print the variable attributes
    print("Variable attributes:")
    for var_name, var in nc_file.variables.items():
        print(f"\t{var_name}:")
        for attr_name in var.ncattrs():
            print(f"\t\t{attr_name}: {getattr(var, attr_name)}")

    # Close the NetCDF file
    nc_file.close()

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
    lon_len = 900
    lat_len = 1450

    # Read the data from the variable
    data = nc_file.variables["tas"][:]

    # Close the NetCDF file
    nc_file.close()
    # Define the CRS using WKT
    wkt = """
    PROJCS["OSGB_1936_British_National_Grid",    GEOGCS["GCS_OSGB 1936",    DATUM["D_OSGB_1936",    SPHEROID["Airy_1830",6377563.396,299.3249646]],
    PRIMEM["Greenwich",0.0],
    UNIT["Degree",0.0174532925199433]],
    PROJECTION["Transverse_Mercator"],
    PARAMETER["False_Easting",400000.0],
    PARAMETER["False_Northing",-100000.0],
    PARAMETER["Central_Meridian",-2.0],
    PARAMETER["Scale_Factor",0.9996012717],
    PARAMETER["Latitude_Of_Origin",49.0],
    UNIT["Meter",1.0],
    AXIS["Easting",EAST],
    AXIS["Northing",NORTH],
    AUTHORITY["EPSG","27700"],
    AUTHORITY["EPSG","7405"]]
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
    lon_masked.min(), lat_masked.min(), lon_masked.max(), lat_masked.max(), lon_len, lat_len
)
    ) as dst:
        # Write the data to the GeoTIFF file
        for i in range(time_len):
            dst.write(data[i], i + 1)
    '''
    
    # Open the original Geotiff file
    with rasterio.open("C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_uk_temp_2020.tif") as src:
        # Define the target CRS as EPSG:4326
        dst_crs = "EPSG:4326"

        # Define the transform to use for the target CRS
        transform, width, height = rasterio.warp.calculate_default_transform(src.crs, dst_crs, src.width, src.height, *src.bounds)

        # Define the metadata for the target Geotiff file
        meta = src.meta.copy()
        meta.update({
            'crs': dst_crs,
            'transform': transform,
            'width': width,
            'height': height
        })

    # Create the target Geotiff file
    with rasterio.open("C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_uk_temp_2020_wgs84.tif", "w", **meta) as dst:
        # Reproject the data from the source to target CRS
        reproject(
            source=rasterio.band(src, 1),
            destination=rasterio.band(dst, 1),
            src_transform=src.transform,
            src_crs=src.crs,
            dst_transform=transform,
            dst_crs=dst_crs,
            resampling=Resampling.bilinear
        )
    '''
 #           

def make_geometry_valid():
    '''
    # ---------------------- Iteration --------------------------- #
    # Open the CSV file
    with open('./Data/LSOA_Geometry.csv', newline='') as csvfile:
    # Create a CSV reader object
        csvreader = csv.reader(csvfile)
        # Iterate over each row in the CSV file
        for row in csvreader:
            # Access the LSOA code in the first column
            lsoa_code = row[0]
            # Access the LSOA geometry shape in the second column
            lsoa_geometry = row[1]
            print('GOD please let it work')
    '''

    '''
    
    '''
    # Read the CSV file into a pandas DataFrame
    df = pd.read_csv('./Data/LSOA_Geometry.csv')

    # Delete rows with null or empty geometry values
    df = df.dropna(subset=['geometry'])
    df = df[df['geometry'] != '']

    # Convert the 'geometry' column to a geoseries object
    geometry = gpd.GeoSeries.from_wkt(df['geometry'])

    # Add the 'geometry' column to the DataFrame
    df['geometry'] = geometry

    # Convert the DataFrame to a GeoDataFrame
    gdf = gpd.GeoDataFrame(df, geometry='geometry')
    print(gdf.dtypes)

def New_convert_NetCDF_to_GeoTiff():
    # Open the NetCDF file and read in the data
    nc_file = nc.Dataset('C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_hadukgrid_uk_1km_mon_202001-202012.nc', 'r')
    data = nc_file.variables['tas'][:]
    src_file  = "C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_hadukgrid_uk_1km_mon_202001-202012.nc"
    tgt_file  = "C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_uk_temp_2020.tif"
    # Get the coordinate variables
    lat = nc_file.variables['latitude'][:]
    lon = nc_file.variables['longitude'][:]
    time = nc_file.variables['time'][:]
    lon_var = nc_file.variables["longitude"]
    lat_var = nc_file.variables["latitude"]
    lon_masked = np.ma.masked_array(lon_var[:], mask=np.ma.getmask(lon_var[:]))
    lat_masked = np.ma.masked_array(lat_var[:], mask=np.ma.getmask(lat_var[:]))
        
    # Define the output file name and spatial reference
    output_file = 'C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_uk_temp_2020.tif'
    # Define the source CRS using WKT
    src_crs_wkt = """
    PROJCS["British_National_Grid",
        GEOGCS["GCS_OSGB_1936",
            DATUM["D_OSGB_1936",
                SPHEROID["Airy_1830",6377563.396,299.3249646]],
            PRIMEM["Greenwich",0],
            UNIT["Degree",0.0174532925199433]],
        PROJECTION["Transverse_Mercator"],
        PARAMETER["latitude_of_origin",49],
        PARAMETER["central_meridian",-2],
        PARAMETER["scale_factor",0.9996012717],
        PARAMETER["false_easting",400000],
        PARAMETER["false_northing",-100000],
        UNIT["Meter",1]
    ]"""
    # Open the source file and read its metadata
    # Define the target CRS using EPSG code
    tgt_crs = CRS.from_wkt("""
        PROJCS["OSGB_1936_British_National_Grid",
        GEOGCS["GCS_OSGB 1936",
        DATUM["D_OSGB_1936",
        SPHEROID["Airy_1830",6377563.396,299.3249646]],
        PRIMEM["Greenwich",0.0],
        UNIT["Degree",0.0174532925199433]],
        PROJECTION["Transverse_Mercator"],
        PARAMETER["False_Easting",400000.0],
        PARAMETER["False_Northing",-100000.0],
        PARAMETER["Central_Meridian",-2.0],
        PARAMETER["Scale_Factor",0.9996012717],
        PARAMETER["Latitude_Of_Origin",49.0],
        UNIT["Meter",1.0]]
    """)
    with rasterio.open(src_file) as src:
        src_profile = src.profile
        src_transform = src.transform
        src_crs = src.crs

        # Calculate the transformation parameters between the source and target CRS
        tgt_transform, tgt_width, tgt_height = calculate_default_transform(
            src_crs, tgt_crs, src.width, src.height, *src.bounds)

        # Update the target profile with the new metadata
        tgt_profile = src_profile.copy()
        tgt_profile.update({
            'crs': tgt_crs,
            'transform': tgt_transform,
            'width': tgt_width,
            'height': tgt_height
        })

        # Create the target file and write the reprojected data
        # Create a new GeoTIFF file
    with rasterio.open(
        "C:/Users/jx309/Documents/TheWorldAvatar/Agents/Test_Repo/Data/tas_uk_temp_2020.tif",
        "w",
        driver="GTiff",
        height=1450,
        width=900,
        count=12,
        dtype=data.dtype,
        crs=tgt_crs,
        transform=rasterio.transform.from_bounds(
            lat_masked.min(), lon_masked.min(), lat_masked.max(), lon_masked.max(), 900, 1450
        )
    ) as tgt:
            for i in range(1, src.count + 1):
                reproject(
                    source=rasterio.band(src, i),
                    destination=rasterio.band(tgt, i),
                    src_transform=src_transform,
                    src_crs=src_crs_wkt,
                    dst_transform=tgt_transform,
                    dst_crs=tgt_crs,
                    resampling=Resampling.nearest)


#############################################################################
#                                                                           #
#                                                                           #
# ----------------------------- Work Board -------------------------------- #
#                                                                           #
#                                                                           #
#############################################################################
#get_the_metadat_of_nc_file()
#convert_NetCDF_to_GeoTiff()
#convert_NetCDF_to_GeoTiff()
generate_temp_rdf()
#consump_figure_using_tom_code()
#make_geometry_valid()