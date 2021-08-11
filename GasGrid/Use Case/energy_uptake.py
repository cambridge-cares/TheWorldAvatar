from enum import unique
from typing import Type
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
from numpy.core.defchararray import add
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
import geopandas as gpd
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes, mark_inset
from mpl_toolkits.axes_grid1.anchored_artists import AnchoredSizeBar
from datetime import datetime
from geomet import wkt
import shapely.wkt
import seaborn as sb 
import json
import imageio
import rdflib
from mpl_toolkits.axes_grid1 import make_axes_locatable
import pandas as pd 
import uuid 
import time
import matplotlib.colors as cl 
from rdflib.namespace import DC, DCTERMS, DOAP, FOAF, SKOS, OWL, RDF, RDFS, VOID, XMLNS, XSD
import os
import pickle

namespace = 'http://localhost:9999/blazegraph/namespace/ontogasgrid/sparql'

def standard_query(query,namespace,limit):
    if limit == False:
        limit = str(10000000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    LOCAL_KG_SPARQL = namespace
    # Querying using SPARQLWrapper for now
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting Query...')
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
    print('Parsing result of length '+str(len(res_array)))
    for row in tqdm(values):
        j = 0 
        for val in row.values():
            res_array[i,j] = val['value']
            j += 1 
        i += 1 
    usage_vals = res_array[1:,:]
    return usage_vals

# QUERYING TEMPERATURES 
def region_temp_query(limit):
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX comp:     <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    SELECT ?s ?start ?end ?var ?t 
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography.
    ?s clim:hasClimateMeasurement ?m.
    ?m comp:hasStartUTC ?start;
        comp:hasEndUTC   ?end.
    ?m  clim:hasClimateVariable ?var.
    ?p om:hasPhenomenon ?m.
    ?p om:hasValue ?oval.
    ?oval om:hasNumericalValue ?t.
    }
    '''
    usage_vals = standard_query(query,namespace,limit=False)
    return usage_vals

# QUERYING GAS CONSUMPTION
def region_usage_query(limit):
    '''
    Querying the KG for all regions gas-usages in 2019 
    '''
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX comp:     <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    SELECT ?s ?usage
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography;
     comp:hasUsed ?gas.
    ?energy om:hasPhenomenon ?gas;
            om:hasValue ?enval.
    ?enval om:hasNumericalValue ?usage.
    }
    '''
    usage_vals = standard_query(query,namespace,limit=False)
    return usage_vals

# QUERY ELECTRICY CONSUMPTION 
def region_elec_usage_query(limit):
    '''
    Querying the KG for all regions elec-usages in 2019 
    '''
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX comp:     <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    SELECT ?s ?usage
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography;
     comp:hasConsumed ?elec.
    ?energy om:hasPhenomenon ?elec;
            om:hasValue ?enval.
    ?enval om:hasNumericalValue ?usage.
    }
    '''
    usage_vals = standard_query(query,namespace,limi=False)
    return usage_vals

# QUERYING GAS METERS
def region_meters_query(limit):
    '''
    Querying the KG for all regions gas meters in 2019 
    '''
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX comp:     <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX gg:       <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#>

    SELECT ?s ?con ?non
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography;
     gg:hasGasMeters ?met.
     ?met gg:hasConsumingGasMeters ?con.
     ?met gg:hasNonConsumingGasMeters ?non
    }
    '''
    usage_vals = standard_query(query,namespace,limit=False)
    return usage_vals

# QUERYING ELECTRICTY METERS
def region_elec_meters_query(limit):
    '''
    Querying the KG for all regions electricity meters in 2019 
    '''
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX comp:     <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX gg:       <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#>

    SELECT ?s ?con 
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography;
     gg:hasElecMeters ?met.
     ?met gg:hasConsumingElecMeters ?con.
    }
    '''
    usage_vals = standard_query(query,namespace,limit=False)
    return usage_vals

# QUERYING FUEL POVERTY (GET % AS AN OUTPUT HERE)
def region_fuel_pov_query(limit):
    '''
    Querying the KG for all regions fuel poverty in 2019 
    households and fuel poor households
    '''
    query='''

            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ofp:    <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#>
        PREFIX ofpt:   <http://www.theworldavatar.com/kb/ontofuelpoverty/abox/>
        PREFIX ons:     <http://statistics.data.gov.uk/id/statistical-geography/>
        PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>

    SELECT ?s (xsd:float(?a)/xsd:float(?b) AS ?result)
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography;
        ofp:hasHouseholds ?houses.
     ?houses ofp:fuelpoorhouseholds ?a.
     ?houses ofp:numberofhouseholds ?b.
    }
    '''
    usage_vals = standard_query(query,namespace,limit=False)
    return usage_vals


# --------------------------------#
# Storing and calling queries     #
# from pickle files whilst        #
# testing                         #
# --------------------------------#

testing = True # True for pickle, False for query
def call_pickle(pathname):
    infile = open(pathname,'rb')
    results = pickle.load(infile)
    infile.close()
    return results

def save_pickle(query,pathname):
    results = query(limit=False)
    outfile = open(pathname,'wb')
    pickle.dump(results,outfile)
    outfile.close()
    return results

filename = 'pickle_files/temp_array'
gas_filename = 'pickle_files/gas_array'
meters_filename = 'pickle_files/meters_array'
elec_filename = 'pickle_files/elec_array'
elec_meters_filename = 'pickle_files/elec_meters_array'
fuel_poor_filename = 'pickle_files/fuel_poor'
if testing == True: 
    all_results = call_pickle(filename)
    gas_results = call_pickle(gas_filename)
    meters_results = call_pickle(meters_filename)
    elec_results = call_pickle(elec_filename)
    elec_meters_results = call_pickle(elec_meters_filename)
    fuel_poor_results = call_pickle(fuel_poor_filename)
else:
    all_results = save_pickle(region_temp_query,filename)
    gas_results = save_pickle(region_usage_query,gas_filename)
    meters_results = save_pickle(region_meters_query,filename)
    elec_results = save_pickle(region_elec_usage_query,elec_filename)
    elec_meters_results = save_pickle(region_elec_meters_query,elec_meters_filename)
    fuel_poor_results = save_pickle(region_fuel_pov_query,fuel_poor_filename)


# function to query and plot temperature values for a given LSOA
def plot_LSOA_temps(LSOA):
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons:      <http://statistics.data.gov.uk/id/statistical-geography/>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX clim:     <http://www.theworldavatar.com/ontology/ontogasgrid/ontoclimate.owl#>
    PREFIX comp:     <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    SELECT ?start ?end ?var ?t
    WHERE
    {       
    ons:%s clim:hasClimateMeasurement ?m.
    ?m comp:hasStartUTC ?start;
        comp:hasEndUTC   ?end.
    ?m  clim:hasClimateVariable ?var.
    ?p om:hasPhenomenon ?m.
    ?p om:hasValue ?oval.
    ?oval om:hasNumericalValue ?t.
    }
    '''%(LSOA)
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/blazegraph"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
    # Querying using SPARQLWrapper for now
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting LSOA Temperature Query...')
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
    print('Parsing result of length '+str(len(res_array)))
    for row in tqdm(values):
        j = 0 
        for val in row.values():
            res_array[i,j] = val['value']
            j += 1 
        i += 1 

    results = res_array[1:,:]

    min_temp = [] 
    mean_temp = [] 
    max_temp = [] 
    datetime_min = []
    datetime_max = [] 
    datetime_mean = []
    for i in range(len(results)):
        if results[i,0] == 'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':
            min_temp.append(float(results[i,-1]))
            datetime_min.append(datetime.strptime(results[i,1], '%Y-%m-%dT%H:%M:%S.000Z'))
        elif results[i,0] == 'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':
            mean_temp.append(float(results[i,-1]))
            datetime_mean.append(datetime.strptime(results[i,1], '%Y-%m-%dT%H:%M:%S.000Z'))
        else:
            max_temp.append(float(results[i,-1]))
            datetime_max.append(datetime.strptime(results[i,1], '%Y-%m-%dT%H:%M:%S.000Z'))

    plt.figure()
    plt.xlabel('Time')
    plt.ylabel('C')
    plt.title(LSOA)
    plt.scatter(datetime_max,max_temp,label='Max Air Temperature')
    plt.scatter(datetime_min,min_temp,label='Min Air Temperature')
    plt.scatter(datetime_mean,mean_temp,label='Mean Air Temperature')
    plt.grid()
    plt.legend()
    plt.show()
    return 
#plot_LSOA_temps('S01011468')
# Querying Polygons from KG to construct geoJSON
def query_poly(limit):
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

    LSOA_shapes = res_array[1:,:]

    return LSOA_shapes

# Function to calculate heating COP from outside temperature
# Assumes a heating temp of 35 degrees C and efficiency of 0.5
def COP(T_c):
    return 0.5*((35+273.15)/(35-T_c))



unique_LSOA = np.unique(all_results[:,0]) # Get unique LSOA keys
# pre-allocated memory for temperature values (TYPE,LSOA,MONTH)
# where type is [min,mean,max]
results_tensor = np.zeros((3,len(unique_LSOA),12)) 
# preallocate yearly gas consumption per LSOA array (LSOA)
gas_tensor = np.zeros(len(unique_LSOA))
# preallocate consuming and non-consuming meters tensor (LSOA,METER)
meters_tensor = np.zeros((len(unique_LSOA),2))

elec_tensor = np.zeros(len(unique_LSOA))
# preallocate consuming and non-consuming meters tensor (LSOA,METER)
elec_meters_tensor = np.zeros(len(unique_LSOA))

fuel_poor_tensor = np.zeros(len(unique_LSOA))



# Note: Using dictionaries to go from IRIs to arrays is wicked quick and better than doing nested loops

# Dictionary to convert climate var type to index in tensor
t_dict = {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
          'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
          'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2}

# Dictionary to convert datetime (month) to index in tensor
date_dict = {'2019-01-01T12:00:00.000Z':0,\
             '2019-02-01T12:00:00.000Z':1,\
             '2019-03-01T12:00:00.000Z':2,\
             '2019-04-01T12:00:00.000Z':3,\
             '2019-05-01T12:00:00.000Z':4,\
             '2019-06-01T12:00:00.000Z':5,\
             '2019-07-01T12:00:00.000Z':6,\
             '2019-08-01T12:00:00.000Z':7,\
             '2019-09-01T12:00:00.000Z':8,\
             '2019-10-01T12:00:00.000Z':9,\
             '2019-11-01T12:00:00.000Z':10,\
             '2019-12-01T12:00:00.000Z':11}

# Dictionary to convert LSOA IRIs to index in tensor
lsoa_dict = {}
for i in range(len(unique_LSOA)):
    lsoa_dict[unique_LSOA[i]] = i 


# Now we have everything in place to format results into tensors 
print('Formatting query results...')

# PARSING TEMPERATURE INTO TENSOR 
# --------------------------------#
# iterating over the list of all temperature values 
for j in tqdm(range(len(all_results[:,0]))):
    # getting the right indexes based off date, temp type and LSOA key
    # (think of it as identifying the right spot in the cube)
    t_ind = t_dict[all_results[j,1]]
    d_ind = date_dict[all_results[j,2]]
    lsoa_ind = lsoa_dict[all_results[j,0]]
    # allocating a value (last index is value)
    results_tensor[t_ind,lsoa_ind,d_ind] = all_results[j,-1]

# PARSING GAS INTO TENSOR 
# --------------------------------#
# iterating over gas results query
for j in tqdm(range(len(gas_results[:,0]))):
    try:
        # try and find the index for the LSOA of each gas value
        gas_lsoa_ind = lsoa_dict[gas_results[j,0]]
        gas_tensor[gas_lsoa_ind] = gas_results[j,1]
    except KeyError: # if it doesn't exist...
        print('No gas data for ',gas_results[j,0].split('/')[-1])


# PARSING METERS INTO TENSOR
# --------------------------------#
# iterating over meters results query
for j in tqdm(range(len(meters_results[:,0]))):
    # try to identify LSOA key
    try:
        gas_lsoa_ind = lsoa_dict[meters_results[j,0]]
        meters_tensor[gas_lsoa_ind,0] = int(meters_results[j,1])
    except KeyError:
        print('No gas meter data for ',meters_results[j,0].split('/')[-1])
    # first is consuming gas meters
    # second is non-consuming 
    if meters_results[j,2] != '-':
        meters_tensor[gas_lsoa_ind,1] = int(meters_results[j,2])
    else: # if none then set to 0 (stored weirdly in KG [Sorry!])
        meters_tensor[gas_lsoa_ind,1] = 0 


# PARSING ELECTRICITY INTO TENSOR 
# --------------------------------#
# iterating over gas results query
for j in tqdm(range(len(elec_results[:,0]))):
    try:
        # try and find the index for the LSOA of each gas value
        gas_lsoa_ind = lsoa_dict[elec_results[j,0]]
        elec_tensor[gas_lsoa_ind] = elec_results[j,1]
    except KeyError: # if it doesn't exist...
        print('No electricity data for ',elec_results[j,0].split('/')[-1])


# PARSING ELECTRICITY METERS INTO TENSOR
# --------------------------------#
# iterating over meters results query
for j in tqdm(range(len(elec_meters_results[:,0]))):
    # try to identify LSOA key
    try:
        elec_lsoa_ind = lsoa_dict[elec_meters_results[j,0]]
        elec_meters_tensor[elec_lsoa_ind] = int(elec_meters_results[j,1])
    except KeyError:
        print('No electricity meter data for ',elec_meters_results[j,0].split('/')[-1])
    

# PARSING FUEL POVERTY INTO TENSOR
# --------------------------------#
# iterating over meters results query
for j in tqdm(range(len(fuel_poor_results[:,0]))):
    # try to identify LSOA key
    try:
        fuel_poor_lsoa_ind = lsoa_dict[fuel_poor_results[j,0]]
        fuel_poor_tensor[fuel_poor_lsoa_ind] = fuel_poor_results[j,1]
    except KeyError:
        print('No fuel poverty data for ',fuel_poor_results[j,0].split('/')[-1])
    
    



# vector of TOTAL gas consumption in 2019 by month
# used to scale yearly LSOA values
# ASSUMPTION: all LSOAs scale proportionately to yearly demand.
monthly_total_gas_demand = [9.7358,7.389,7.17968,8.073659,5.4084,4.4428,3.93779,3.3926,4.004,6.117,7.989,8.154]
total_uk_demand = sum(monthly_total_gas_demand)
months = ['January','February','March','April','May','June','July','August','September','October','November','December']
plot = False 
if plot == True:
    plt.figure()
    plt.grid()
    plt.title('Monthly UK gas demand')
    plt.ylabel('Billion cubic meters')
    plt.plot(np.arange(len(monthly_total_gas_demand)),monthly_total_gas_demand)
    plt.xticks(np.arange(len(months)),months)
    plt.show()
else:
    print('Not plotting temperature')

# preallocating disaggregated monthly gas consumption tensor
monthly_gas_tensor = np.zeros((len(unique_LSOA),12))

# scaling yearly gas values for each LSOA to monthly values
for i in range(len(gas_tensor)):
    for j in range(len(months)):
        monthly_gas_tensor[i,j] = gas_tensor[i] * monthly_total_gas_demand[j] / total_uk_demand


# vector of TOTAL electricity consumption in 2019 by month
# used to scale yearly LSOA values
# ASSUMPTION: all LSOAs scale proportionately to yearly demand.
monthly_total_elec_demand = [29.61,25.1,26.13,24.67,23.75,22.3,23.19,23.01,22.82,25.75,27.48,27.82]
total_uk_elec_demand = sum(monthly_total_elec_demand)
if plot == True:
    plt.figure()
    plt.grid()
    plt.title('Monthly UK electricity demand')
    plt.ylabel('Terrawatt Hours')
    plt.plot(np.arange(len(monthly_total_elec_demand)),monthly_total_elec_demand)
    plt.xticks(np.arange(len(months)),months)
    plt.show()
else:
    print('Not plotting temperature')

# preallocating disaggregated monthly gas consumption tensor
monthly_elec_tensor = np.zeros((len(unique_LSOA),12))

# scaling yearly gas values for each LSOA to monthly values
for i in range(len(elec_tensor)):
    for j in range(len(months)):
        monthly_elec_tensor[i,j] = elec_tensor[i] * monthly_total_elec_demand[j] / total_uk_elec_demand



def dataframe_construction(temp_var_type,uptake,month,df_box,complete_df):
    month_str = months[month]

    # getting mean min or max tensor
    temp_tensor = results_tensor[t_dict[temp_var_type],:,:] 
    # calculating COP
    cop_tensor = np.array(list(map(COP, temp_tensor)))      
    # caluclating converted gas to electricity via HP
    hp_in_tensor = np.divide((uptake*monthly_gas_tensor),cop_tensor) 
    # calculating leftover gas 
    resulting_gas_tensor = monthly_gas_tensor  * (1-uptake)
    # calculating resulting electricity 
    resulting_elec_tensor = monthly_elec_tensor + hp_in_tensor



    

    # Create arrays to extract values from the tensors from 
    # Note the tensors were just to organise everything and make 
    # sure the values are in the right place

    # preallocating memory 
    gas_values            = np.zeros(len(hp_in_tensor[:,1]))
    elec_values           = np.zeros_like(gas_values)
    remaining_elec_values = np.zeros_like(gas_values)
    remaining_gas_values  = np.zeros_like(gas_values)
    temp_values           = np.zeros_like(gas_values)
    poverty_values        = np.zeros_like(gas_values)
    delta_elec_values     = np.zeros_like(gas_values)
    shapes_of_interest    = np.zeros_like(gas_values,dtype='object')
    cop_values            = np.zeros_like(gas_values,dtype='object')


    # going over all the gas values
    for i in range(len(gas_values)):
        key = unique_LSOA[i] # getting the key for the specific LSOA
        # finding the respective 'shape'
        # assigning gas consumption 
        gas_values[i] = monthly_gas_tensor[i,month]
        # assigning elec consumption 
        elec_values[i] = monthly_elec_tensor[i,month]
        # assigning temperature value
        temp_values[i] = temp_tensor[i,month]
        # assigning additional electricity
        delta_elec_values[i] = hp_in_tensor[i,month]
        # assigning COP
        cop_values[i] = cop_tensor[i,month]
        # assigning remaining gas values
        remaining_gas_values[i] = resulting_gas_tensor[i,month]
        # assigning remaining elec values
        remaining_elec_values[i] = resulting_elec_tensor[i,month]
        # assigning remaining fuel poverty values
        poverty_values[i] = fuel_poor_tensor[i]



    ## CODE FOR PLOTTING *IN* PYTHON

    # new_df = pd.DataFrame(unique_LSOA)
    # new_df['geometry'] = gpd.GeoSeries.from_wkt(shapes_of_interest)
    # new_df['geom_str'] = list([str(x) for x in shapes_of_interest])
    # # properties 
    # new_df['delta_elec'] = list(delta_elec_values)
    # new_df['gas']    = list(np.around(gas_values,decimals=3))
    # new_df['elec']   = list(np.around(elec_values,decimals=3))
    # new_df['temp']   = list(np.around(temp_values,decimals=3))
    # new_df['cop']    = list(cop_values)
    # new_df['fuel_poor_percen'] = list(np.around(100*poverty_values,decimals=3))
    # new_df['remaining_gas']    = list(np.around(remaining_gas_values,decimals=3))
    # new_df['remaining_elec']   = list(np.around(remaining_elec_values,decimals=3))
    # new_df['month'] = list(np.array([month_str for i in range(len(gas_values))]))
    # df = df.append(new_df)

    new_df = pd.DataFrame({'LSOA':[]})
    new_df['Gas']    = list(np.around(gas_values,decimals=3))
    new_df['Electricity']   = list(np.around(elec_values,decimals=3))
    new_df['Usage']  = list(np.array(['Original' for i in range(len(gas_values))])) 
    new_df['Month (2019)'] = list(np.array([month_str for i in range(len(gas_values))]))
    df_box = df_box.append(new_df)  

    new_df = pd.DataFrame({'LSOA':[]})
    new_df['Gas']    = list(np.around(remaining_gas_values,decimals=3))
    new_df['Electricity']   = list(np.around(remaining_elec_values,decimals=3))
    new_df['Usage']  = list(np.array(['Transition' for i in range(len(gas_values))])) 
    new_df['Month (2019)'] = list(np.array([month_str for i in range(len(gas_values))]))
    df_box = df_box.append(new_df)  
 
    elec_co = 0.233
    gas_co = 0.184

    new_df = pd.DataFrame({'LSOA':[]})
    new_df['Gas']    = list(np.around(gas_values,decimals=3))
    new_df['Gas Emissions'] = list(np.around(gas_co * gas_values,decimals=3)) 
    new_df['Electricity']   = list(np.around(elec_values,decimals=3))
    new_df['Electricity Emissions']   = list(np.around(elec_co * elec_values,decimals=3))
    new_df['Total Emissions'] = list(np.around(elec_co * elec_values + gas_co * gas_values,decimals=3))
    new_df['Remaining Gas']    = list(np.around(remaining_gas_values,decimals=3))
    new_df['Remaining Electricity']   = list(np.around(remaining_elec_values,decimals=3))
    new_df['Month (2019)'] = list(np.array([month_str for i in range(len(gas_values))]))
    new_df['LSOA'] = list(unique_LSOA)
    new_df['Electricity Change'] = list(delta_elec_values)
    new_df['Temperature'] = list(np.around(temp_values,decimals=3))
    new_df['COP'] = list(cop_values)
    new_df['Percentage Fuel Poor'] = list(np.around(100*poverty_values,decimals=3))
    delta_elec_values = np.array(delta_elec_values)/np.array(elec_values)
    scaled_delta_elec = (delta_elec_values-np.mean(delta_elec_values))/(np.std(delta_elec_values))
    scaled_fuel_pov = (poverty_values-np.mean(poverty_values))/(np.std(poverty_values)) 
    inequality = scaled_delta_elec - scaled_fuel_pov
    new_df['Inequality Index']   = list(np.around(inequality,decimals=3))
    new_df['Emissions'] = ((np.array(remaining_gas_values)*gas_co)+(np.array(remaining_elec_values)*elec_co)) 
    new_df['Change in Emissions'] = new_df['Emissions'].to_numpy() - (new_df['Gas'].to_numpy()*gas_co + new_df['Electricity'].to_numpy()*elec_co) 

    complete_df = complete_df.append(new_df)  

    return df_box, complete_df



#plt.rc('text', usetex=True)
plt.rc('font', family='sans-serif')
import os

def plot_var_annual_uptake_with_temps():
    # define min mean or max 
    min  = 'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin'
    mean = 'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas'
    max  ='http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax' 
    temp_vars = [min,mean,max]
    temp_vars_label = ['Minimum Air Temperature','Mean Air Temperature','Maximum Air Temperature']
    temp_colors = ['tab:blue','tab:orange','k']
    styles = ['solid','dashed','dotted']

    months_letter = ['J','F','M','A','M','J','J','A','S','O','N','D']
    vars = ['Gas','Electricity']
    plt.figure(figsize=(4,3))
    plt.xlabel('% Uptake')
    plt.ylabel('Energy Consumption (kWh/year)')
    uptakes = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]
    results_gas = np.zeros((len(uptakes),3))
    results_elec = np.zeros((len(uptakes),3))
    for u in range(len(uptakes)):
        for j in range(len(temp_vars)):
            temp_var_type = temp_vars[j]
            # define amount of heat pump uptake 
            df_box = pd.DataFrame({'Gas' : []})
            complete_df = pd.DataFrame({'Gas' : []})
            for i in tqdm(range(12)):
                df_box, complete_df = dataframe_construction(temp_var_type,uptakes[u],i,df_box,complete_df)
            gas_sum = []
            elec_sum = []
            for i in range(len(months)):
                temp_df = complete_df[complete_df['Month (2019)'] == months[i]]
                gas_sum.append(sum(list(temp_df['Remaining '+str(vars[0])].values)))
                elec_sum.append(sum(list(temp_df['Remaining '+str(vars[1])].values)))
            results_gas[u,j] = sum(gas_sum)
            results_elec[u,j] = sum(elec_sum)
            
    plt.plot((np.array(uptakes)*100).astype(int),results_gas[:,1],c='k',linestyle='dashed',label='Gas')
    plt.fill_between((np.array(uptakes)*100).astype(int),results_gas[:,0],results_gas[:,2],color='k',alpha=0.1)
    plt.tight_layout()
    plt.plot((np.array(uptakes)*100).astype(int),results_elec[:,1],c='k',label='Electricity')
    plt.fill_between((np.array(uptakes)*100).astype(int),results_elec[:,0],results_elec[:,2],color='k',alpha=0.1)
    plt.ylim(bottom=0)
    plt.legend(frameon=False)
    plt.savefig('figure_output/energy_uptake.pdf')
    plt.savefig('figure_output/energy_uptake.png')



plot_var_annual_uptake_with_temps()
