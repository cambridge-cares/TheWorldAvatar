from enum import unique
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
from numpy.core.defchararray import add
from tqdm import tqdm 
import numpy as np 
import matplotlib.pyplot as plt
import geopandas as gpd
from mpl_toolkits.axes_grid1.inset_locator import zoomed_inset_axes, mark_inset
from mpl_toolkits.axes_grid1.anchored_artists import AnchoredSizeBar
from datetime import datetime
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



def region_query(limit):
    '''
    Querying the KG for all regions gas-usages in 2019 
    '''
    if limit == False:
        limit = str(10000000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    
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

    usage_vals = res_array[1:,:]
    return usage_vals

def region_usage_query(limit):
    '''
    Querying the KG for all regions gas-usages in 2019 
    '''
    if limit == False:
        limit = str(10000000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    
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
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/blazegraph"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
    # Querying using SPARQLWrapper for now
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting LSOA Gas Usage Query...')
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

def region_meters_query(limit):
    '''
    Querying the KG for all regions gas meters in 2019 
    '''
    if limit == False:
        limit = str(10000000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    
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
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/blazegraph"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
    # Querying using SPARQLWrapper for now
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting LSOA Gas Usage Query...')
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


filename = 'temp_array'
gas_filename = 'gas_array'
meters_filename = 'meters_array'

# all_results = region_query(limit=False)
# #-----------------# 
# outfile = open(filename,'wb')
# pickle.dump(all_results,outfile)
# outfile.close()

# gas_results = region_usage_query(limit=False)
# #-----------------# 
# outfile = open(gas_filename,'wb')
# pickle.dump(gas_results,outfile)
# outfile.close()

# meters_results = region_meters_query(limit=False)
# #-----------------# 
# outfile = open(meters_filename,'wb')
# pickle.dump(meters_results,outfile)
# outfile.close()
# # -----------------# 

infile = open(filename,'rb')
all_results = pickle.load(infile)
infile.close()
infile = open(gas_filename,'rb')
gas_results = pickle.load(infile)
infile.close()
infile = open(meters_filename,'rb')
meters_results = pickle.load(infile)
infile.close()
#-----------------# 

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


unique_LSOA = np.unique(all_results[:,0])
results_tensor = np.zeros((3,len(unique_LSOA),12))
gas_tensor = np.zeros(len(unique_LSOA))
meters_tensor = np.zeros((len(unique_LSOA),2))


t_dict = {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
          'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
          'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2}

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

lsoa_dict = {}
for i in range(len(unique_LSOA)):
    lsoa_dict[unique_LSOA[i]] = i 


print('Formatting query results...')

for j in tqdm(range(len(all_results[:,0]))):
    t_ind = t_dict[all_results[j,1]]
    d_ind = date_dict[all_results[j,2]]
    lsoa_ind = lsoa_dict[all_results[j,0]]
    results_tensor[t_ind,lsoa_ind,d_ind] = all_results[j,-1]
for j in tqdm(range(len(gas_results[:,0]))):
    try:
        gas_lsoa_ind = lsoa_dict[gas_results[j,0]]
    except KeyError:
        print('Invalid region: ',gas_results[j,0])
    gas_tensor[gas_lsoa_ind] = gas_results[j,1]

for j in tqdm(range(len(meters_results[:,0]))):
    try:
        gas_lsoa_ind = lsoa_dict[meters_results[j,0]]
    except KeyError:
        print('Invalid region: ',meters_results[j,0])
    meters_tensor[gas_lsoa_ind,0] = int(meters_results[j,1])
    if meters_results[j,2] != '-':
        meters_tensor[gas_lsoa_ind,1] = int(meters_results[j,2])
    else: 
        meters_tensor[gas_lsoa_ind,1] = 0 


def COP(T_c):
    return 0.5*((35+273.15)/(35-T_c))


scaled_gas_tensor = np.zeros_like(gas_tensor)

for i in range(len(gas_tensor)):
    scaled_gas_tensor[i] = gas_tensor[i] + gas_tensor[i] * meters_tensor[i,1] / meters_tensor[i,0]


monthly_total_demand = [9.7358,7.389,7.17968,8.073659,5.4084,4.4428,3.93779,3.3926,4.004,6.117,7.989,8.154]
total_uk_demand = sum(monthly_total_demand)

months = ['January','February','March','April','May','June','July','August','September','October','November','December']
# plt.figure()
# plt.grid()
# plt.title('Monthly UK gas demand')
# plt.ylabel('Billion cubic meters')
# plt.plot(np.arange(len(monthly_total_demand)),monthly_total_demand)
# plt.xticks(np.arange(len(months)),months)
# plt.show()

scaled_monthly_gas_tensor = np.zeros((len(unique_LSOA),12))

for i in range(len(scaled_gas_tensor)):
    for j in range(len(months)):
        scaled_monthly_gas_tensor[i,j] = scaled_gas_tensor[i] * monthly_total_demand[j] / total_uk_demand
    



temp_tensor = results_tensor[1,:,:]
cop_tensor = np.array(list(map(COP, temp_tensor)))

energy_in_tensor = np.divide(scaled_monthly_gas_tensor,cop_tensor)


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


shapes_filename = 'shapes_array'
# LSOA_shapes = query_poly(limit=False)
# #-----------------# 
# outfile = open(shapes_filename,'wb')
# pickle.dump(LSOA_shapes,outfile)
# outfile.close()

infile = open(shapes_filename,'rb')
LSOA_shapes = pickle.load(infile)
infile.close()
'''
check if WKT is valid
'''

for i in range(len(LSOA_shapes[:,1])):
    if type(LSOA_shapes[i,1]) == int:
        del_ind = i 

LSOA_shapes = np.delete(LSOA_shapes,del_ind,axis=0)
LSOA_shapes = dict(LSOA_shapes)

vals_of_interest = np.zeros(len(energy_in_tensor[:,1]))
temp_of_interest = np.zeros(len(energy_in_tensor[:,1]))
energy_of_interest = np.zeros(len(energy_in_tensor[:,1]))

shapes_of_interest = np.zeros(len(energy_in_tensor[:,1]),dtype='object')
cop_of_interest = np.zeros(len(energy_in_tensor[:,1]),dtype='object')

for i in range(len(vals_of_interest)):
    key = unique_LSOA[i]
    shapes_of_interest[i] = LSOA_shapes[key]
    vals_of_interest[i] = scaled_monthly_gas_tensor[i,2]
    temp_of_interest[i] = results_tensor[1,i,2]
    energy_of_interest[i] = energy_in_tensor[i,2]
    cop_of_interest[i] = cop_tensor[i,2]


df = pd.DataFrame(unique_LSOA)
df['geometry'] = gpd.GeoSeries.from_wkt(shapes_of_interest)
df['energy'] = list(energy_of_interest)
df['gas'] = list(vals_of_interest)
df['temp'] = list(temp_of_interest)
df['cop'] = list(cop_of_interest)
my_geo_df = gpd.GeoDataFrame(df, geometry='geometry')



# fig,axs = plt.subplots(1,4)
# my_geo_df.plot(column='energy',norm=cl.LogNorm(vmin=10000, vmax=10000000),legend=True,cmap='OrRd',antialiased=False,ax=axs[3])
# my_geo_df.plot(column='gas',norm=cl.LogNorm(vmin=10000, vmax=10000000),legend=True,cmap='OrRd',antialiased=False,ax=axs[0])
# my_geo_df.plot(column='temp',legend=True,cmap='OrRd',antialiased=False,ax=axs[2])
# my_geo_df.plot(column='cop',legend=True,cmap='OrRd',antialiased=False,ax=axs[1])
# axs[0].set_title('gas consumption')
# axs[3].set_title('heat pump consumption')
# axs[2].set_title('temperature')
# axs[1].set_title('COP')
# plt.show()

plt.rc('text', usetex=True)
plt.rc('font', family='sans-serif')
import os
print(os.environ['PATH'])

fig,axs = plt.subplots(1,2,figsize=(10,5))
plt.subplots_adjust(wspace=0.31,left=0.074,right=0.93)
divider = make_axes_locatable(axs[0])
cax1 = divider.append_axes("right", size="5%", pad=0.05)
divider = make_axes_locatable(axs[1])
cax2 = divider.append_axes("right", size="5%", pad=0.05)
tl = my_geo_df.plot(column='energy',norm=cl.LogNorm(vmin=10000, vmax=500000),cmap='coolwarm',antialiased=False,ax=axs[0],legend=True,cax=cax1,legend_kwds={'label':'kWh'})
tr = my_geo_df.plot(column='temp',cmap='coolwarm',antialiased=False,ax=axs[1],legend=True,cax=cax2,legend_kwds={'label':'°C'})

axs[0].set_title('Gas Consumption')
axs[1].set_title('Mean Air Temperature')
axs[0].set_xlabel('Longitude')
axs[0].set_ylabel('Latitude')
axs[1].set_xlabel('Longitude')
axs[1].set_ylabel('Latitude')

# MANCHESTER
axins2 = zoomed_inset_axes(axs[1], zoom=6, loc=1)
plt.setp(axins2.get_xticklabels(), visible=False)
plt.setp(axins2.get_yticklabels(), visible=False)
axins2.set_xlim(-2.5,-2)
axins2.set_ylim(53.4,53.9)

# # WESTMINSTER
# axins2 = zoomed_inset_axes(axs[0], zoom=43, loc=1)
# plt.setp(axins2.get_xticklabels(), visible=False)
# plt.setp(axins2.get_yticklabels(), visible=False)
# axins2.set_xlim(-0.19,-0.09)
# axins2.set_ylim(51.48,51.51)

my_geo_df.plot(column='energy',norm=cl.LogNorm(vmin=10000, vmax=500000),cmap='coolwarm',antialiased=False,ax=axins2)
mark_inset(axs[0],axins2,loc1=2,loc2=4,fc='none',ec='0.5')


# MANCHESTER
axins2 = zoomed_inset_axes(axs[1], zoom=6, loc=1)
plt.setp(axins2.get_xticklabels(), visible=False)
plt.setp(axins2.get_yticklabels(), visible=False)
axins2.set_xlim(-2.5,-2)
axins2.set_ylim(53.4,53.9)

# # WESTMINSTER
# axins2 = zoomed_inset_axes(axs[1], zoom=43, loc=1)
# plt.setp(axins2.get_xticklabels(), visible=False)
# plt.setp(axins2.get_yticklabels(), visible=False)
# axins2.set_xlim(-0.19,-0.09)
# axins2.set_ylim(51.48,51.51)

my_geo_df.plot(column='temp',cmap='coolwarm',antialiased=False,ax=axins2)
mark_inset(axs[1],axins2,loc1=2,loc2=4,fc='none',ec='0.5')



plt.show()

