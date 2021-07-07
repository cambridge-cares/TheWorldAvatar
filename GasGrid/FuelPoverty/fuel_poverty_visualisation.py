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



def fuel_poor_query(limit):
    '''
    Querying the KG for all poverty stats in the KG
    '''
    if limit == False:
        limit = str(10000000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ofp:    <http://www.theworldavatar.com/ontology/ontofuelpoverty/ontofuelpoverty.owl#>
    PREFIX ofpt:   <http://www.theworldavatar.com/kb/ontofuelpoverty/abox/>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>

    SELECT ?s ?start ?end ?houses ?poor
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography.
    ?s ofp:hasHouseholds ?h.
    ?h ofp:validFrom ?start;
       ofp:validTo   ?end;
       ofp:numberofhouseholds ?houses;
       ofp:fuelpoorhouseholds ?poor.
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
    print('Starting Fuel Poverty Query...')
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
    print('Starting LSOA Region Usage Query...')
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

LSOA_shapes = query_poly(limit = 10000000)
fuel_data  = fuel_poor_query(limit = 10000000)


'''
check if WKT is valid
'''

for i in range(len(LSOA_shapes[:,1])):
    if type(LSOA_shapes[i,1]) == int:
        del_ind = i 

LSOA_shapes = np.delete(LSOA_shapes,del_ind,axis=0)
LSOA_shapes = dict(LSOA_shapes)

percentage_poor = []
geom_list = []
LSOA_names = []
for i in tqdm(range(len(fuel_data))):
	region = fuel_data[i,0]
	percentage_poor += [(float(fuel_data[i,4])/float(fuel_data[i,3])*100)]
	geom_list += [(LSOA_shapes[region])]
	LSOA_names += [region]

df = pd.DataFrame(LSOA_names)
df['geometry'] = gpd.GeoSeries.from_wkt(geom_list)
df['fuel_poor'] = list(percentage_poor)
my_geo_df = gpd.GeoDataFrame(df, geometry='geometry')

print('Generating figure...')
plt.rc('text', usetex=True)
plt.rc('font', family='sans-serif')
import os
print(os.environ['PATH'])
fig = plt.figure(figsize=(5,10))
my_geo_df.plot(column='fuel_poor',cmap='coolwarm',antialiased=False,legend=True,legend_kwds={'label':'%'})
plt.title('Percentage of households that are fuel poor, 2019')
plt.xlabel('Longitude')
plt.ylabel('Latitude')
plt.tight_layout()
plt.savefig('output_images/national_fuel_poverty.pdf')

