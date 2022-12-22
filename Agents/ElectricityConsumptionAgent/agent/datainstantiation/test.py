# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
from py4jps.resources import JpsBaseLib
from agent.kgutils.kgclient import KGClient
import pandas as pd
import json
#from agent.datainstantiation.readings import *

def parse_to_file(query):
  f = open("demofile3.txt", "w")
  f.write(query)
  f.close()

  #open and read the file after the appending:
  f = open("demofile3.txt", "r")

def create_geojson_for_postgis(station_iri: str, station_name: str, station_type: str,
                               station_subtype: str, lat: float, long: float, kg_endpoint: str):
    """
    Create GeoJSON object for upload to PostGIS database
    Needs to contain at least the following properties for FeatureInfoAgent to work:
        "name" - human readable name of the feature 
        "iri" - full IRI of the feature as represented in the knowledge graph
        "endpoint" - URL of the Blazegraph namespace containing data on the feature,
                     i.e. from where shall FeatureInfoAgent retrieve data about "iri"
    Further properties can be added as needed, i.e. for styling purposes
    """
    # Initialise GeoJSON
    geojson = {'type': 'Feature'}

    # Define properties
    props = {
        'iri': station_iri,
        'name': station_name,
        'endpoint': kg_endpoint,
        'geom_iri': station_iri + '/geometry',
        'type': station_type,
        'subtype': station_subtype,
    }

    # Define geometry
    geom = {
        'type': 'Point',
        'coordinates': [float(long), float(lat)]
    }

    # Construct GeoJSON
    geojson['properties'] = props
    geojson['geometry'] = geom
    
    print(json.dumps(geojson))
    return json.dumps(geojson)


create_geojson_for_postgis(station_iri = 'station_iri', station_name= 'name', station_type= 'type',
                               station_subtype= 'subtype', lat = 6 , long = 66 , kg_endpoint= 'kg_endpoin')
'''
df.to_csv('C:/Users/jx309/Documents/TheWorldAvatar/Agents/ElectricityConsumptionAgent/df.txt', sep='\t', index=False)

df = pd.DataFrame({"id": [25,53,15,47,52,54,45,9], "sex": list('mfmfmfmf'), 'score': [1.5, 1.2, 3.4, 4.5,6.4,5.7,5.6,4.3],"name":['daisy','tony','peter','tommy','ana','david','ken','jim']})
newdf = df[df['score'] == 1.2]["sex"].values
print(newdf)
'''

'''
# create a JVM module view and use it to import the required java classes
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# this function shows how to do a simple KG query
query ="""
select (count(?x) as ?nOfTripes)
{
  ?x ?y ?z .
  }
    """

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# query_endpoint=update_endpoint='http://localhost:8080/blazegraph/namespace/ontogasgrid/sparql'
query_endpoint=update_endpoint='http://localhost:8080/blazegraph/namespace/ontogasgrid/sparql'
kg_client = KGClient(query_endpoint, update_endpoint)
query_endpoint = kg_client.kg_client.setQueryEndpoint(query_endpoint)
result = kg_client.performQuery(query)
print(result)
'''

'''
'''

'''
usage_vals = np.array([["2019-01-01T12:00:00.000Z",0],
              ["2019-01-01T12:00:00.000Z",1],
              ["2019-03-01T12:00:00.000Z",2],
              ["2019-05-01T12:00:00.000Z",3],
              ["2019-11-01T12:00:00.000Z",4],
              ["2020-01-01T12:00:00.000Z",5],
              ["2020-01-01T12:00:00.000Z",6],
              ["2020-01-01T12:00:00.000Z",7],
              ["2019-01-01T12:00:00.000Z",8],
              ["2020-01-01T12:00:00.000Z",9],
              ["2020-01-01T12:00:00.000Z",10]])

for a in ['01','02','03','04','05','06','07','08','09','10','11','12']: 
    i = 0
    while i < len(usage_vals):
            if "2019-%s-01T12:00:00.000Z" %(a) in usage_vals[i, 0]:
                usage_vals = np.delete(usage_vals, i, axis=0)
                i = i - 1
            i = i + 1

print(usage_vals)
'''