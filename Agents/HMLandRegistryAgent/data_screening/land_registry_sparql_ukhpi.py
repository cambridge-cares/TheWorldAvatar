###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 23 Jan 2022                           #
###############################################

""" 
This module investigates the capabilities of the HM Land Registry Open Data
SPARQL endpoint
https://landregistry.data.gov.uk/
"""

import pprint
from SPARQLWrapper import SPARQLWrapper, JSON


# SPARQL query endpoint
endpoint = 'http://landregistry.data.gov.uk/landregistry/query'

# Example query (following example provided here:
# https://landregistry.data.gov.uk/app/qonsole#)
query1 = '''prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           prefix owl: <http://www.w3.org/2002/07/owl#>
           prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           prefix sr: <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/>
           prefix ukhpi: <http://landregistry.data.gov.uk/def/ukhpi/>
           prefix lrppi: <http://landregistry.data.gov.uk/def/ppi/>
           prefix skos: <http://www.w3.org/2004/02/skos/core#>
           prefix lrcommon: <http://landregistry.data.gov.uk/def/common/>
           
           SELECT DISTINCT ?region
           WHERE
           {
               ?region a ukhpi:Region
           }
          '''

query2 = '''prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           prefix owl: <http://www.w3.org/2002/07/owl#>
           prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           prefix sr: <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/>
           prefix ukhpi: <http://landregistry.data.gov.uk/def/ukhpi/>
           prefix lrppi: <http://landregistry.data.gov.uk/def/ppi/>
           prefix skos: <http://www.w3.org/2004/02/skos/core#>
           prefix lrcommon: <http://landregistry.data.gov.uk/def/common/>
           
           CONSTRUCT 
           {
               ?indices ?p ?o
           }
           WHERE
           {
            ?indices ukhpi:refRegion <http://landregistry.data.gov.uk/id/region/king's-lynn-and-west-norfolk> ;
                     a ukhpi:MonthlyIndicesByRegion ;
                     ?p ?o
           }
           '''

#####################################################################################

# Get regions with published UK HPI data
sparql = SPARQLWrapper(endpoint)
sparql.setQuery(query1)
sparql.setReturnFormat(JSON)
results = sparql.query().convert()
pprint.pprint(results)

# Construct UKHPI graph
sparql = SPARQLWrapper(endpoint)
sparql.setQuery(query2)
results = sparql.queryAndConvert()
results.serialize('./output/ukhpi.ttl', format="ttl")