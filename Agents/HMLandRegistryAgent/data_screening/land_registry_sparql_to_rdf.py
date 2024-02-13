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
postcode = 'PE30 2LB'
query1 = '''prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           prefix owl: <http://www.w3.org/2002/07/owl#>
           prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           prefix sr: <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/>
           prefix ukhpi: <http://landregistry.data.gov.uk/def/ukhpi/>
           prefix lrppi: <http://landregistry.data.gov.uk/def/ppi/>
           prefix skos: <http://www.w3.org/2004/02/skos/core#>
           prefix lrcommon: <http://landregistry.data.gov.uk/def/common/>
           
           SELECT ?addr (COUNT(?transx) as ?txCount)
           WHERE
           {
           VALUES ?postcode {"%s"^^xsd:string}

           ?addr lrcommon:postcode ?postcode.

           ?transx lrppi:propertyAddress ?addr .
           }
           GROUP BY ?addr
''' % postcode 

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
               ?transx ?p ?o
           }
           WHERE
           {
           # property with one transaction record
           #?transx lrppi:propertyAddress <http://landregistry.data.gov.uk/data/ppi/address/530e5441a706619e28739cd740cbf2999f5ad06a> ;
           # property with several transaction records
           ?transx lrppi:propertyAddress <http://landregistry.data.gov.uk/data/ppi/address/3128914fd3c2934e3bea11c72a52a456aa7b604e> ;
                   ?p ?o .
           }
           '''


#####################################################################################

# Retrieve SPARQL results
sparql = SPARQLWrapper(endpoint)
sparql.setQuery(query1)
sparql.setReturnFormat(JSON)
results = sparql.query().convert()
pprint.pprint(results)

# Construct RDF graph
sparql = SPARQLWrapper(endpoint)
sparql.setQuery(query2)
results = sparql.queryAndConvert()
results.serialize('./output/txns.ttl', format="ttl")
