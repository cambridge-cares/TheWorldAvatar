###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 23 Jan 2022                           #
###############################################

""" 
This module investigates the capabilities of the HM Land Registry Open Data
SPARQL endpoint
https://landregistry.data.gov.uk/
"""

import pandas as pd
from SPARQLWrapper import SPARQLWrapper, JSON


# SPARQL query endpoint
endpoint = 'http://landregistry.data.gov.uk/landregistry/query'

# Example query (following example provided here:
# https://landregistry.data.gov.uk/app/qonsole#)
postcode = 'PE30 1BL'
query = '''prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           prefix owl: <http://www.w3.org/2002/07/owl#>
           prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           prefix sr: <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/>
           prefix ukhpi: <http://landregistry.data.gov.uk/def/ukhpi/>
           prefix lrppi: <http://landregistry.data.gov.uk/def/ppi/>
           prefix skos: <http://www.w3.org/2004/02/skos/core#>
           prefix lrcommon: <http://landregistry.data.gov.uk/def/common/>

           SELECT ?paon ?saon ?street ?town ?county ?postcode ?amount ?date ?category
           WHERE
           {
           VALUES ?postcode {"%s"^^xsd:string}

           ?addr lrcommon:postcode ?postcode.

           ?transx lrppi:propertyAddress ?addr ;
                   lrppi:pricePaid ?amount ;
                   lrppi:transactionDate ?date ;
                   lrppi:transactionCategory/skos:prefLabel ?category.

           OPTIONAL {?addr lrcommon:county ?county}
           OPTIONAL {?addr lrcommon:paon ?paon}
           OPTIONAL {?addr lrcommon:saon ?saon}
           OPTIONAL {?addr lrcommon:street ?street}
           OPTIONAL {?addr lrcommon:town ?town}
           }
           ORDER BY ?amount
''' % postcode 

#####################################################################################

# Retrieve SPARQL results
sparql = SPARQLWrapper(endpoint)
sparql.setQuery(query)
sparql.setReturnFormat(JSON)
results = sparql.query().convert()

# Extract all transactions into DataFrame
tx_data = pd.DataFrame(index=results['head']['vars'])
cols = ['tx_{}'.format(i+1) for i in range(len(results['results']['bindings']))]
for i in range(len(cols)):
    tx_data[cols[i]] = tx_data.index.map(results['results']['bindings'][i])
# Extract values from dictionaries in DataFrame if not NaN
tx_data = tx_data.applymap(lambda x: x['value'] if isinstance(x, dict) else x)

tx_data.to_csv('./data/housing_market.csv')