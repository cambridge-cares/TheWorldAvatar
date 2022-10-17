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


query_all_postcode = '''
    prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    prefix owl: <http://www.w3.org/2002/07/owl#>
    prefix xsd: <http://www.w3.org/2001/XMLSchema#>
    prefix sr: <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/>
    prefix ukhpi: <http://landregistry.data.gov.uk/def/ukhpi/>
    prefix ppd: <http://landregistry.data.gov.uk/def/ppi/>
    prefix skos: <http://www.w3.org/2004/02/skos/core#>
    prefix lrcommon: <http://landregistry.data.gov.uk/def/common/>

    SELECT distinct ?pc
    WHERE
    { ?tx rdf:type ppd:TransactionRecord ;
            ppd:propertyAddress ?addr .
    ?addr lrcommon:district "KING'S LYNN AND WEST NORFOLK" ;
            lrcommon:postcode ?pc
    }
'''

# Example query (following example provided here:
# https://landregistry.data.gov.uk/app/qonsole#)

def get_data_for_postcodes(postcodes):
    
    # Create list of postcodes of interest
    values = '", "'.join(postcodes)
    values = values.replace(',', '^^xsd:string')
    values = f'"{values}"^^xsd:string'
    
    query = f'''
        prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        prefix owl: <http://www.w3.org/2002/07/owl#>
        prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        prefix sr: <http://data.ordnancesurvey.co.uk/ontology/spatialrelations/>
        prefix ukhpi: <http://landregistry.data.gov.uk/def/ukhpi/>
        prefix lrppi: <http://landregistry.data.gov.uk/def/ppi/>
        prefix skos: <http://www.w3.org/2004/02/skos/core#>
        prefix lrcommon: <http://landregistry.data.gov.uk/def/common/>

        SELECT *
        WHERE
        {{
        VALUES ?postcode {{ {values} }}

        ?addr lrcommon:postcode ?postcode.

        ?transx lrppi:propertyAddress ?addr ;
                lrppi:pricePaid ?amount ;
                lrppi:transactionDate ?date ;
                lrppi:propertyType/rdfs:label ?property ;
                lrppi:transactionCategory/skos:prefLabel ?category.
        OPTIONAL {{?addr lrcommon:paon ?paon}}
        OPTIONAL {{?addr lrcommon:saon ?saon}}
        OPTIONAL {{?addr lrcommon:street ?street}}
        OPTIONAL {{?addr lrcommon:town ?town}}
        OPTIONAL {{?addr lrcommon:district ?district}}
        OPTIONAL {{?addr lrcommon:county ?county}}
        }}
        ORDER BY ?amount
    '''
    return query

#####################################################################################

sparql = SPARQLWrapper(endpoint)

# Retrieve postcodes from the Land Registry
sparql.setQuery(query_all_postcode)
sparql.setReturnFormat(JSON)
results = sparql.query().convert()
postcodes = [r['pc']['value'] for r in results['results']['bindings']]

# Split into chunks of 100
n = 100
postcodes = [postcodes[i:i + n] for i in range(0, len(postcodes), n)]

results = []
for pc in postcodes:
    # Retrieve data for each postcodes
    query = get_data_for_postcodes(pc)
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    res = sparql.query().convert()

    r = res['results']['bindings']
    results.extend(r)

# Extract all transactions into DataFrame
tx_data = pd.DataFrame(columns=res['head']['vars'], data=results)
# Extract values from dictionaries in DataFrame if not NaN
tx_data = tx_data.applymap(lambda x: x['value'] if isinstance(x, dict) else x)

tx_data.to_csv('./input/housing_market.csv')