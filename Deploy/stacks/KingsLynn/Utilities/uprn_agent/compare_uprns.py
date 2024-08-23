################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Aug 2022                            #
################################################

# This module retrieves the old UPRNs (i.e. instantiated using static data in
# FME workflow) with new UPRNs (i.e. instantiated using the UPRN agent) and
# compares their alignment

import json
import requests
from deepdiff import DeepDiff
from SPARQLWrapper import SPARQLWrapper, JSON


# Specify SPARQL endpoint to Blazegraph namespace
blazegraph = "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql/"
# Specify initial Blazegraph namespace (i.e. used for instantiation of OntoCityGml
# used to prefix named graph IRIs and (newly) instantiated object IRIs)
namespace = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"


def get_fme_uprns(endpoint):
    """
        Retrieve UPRNs as instantiated way back from static OS download 
        using FME workflow

        Returns dict of {'bldgIRI': set of UPRNs}
    """
    sparql = SPARQLWrapper(endpoint)
    sparql.setReturnFormat(JSON)
    query_string = f"""
    PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> 
    PREFIX osid: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#>

    SELECT DISTINCT ?bldg ?cityobj ?uprns

    WHERE {{ 
    GRAPH <{namespace}building/>                             
            {{ ?bldg ocgml:objectClassId 26 .
            BIND(IRI(REPLACE(str(?bldg), "building", "cityobject")) AS ?cityobj) 
            }}
            {{ # UPRNs from FME workflow
            SELECT DISTINCT ?cityobj ?uprns
            WHERE {{
                GRAPH <{namespace}cityobjectgenericattrib/>
                    {{ OPTIONAL {{
                        ?attr ocgml:attrName "OS_UPRNs" ;
                            ocgml:cityObjectId ?cityobj ;
                            ocgml:strVal ?uprns .
                        }}
                    }}
                }}
            }}
    }}
    """
    sparql.setQuery(query_string)
    results = sparql.query().convert()

    # Create dict of building IRIs and set of UPRNs
    res = {r['bldg']['value']: set() if not r.get('uprns') else 
                               set(r['uprns']['value'].split(','))
                               for r in results['results']['bindings']}
    return res


def get_agent_uprns(endpoint):
    """
        Retrieve UPRNs as instantiated using UPRN agent

        Returns dict of {'bldgIRI': set of UPRNs}
    """
    sparql = SPARQLWrapper(endpoint)
    sparql.setReturnFormat(JSON)
    query_string = f"""
    PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> 
    PREFIX osid: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#>

    SELECT DISTINCT ?bldg ?cityobj ?uprns

    WHERE {{ 
    GRAPH <{namespace}building/>                             
            {{ ?bldg ocgml:objectClassId 26 .
            BIND(IRI(REPLACE(str(?bldg), "building", "cityobject")) AS ?cityobj) 
            }}
            {{ # UPRNs from UPRN Agent
            SELECT DISTINCT ?cityobj ?uprns
            WHERE {{
                GRAPH <{namespace}identifiers>
                    {{ OPTIONAL {{
                        ?cityobj ^osid:intersectsFeature/osid:hasValue ?uprns
                        }}
                    }}
                }}
            }}
    }}
    """
    sparql.setQuery(query_string)
    results = sparql.query().convert()

    # Create dict of building IRIs and set of UPRNs
    bldgs = [r['bldg']['value'] for r in results['results']['bindings']]
    bldgs = set(bldgs)
    res = {b: set() for b in bldgs}
    for r in results['results']['bindings']:
        if r.get('uprns'):
            res[r['bldg']['value']].add(r['uprns']['value'])
    return res


if __name__ == '__main__':

    # Get dictionary with old FME UPRNs
    old_uprns = get_fme_uprns(blazegraph)

    # Get dictionary with new UPRN agent UPRNs
    new_uprns = get_agent_uprns(blazegraph)

    # Analyse results
    print('\nNumber of buildings in data sets:')
    print(f'Old data set: {len(old_uprns):>6}')
    print(f'New data set: {len(new_uprns):>6}')

    print('\nNumber of buildings with UPRNs:')
    bo = [b for b in old_uprns.values() if len(b)!=0]
    bn = [b for b in new_uprns.values() if len(b)!=0]
    print(f'Old data set: {len(bo):>6}')
    print(f'New data set: {len(bn):>6}')

    uprnsn = set().union(*bn)
    print(f'\nNumber of new UPRNs: {len(uprnsn):>6}\n')
    
    print('##########  Discrepancy analysis  ##########')
    # Rund DeepDiff on results
    ddiff = DeepDiff(old_uprns, new_uprns, 
                     ignore_order=True, ignore_string_case=True)

    print('\nNumber of UPRN discrepancies between data sets:')
    only_old = ddiff['set_item_removed']
    only_new = ddiff['set_item_added']
    print(f'Only in old data set: {len(only_old):>6}')
    print(f'Only in new data set: {len(only_new):>6}')

    print('\nNumber of building discrepancies between data sets:')
    only_old = [o[o.find('http'):] for o in only_old]
    only_old = [o[:o.find('\'][')] for o in only_old]
    only_old = set(only_old)
    only_new = [o[o.find('http'):] for o in only_new]
    only_new = [o[:o.find('\'][')] for o in only_new]
    only_new = set(only_new)
    print(f'Only in old data set: {len(only_old):>6}')
    print(f'Only in new data set: {len(only_new):>6}')

    print('Done')
