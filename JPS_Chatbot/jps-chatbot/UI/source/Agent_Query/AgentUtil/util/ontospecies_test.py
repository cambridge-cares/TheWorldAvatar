import json
from pprint import pprint

from SPARQLWrapper import SPARQLWrapper, JSON
from SPARQLWarehouse import GET_VALID_ONTOSPECIES_IRI, ONTOSPECIES_GET_SMILES, ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY, \
    ONTOSPECIES_GET_ALTLABEL, ONTOSPECIES_GET_LABEL, ONTOSPECIES_GET_INCHIS

dictionary = {}
mappings = open('cc.csv').readlines()[1:]
for m in mappings:
    old, new = m.split(',')
    dictionary[new.strip()] = old.strip()


def query_blazegraph(query, namespace):
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results


species_IRI = [s['species']['value'] for s in
               query_blazegraph(GET_VALID_ONTOSPECIES_IRI, 'ontocompchem')['results']['bindings']]


# ONTOSPECIES_GET_ALTLABEL

def get_label(key, query, s_iri):
    query = query % s_iri
    rst = query_blazegraph(query, 'ontospecies')['results']['bindings']
    r = [b[key]['value'] for b in rst]
    return r


smiles = []
altlabels = []
labels = []
inchis = []

print(len(species_IRI))
for s in species_IRI:
    if s in dictionary:
        print(s)
        s = dictionary[s]
        smiles = smiles + get_label('SMILES', ONTOSPECIES_GET_SMILES, s)
        altlabels = altlabels + get_label('alt_label', ONTOSPECIES_GET_ALTLABEL, s)
        labels = labels + get_label('label', ONTOSPECIES_GET_LABEL, s)
        inchis = inchis + get_label('inchi', ONTOSPECIES_GET_INCHIS,s)
        print(inchis)
    else:
        pass


super_list = smiles + altlabels + labels + inchis
super_list = list(set(super_list))

with open('super_list', 'w') as f:
    f.write(json.dumps(super_list))
    f.close()

# get all the valid species

# q = ONTOCOMPCHEM_IRI_FROM_ONTOSPECIES_QUERY % species_IRI[0]
# r = query_blazegraph(q, 'ontocompchem')
# pprint(r)
