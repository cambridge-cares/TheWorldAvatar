import json
import os

from SPARQLWrapper import SPARQLWrapper, JSON
from .SPARQLWarehouse import ONTOSPECIES_GET_SMILES
from .location import JPS_MODELS_DIR, JPS_QUERY_DIR, AGENT_QUERY_DIR
from .Lookup import find_nearest_match


class OntoSpecies:
    def __init__(self):
        with open(os.path.join(JPS_QUERY_DIR, 'JPS_DICTS', 'ONTOSPECIES_URI_DICT')) as f:
            self.dict = json.loads(f.read())

        with open(os.path.join(JPS_QUERY_DIR, 'JPS_DICTS', 'ONTOSPECIES_KEYS')) as f:
            self.keys = json.loads(f.read())
        with open('OntoSpeciesLog', 'w') as f:
            f.write('')
            f.close()
        self.dictionary = {}
        mappings = open(os.path.join(AGENT_QUERY_DIR, 'cc.txt')).readlines()[1:]
        for m in mappings:
            old, new = m.split(',')
            self.dictionary[old.strip()] = new.strip()

    def findSMILES(self, IRI):
        print('IRI IN FIND SMILES', IRI)
        if type(IRI) == type([]):
            IRI = IRI[0]

        if IRI in self.dictionary:
            IRI = self.dictionary[IRI]
        query = ONTOSPECIES_GET_SMILES % (IRI)
        SMILES = []
        namespace = "ontospecies"
        sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()

        for result in results["results"]["bindings"]:
            SMILES.append(result['SMILES']['value'])
        if len(SMILES) > 0:
            return SMILES[0]
        else:
            return ''

    def find_IRI(self, _key):
        if _key is None:
            return None
        return self.dict[_key]

    def findOntoSpecies(self, species):
        _key, _score = find_nearest_match(species, self.keys)
        with open('OntoSpeciesLog', 'a') as f:
            f.write(json.dumps({'species': species, 'key': _key, 'score': _score}))
            f.close()
        _IRI = self.find_IRI(_key)
        if len(_IRI) > 1:
            return _IRI[0]
        else:
            return _IRI

# osc = OntoSpecies()
# IRIS = osc.findOntoSpecies('CO2')
# for i in IRIS:
#     smiles = osc.findSMILES(i)
#     print(smiles)
