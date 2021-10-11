import json
import os
import urllib

from rapidfuzz import process, fuzz

from .location import JPS_DICT_DIR

ONTOCOMPCHEM_IRI_QUERY = '''

PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX oc:  <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#>
        PREFIX os:  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX gc:  <http://purl.org/gc/>
        SELECT DISTINCT ?ocIRI 
        WHERE {
            ?ocIRI oc:hasUniqueSpecies <%s> ;
                gc:isCalculationOn ?geomOpt ;
                gc:isCalculationOn ?geomType ;
                oc:hasInitialization ?init .
            ?init a oc:InitializationModule ;
                gc:hasParameter ?method ;
                gc:hasParameter ?basisSet .
            ?geomOpt a gc:GeometryOptimization ;
                       gc:hasMolecule ?mol .
            ?mol oc:hasSpinMultiplicity ?spin_mult .
            ?geomType a oc:GeometryType .
            ?geomType oc:hasGeometryType ?geomTypeValue .
            OPTIONAL {
                ?ocIRI gc:isCalculationOn ?vibAnal ;
                    gc:isCalculationOn ?rotConsts ;
                    gc:isCalculationOn ?rotSym .
                ?vibAnal a gc:VibrationalAnalysis ;
                        gc:hasResult ?freqResult .
                ?freqResult oc:hasFrequencies ?frequencies .
                ?rotConsts a oc:RotationalConstants ;
                        oc:hasRotationalConstants ?rot_constants .
                ?rotSym a oc:RotationalSymmetry ;
                        oc:hasRotationalSymmetryNumber ?sym_number .
            }
        }
'''



class ThermoAgent:
    def __init__(self):
        with open(os.path.join(JPS_DICT_DIR, 'ONTOSPECIES_URI_DICT')) as f:
            self.dict = json.loads(f.read())

        with open(os.path.join(JPS_DICT_DIR, 'ONTOSPECIES_KEYS')) as f:
            self.keys = json.loads(f.read())

    def unitConversion(self, ):
        pass

    def find_nearest_match(self, species, KEYS):
        species = species.strip()
        rst = process.extractOne(species, KEYS, scorer=fuzz.ratio)
        key = rst[0]
        score = rst[1]
        return key, score

    def find_IRI(self, _key):
        return self.dict[_key]

    def findOntoSpecies(self, species):
        _key, _score = self.find_nearest_match(species,self.keys)
        _IRI = self.find_IRI(_key)
        return _IRI

    def callThermoAgent(self, species=None, attribute=None, temperature=None, pressure=None):
        # OntoSpecies_IRI = self.findOntoSpecies(species)
        return self.makeHTTP()
        # temperature in K
        # pressure    in Pa

    def find_ontocompchem_IRI(self, OntoSpecies_IRI):
        # TODO: use SPARQL to find the ocIRI
        pass


    def request(self, input_map):
        url = input_map['url']
        url += "?"
        parameters = []
        for key in input_map['input_map']:
            value = input_map['input_map'][key]
            if type(value) == type([]):
                value = key + '=[' + ','.join(value) + ']'
            else:
                value = key + '=' + value

            parameters.append(value)

        parameter_string = '&'.join(parameters)
        full_url = url + parameter_string
        print(full_url)
        req = urllib.request.Request(full_url)
        rst = urllib.request.urlopen(req).read()
        return rst

    def makeHTTP(self, species=None, attribute=None, temperature=None, pressure=None):
        url = 'http://kg.cmclinnovations.com:81/stdc-agent/' + 'api/thermoagent/calculate'
        ontocompchem_IRI = 'http://www.theworldavatar.com/kb/ontocompchem/G09_85949b34-1d08-4287-b2d2-5748933797ce'
        ontospecies_IRI = 'http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000'
        data = {'ontocompchem_IRI': ontocompchem_IRI,
                   'ontospecies_IRI':ontospecies_IRI,
                   'temperature': '300',
                   'pressure': '10000'}

        _input_map = {'url': url, 'input_map': data}

        return self.request(_input_map)
        # TODO: construct the HTTP request
# species = 'inchi=1s/c2h6o/c1-2-3/h3h,2h2,1h3'
# findOntoSpecies(species)
if __name__ == '__main__':
    ta = ThermoAgent()
    response = ta.callThermoAgent()
    print(response)