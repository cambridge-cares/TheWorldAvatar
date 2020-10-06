import sys

import rdflib
import os
from ontologyWrapper import *
class PlusImport():
    '''
    create a temp file using onto and all imports
    a custom class dealing with the problem taht owlready2 does not auto import imported ontology triples
    '''
    def __init__(self, imports, name ='./temp/temp.owl'):
        g = rdflib.Graph()
        for addr in imports:
            print(addr)
            gt = rdflib.Graph()
            if type(addr) is tuple:
                gt.parse(addr[0], format = addr[1])
            else:
                gt.parse(addr)
            g = g+gt
        self.name = name


        g.serialize(destination=self.name, format='xml')




    def close(self):
        os.remove(self.name)

if __name__ == "__main__":
    #ontologyIRI = sys.argv[1]
    #saveIRI = sys.argv[2]
    ontologyIRI = "D:/workwork/ontoMatchFiles/jpspp.rdf"
    saveIRI = "D:/workwork/ontoMatchFiles/jpsppcombine.rdf"
    ontoObject = Ontology(ontologyIRI)
    imports =ontoObject.imports
    importAddrs = [
        "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl",
        "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl",
        "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl",
        "http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl",
        "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl",
        "http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl",
        "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl"]
    for item in imports:
        print(item)
    imports.append(ontologyIRI)
    try:
        PlusImport(imports,saveIRI)
    except Exception:
        print(Exception)

