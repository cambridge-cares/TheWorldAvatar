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
    ontologyIRI = sys.argv[1]
    tIRI = sys.argv[2]
    saveIRI = sys.argv[3]
    #ontologyIRI = "D:/workwork/ontoMatchFiles/jpspp.rdf"
    #saveIRI = "D:/workwork/ontoMatchFiles/jpsppcombine.rdf"
    #tIRI = "D:/workwork/testFiles/ontologies/PowerPlant.owl"
    #C:\Users\morta\WebstormProjects\MatchAgentVisual\public\kb\ONTOMATCH\jpspppt.owl
    #saveIRI = "file:///C:/Users/morta/WebstormProjects/MatchAgentVisual/public/kb/ONTOMATCH/jpspppt.owl"
    #ontologyIRI = "D:/workwork/ontoMatchFiles/jpspp.rdf"
    #ontologyIRI = "D:/workwork/ontoMatchFiles/tmpdbp.owl"
    #saveIRI = "D:/workwork/ontoMatchFiles/tmpdbpT.owl"
    #tIRI = "D:/workwork/testFiles/ontologies/dbpedia_2014.owl"
    #load all imports recursively
    ontoObject = Ontology(tIRI)
    imports =ontoObject.imports
    for item in imports:
        print(item)
    imports.append(tIRI)
    imports.append(ontologyIRI)
    try:
        PlusImport(imports,saveIRI)
        print("success")
    except Exception as e:
        print(str(e))

