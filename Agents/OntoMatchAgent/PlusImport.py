import rdflib
import os
class PlusImport():
    '''
    create a temp file using onto and all imports
    a custom class dealing with the problem taht owlready2 does not auto import imported ontology triples
    '''
    def __init__(self, imports, name ='./temp/temp.owl'):
        g = rdflib.Graph()
        for addr in imports:
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