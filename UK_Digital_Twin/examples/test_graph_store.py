"""Test OWLReady2 working with RDFLib"""
#import owlready2
from owlready2 import *
# from rdflib import Graph, URIRef 
# from rdflib.namespace import RDF

# RDFLib Graph store: sleepcat

# graph = Graph(store='Sleepycat', identifier = URIRef('http://example.org/donna'))

# graph.open('C:\\Users\\wx243\\Documents\\myRDFLibStore', create = True)

# graph.add((URIRef('http://example.org/donna'), RDF.type, URIRef('http://example.org/donna')))

# uri = URIRef('http://example.org/donna')
# uri = uri.__add__('a')

# uri.__invert__

# print(uri)

# graph.close()
filepath = 'C:\\Users\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\UK_energy_consumption_Wales_UK.owl'
testOnto = get_ontology(filepath)# .load()

# test_save_path = 'C:\\Users\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\test_1.owl'


#onto = get_ontology('http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_energy_consumption/energyConsumptionIn2017/Wales.owl')

# class Drug(Thing):
#    namespace = onto

# my_drug = Drug("my_drug")

default_world.set_backend(filename = "C:\\Users\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\file_3.sqlite3",\
                          exclusive = False)


if __name__ == '__main__':
    # testOnto.save(file = test_save_path, format = 'rdfxml')
    default_world.save()