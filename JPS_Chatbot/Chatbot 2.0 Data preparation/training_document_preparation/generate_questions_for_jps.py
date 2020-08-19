# Created by Xiaochi Zhou (xz378)

# To experimentally use ontology inference and reasoning to extend the range of question generation

# A new python Owlready2, which presumably
#   1. DFT questions , ontocompchem
# OntoCompChem will be the focus of this project
from pprint import pprint

from owlready2 import *
import rdflib

ontocompchem_address = 'http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl'

g = rdflib.Graph()
g.parse(ontocompchem_address)
onto = get_ontology("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl")

# make an analysis on the content of ontocompchem, focusing on the class hierarchy
#

qres = g.query(
    """SELECT DISTINCT ?class
       WHERE {
          ?class a owl:Class . 
       }""")

all_classes = []
root_classes = []

for row in qres:
    # print("%s" % row)
    all_classes.append(str(row['class']))

# pprint(all_classes)
# iterate through the classes, find out those who are not a subclass of anything

for class_uri in all_classes:
    qres = g.query(
        """SELECT DISTINCT ?root_class 
           WHERE {
              <%s> rdfs:subClassOf ?root_class  . 
           }""" % class_uri)

    for binding in (qres):
        uri = str(binding['root_class'])
        # print(uri)
        if uri.strip() == 'http://www.w3.org/2002/07/owl#Thing':
            print('This is a root class', class_uri)
            root_classes.append(class_uri)
        # print('=======================================')

# ================= find all the subclasses of those root classes =======
print('========================= subclasses =============================')


# extract a hierarchy tree recursively.


def find_sub_classes(target_root_classes):
    for root_uri in target_root_classes:
        qres = g.query(
            """SELECT DISTINCT ?sub_class 
               WHERE {
                  ?sub_class rdfs:subClassOf   <%s>. 
               }""" % root_uri)
        sub_classes_list = []
        print('================ a batch ===========')
        for binding in qres:
            sub_class_uri = str(binding['sub_class'])
            sub_classes_list.append(sub_class_uri)
            print(sub_class_uri, 'of', root_uri)
        find_sub_classes(sub_classes_list)

find_sub_classes(root_classes)
