from pprint import pprint

from owlready2 import *
ontoagent_tbox = 'http://www.theworldavatar.com/ontology/ontoagent/MSM.owl'
onto = get_ontology(ontoagent_tbox)
onto.load()

pprint(onto.Service)
pprint(onto.Operation)
pprint(onto.MessagePart)
pprint(onto.MessageContent)
pprint(onto.hasOutput)
pprint(onto.hasInput)
pprint(onto.hasHttpUrl)
pprint(onto.isArray)
pprint(onto.hasName)

