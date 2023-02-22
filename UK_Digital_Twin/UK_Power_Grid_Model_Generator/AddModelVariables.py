##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 29 June 2022        #
##########################################

"""This module defines the function of adding the model veriables to UK power grid knowledge graph -- EGen & EGen cost, EBus and ELine"""
import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import owlready2
from rdflib import URIRef, Literal
from rdflib.namespace import RDF
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
import uuid
from logging import raiseExceptions
import urllib.request

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""T-Box URI"""
if urllib.request.urlopen("http://www.theworldavatar.com/ontology/").getcode() == 200:
    ontocape_upper_level_system = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
    ontocape_mathematical_model = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
    ontocape_network_system = owlready2.get_ontology(t_box.ontocape_network_system).load()
    # ontopowsys = owlready2.get_ontology(t_box.ontopowsys_OntoPowSys).load()
else:
    print('---THE WORLD AVATAR NOT FOUND---')


def AddModelVariable(graph, root_node, namespace, varKey, varValue, unit, varType, inputOrOutput:bool):

    if type(inputOrOutput) is not bool:
            raiseExceptions("inputOrOutput has to be a bool number")
    # parameter iri
    var_iri = namespace + varKey + str(uuid.uuid4())
    value_var_iri = namespace + UK_PG.ModelVariableSpecificationKey + str(uuid.uuid4())
    # add var node and type
    if inputOrOutput is True:
        graph.add((URIRef(root_node), URIRef(ontocape_network_system.hasInput.iri), URIRef(var_iri)))
    else:
        graph.add((URIRef(root_node), URIRef(ontocape_network_system.hasOutput.iri), URIRef(var_iri)))

    graph.add((URIRef(var_iri), RDF.type, URIRef(varType)))
    #add var value
    graph.add((URIRef(var_iri), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_var_iri)))
    graph.add((URIRef(value_var_iri), RDF.type, URIRef(ontocape_mathematical_model.ModelVariableSpecification.iri)))
    graph.set((URIRef(value_var_iri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(varValue)))
    if unit != None:
        graph.add((URIRef(value_var_iri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(unit)))
    return graph, var_iri