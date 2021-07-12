##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 May 2021          #
##########################################

"""This module defines the function of adding the model veriables to UK power grid knowledge graph -- EGen & EGen cost, EBus and ELine"""
import os
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import owlready2
from rdflib import URIRef, Literal
from rdflib.namespace import RDF
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""T-Box URI"""
ontocape_upper_level_system = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_mathematical_model = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()


def AddModelVariable(graph, root_node, namespace, node_locator, varKey, varValue, unit, *varType):
    # parameter iri
    var_iri = namespace + varKey + node_locator
    value_var_iri = namespace + UK_PG.valueKey + varKey + node_locator
    # add var node and type
    graph.add((URIRef(root_node), URIRef(ontocape_mathematical_model.hasModelVariable.iri), URIRef(var_iri)))
    for type_ in varType:
        graph.add((URIRef(var_iri), RDF.type, URIRef(type_)))
    #add var value
    graph.add((URIRef(var_iri), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_var_iri)))
    graph.add((URIRef(value_var_iri), RDF.type, URIRef(ontocape_mathematical_model.ModelVariableSpecification.iri)))
    graph.set((URIRef(value_var_iri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(varValue)))
    if unit != None:
        graph.add((URIRef(value_var_iri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(unit)))
    return graph