##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 30 Nov 2020                      #
##########################################

"""This module is designed and developed to allow users to create and link
instances and add data properties to instances."""

from rdflib import Graph, FOAF, URIRef, BNode, Literal
from rdflib.namespace import RDF, RDFS

"""Creates an instance"""
def create_instance(graph, class_iri, instance_iri, instance_name):
    instance = URIRef(instance_iri)
    name = Literal(instance_name)
    graph.add((instance, RDF.type, class_iri))
    graph.add((instance, RDFS.label, name))
    return graph

"""Links a source instance with a target instance"""
def link_instance(graph, object_property, source_instance_iri, target_instance_iri):
    graph.add((source_instance_iri, object_property, target_instance_iri))
    return graph

"""Adds a data property to an instance"""
def link_data(graph, data_property, instance_iri, value):
    data = Literal(value)
    graph.add((instance_iri, data_property, data))
    return graph
