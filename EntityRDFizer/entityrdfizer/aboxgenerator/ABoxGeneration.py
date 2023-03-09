##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 30 Nov 2020                      #
##########################################

"""This module is designed and developed to allow users to create and link
instances and add data properties to instances."""

from rdflib import URIRef, Literal
from rdflib.namespace import RDF, RDFS, XSD

"""Creates an instance"""
def create_instance(graph, class_iri, instance_iri, instance_name):
    instance = URIRef(instance_iri)
    graph.add((instance, RDF.type, class_iri))
    link_data_with_type(graph, RDFS.label, instance, instance_name, XSD.string)
    return graph

"""Creates an instance"""
def create_instance_without_name(graph, class_iri, instance_iri):
    instance = URIRef(instance_iri)
    graph.add((instance, RDF.type, class_iri))
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

"""Adds a data property including the data type to an instance"""
def link_data_with_type(graph, data_property, instance_iri, value, data_type):
    data = Literal(value, datatype=data_type)
    graph.add((instance_iri, data_property, data))
    return graph
