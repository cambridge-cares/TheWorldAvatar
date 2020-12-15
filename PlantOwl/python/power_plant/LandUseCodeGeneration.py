##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 30 Nov 2020                      #
##########################################

"""This module demonstrates the use of functions defined in ABoxGeneration for representing instances, links between
them and their data"""

from rdflib import Graph, FOAF, URIRef, BNode, Literal
from rdflib.namespace import RDF, RDFS, Namespace
import ABoxGeneration as aboxgen

if __name__ == '__main__':
    """Creates an instance of Graph"""
    g = Graph()
    """Assigns the IRI of the OntoLandUse ontology"""
    land_use_code_ontology_iri = "http://www.theworldavatar.com/ontology/ontolanduse/OntoLandUse.owl#"
    """Binds the IRI with the landusecode literal in the graph"""
    g.bind("landusecode", land_use_code_ontology_iri)
    """Creates the namespace of lucode AC01"""
    instance_iri_lucode_namespace = Namespace("http://www.theworldavatar.com/kb/ontolanduse/AC01.owl#")
    """Assigns the IRI of lucode AC01"""
    instance_iri_lucode = instance_iri_lucode_namespace.LandUseCode_AC01
    """Assigns the name of lucode"""
    instance_name_lucode = "AC01"
    """Creates the namespace of the OntoLandUse ontology"""
    namespace_onto_land_use = Namespace("http://www.theworldavatar.com/ontology/ontolanduse/OntoLandUse.owl#")
    """Creates an instance of lucode"""
    g = aboxgen.create_instance(g, namespace_onto_land_use.LandUseCode, instance_iri_lucode, instance_name_lucode)
    """Assigns the IRI of England"""
    instance_iri_admindiv = "http://www.theworldavatar.com/kb/ontolanduse/England.owl#AdministrativeDivision_England"
    """Creates the namespace of administrative division"""
    instance_iri_admindiv_namespace = Namespace("http://www.theworldavatar.com/kb/ontolanduse/England.owl#")
    """Assigns the name of administrative division"""
    instance_name_admin_div = "England"
    """Creates an instance of administrative division"""
    g = aboxgen.create_instance(g, namespace_onto_land_use.AdministrativeDivision, instance_iri_admindiv, instance_name_admin_div)
    """Links the lucode instance with the administrative division instance"""
    g = aboxgen.link_instance(g, namespace_onto_land_use.usedIn, instance_iri_lucode,
                              instance_iri_admindiv_namespace.AdministrativeDivision_England)
    comment = "test comment for the land use code"
    g = aboxgen.link_data(g, RDFS.comment, instance_iri_lucode_namespace.LandUseCode_AC01, comment)
    abox_file_name = instance_name_lucode + '.owl'
    g.serialize(destination=abox_file_name, format="application/rdf+xml")
