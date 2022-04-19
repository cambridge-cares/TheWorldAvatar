from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF
import json

from pyderivationagent.kg_operations.gateway import jpsBaseLibGW

from pyderivationagent.data_model import *

class PySparqlClient:
    def __init__(self, query_endpoint, update_endpoint, kg_user=None, kg_password=None) -> None:
        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

        if kg_user is not None:
            self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user, kg_password)
        else:
            self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)

    def checkInstanceClass(self, instance, instance_class):
        """
            This method checks if the given instance is instantiated from the given instance class.
            Arguments:
                instance - IRI of an instance
                instance_class - IRI of the instance class to be checked against
        """
        # Delete "<" and ">" around the IRI
        instance = trimIRI(instance)
        instance_class = trimIRI(instance_class)

        # Prepare query string, ignore owl:Thing and owl:NamedIndividual
        query = PREFIX_RDFS + \
                PREFIX_RDF + \
                PREFIX_XSD + \
                PREFIX_OWL + \
                """SELECT ?result \
                WHERE { <%s> rdf:type ?type . \
                FILTER(?type != owl:Thing && ?type != owl:NamedIndividual) . \
                BIND(xsd:boolean(if(?type = <%s>, "true", "false")) as ?result)  \
                }""" % (instance, instance_class)

        # Perform query
        response = self.performQuery(query)
        res = [list(r.values())[0] for r in response]
        if res[0] == 'true':
            return True
        else:
            return False

    def getAmountOfTriples(self):
        return self.kg_client.getTotalNumberOfTriples() # return an integer of total number of triples

    def performQuery(self, query):
        """
            This function performs query to knowledge graph.
            Arguments:
                query - SPARQL Query string
        """
        response = self.kg_client.execute(query)
        return json.loads(response)

    def performUpdate(self, update):
        """
            This function performs SPARQL Update to knowledge graph.
            Arguments:
                update - SPARQL Update string
        """
        self.kg_client.executeUpdate(update)

    def uploadOntology(self, filePath):
        """
            This function uploads ontology to knowledge graph.
            Arguments:
                filePath - the file path of ontology to be uploaded
        """
        javaFile = self.jpsBaseLib_view.java.io.File(filePath)
        self.kg_client.uploadFile(javaFile)
