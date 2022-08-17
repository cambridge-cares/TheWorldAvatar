import json
import requests
from requests import status_codes
from datetime import datetime
from typing import List, Tuple
from rdflib import Literal
from rdflib import URIRef
from rdflib import Graph
from rdflib import RDF
from rdflib import XSD

from pyderivationagent.kg_operations.gateway import jpsBaseLibGW

from pyderivationagent.data_model import *

class PySparqlClient:
    def __init__(self, query_endpoint, update_endpoint, kg_user=None, kg_password=None, fs_url=None, fs_user=None, fs_pwd=None) -> None:
        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

        if kg_user is not None:
            self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user, kg_password)
        else:
            self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)

        # Also initialise the fileserver URL and auth info
        # TODO in the future development, make use of pyuploader
        self.fs_url = fs_url
        self.fs_auth = (fs_user, fs_pwd)

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

    def uploadFile(self, local_file_path, filename_with_subdir: str=None) -> Tuple[str, float]:
        """This function uploads the file at the given local file path to file server."""
        if self.fs_url is None or self.fs_auth is None:
            raise Exception("ERROR: Fileserver URL and auth are not provided correctly.")
        with open(local_file_path, 'rb') as file_obj:
            files = {'file': file_obj}
            timestamp_upload, response = datetime.now().timestamp(), requests.post(
                self.fs_url+filename_with_subdir if filename_with_subdir is not None else self.fs_url,
                auth=self.fs_auth, files=files
            )

            # If the upload succeeded, return the remote file path and the timestamp when the file was uploaded
            if (response.status_code == status_codes.codes.OK):
                remote_file_path = response.headers['file']

                return remote_file_path, timestamp_upload
            else:
                raise Exception("ERROR: Local file (%s) upload to file server <%s> failed with code %d and response body: %s" % (
                    local_file_path, self.fs_url, response.status_code, str(response.content)))

    def downloadFile(self, remote_file_path, downloaded_file_path):
        """This function downloads a file given the remote file path and the local file path to store the downloaded file."""
        response = requests.get(remote_file_path, auth=self.fs_auth)
        if (response.status_code == status_codes.codes.OK):
            with open(downloaded_file_path, 'wb') as file_obj:
                for chunk in response.iter_content(chunk_size=128):
                    file_obj.write(chunk)
        else:
            raise Exception("ERROR: File <%s> download failed with code %d and response body: %s" % (
                remote_file_path, response.status_code, str(response.content)))

    def generate_ontoagent_instance(self, service_iri:str, http_url:str, input_types:List[str], output_types:List[str]):
        operation_iri = initialiseInstanceIRI(getNameSpace(service_iri), ONTOAGENT_OPERATION)
        msg_input_iri = initialiseInstanceIRI(getNameSpace(service_iri), ONTOAGENT_MESSAGECONTENT)
        msg_output_iri = initialiseInstanceIRI(getNameSpace(service_iri), ONTOAGENT_MESSAGECONTENT)

        g = Graph()
        g.add((URIRef(service_iri), RDF.type, URIRef(ONTOAGENT_SERVICE)))
        g.add((URIRef(service_iri), URIRef(ONTOAGENT_HASOPERATION), URIRef(operation_iri)))
        g.add((URIRef(operation_iri), RDF.type, URIRef(ONTOAGENT_OPERATION)))
        g.add((URIRef(operation_iri), URIRef(ONTOAGENT_HASINPUT), URIRef(msg_input_iri)))
        g.add((URIRef(operation_iri), URIRef(ONTOAGENT_HASOUTPUT), URIRef(msg_output_iri)))
        g.add((URIRef(operation_iri), URIRef(ONTOAGENT_HASHTTPURL), Literal(http_url, datatype=XSD.anyURI)))

        g.add((URIRef(msg_input_iri), RDF.type, URIRef(ONTOAGENT_MESSAGECONTENT)))
        for each_input in input_types:
            msg_part_iri = initialiseInstanceIRI(getNameSpace(service_iri), ONTOAGENT_MESSAGEPART)
            g.add((URIRef(msg_input_iri), URIRef(ONTOAGENT_HASMANDATORYPART), URIRef(msg_part_iri)))
            g.add((URIRef(msg_part_iri), RDF.type, URIRef(ONTOAGENT_MESSAGEPART)))
            g.add((URIRef(msg_part_iri), URIRef(ONTOAGENT_HASTYPE), URIRef(each_input)))

        g.add((URIRef(msg_output_iri), RDF.type, URIRef(ONTOAGENT_MESSAGECONTENT)))
        for each_output in output_types:
            msg_part_iri = initialiseInstanceIRI(getNameSpace(service_iri), ONTOAGENT_MESSAGEPART)
            g.add((URIRef(msg_output_iri), URIRef(ONTOAGENT_HASMANDATORYPART), URIRef(msg_part_iri)))
            g.add((URIRef(msg_part_iri), RDF.type, URIRef(ONTOAGENT_MESSAGEPART)))
            g.add((URIRef(msg_part_iri), URIRef(ONTOAGENT_HASTYPE), URIRef(each_output)))

        # NOTE SPARQL update with sub-query to ensure one agent service don't get duplicated entries in KG
        # NOTE TODO this implies that ONE AGENT SERVICE ONLY HAS ONE ONTOAGENT:OPERATION
        update = PREFIX_RDF + """INSERT { %s } WHERE { FILTER NOT EXISTS {<%s> rdf:type <%s>.} }""" % (
            g.serialize(format='nt'), service_iri, ONTOAGENT_SERVICE
        )
        self.performUpdate(update)

    def uploadGraph(self, g: Graph):
        update = """INSERT DATA {""" + g.serialize(format='nt') + "}"
        self.performUpdate(update)
