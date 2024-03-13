import json
import requests
from datetime import datetime
from typing import Tuple, Dict, Any, Set
from rdflib import Graph, Literal

from py4jps.kg_operations.gateway import jpsBaseLibGW

from py4jps.data_model import utils
from py4jps.data_model.iris import PREFIX_RDFS, PREFIX_RDF, PREFIX_XSD, PREFIX_OWL

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

        # Expose query and update endpoint
        self.query_endpoint = query_endpoint
        self.update_endpoint = update_endpoint

        # Also initialise the fileserver URL and auth info
        # TODO in the future development, make use of pyuploader
        self.fs_url = fs_url
        self.fs_auth = (fs_user, fs_pwd)

    def check_instance_class(self, instance, instance_class):
        """
            This method checks if the given instance is instantiated from the given instance class.
            Arguments:
                instance - IRI of an instance
                instance_class - IRI of the instance class to be checked against
        """
        # Delete "<" and ">" around the IRI
        instance = utils.trim_iri(instance)
        instance_class = utils.trim_iri(instance_class)

        # Prepare query string, ignore owl:Thing and owl:NamedIndividual
        query = f"""{PREFIX_RDFS} {PREFIX_RDF} {PREFIX_XSD} {PREFIX_OWL}
                SELECT ?result
                WHERE {{ <{instance}> rdf:type ?type .
                    FILTER(?type != owl:Thing && ?type != owl:NamedIndividual) .
                    BIND(xsd:boolean(if(?type = <{instance_class}>, "true", "false")) as ?result)
                }}"""

        # Perform query
        response = self.perform_query(query)

        res = [list(r.values())[0] for r in response]
        if res[0] == 'true':
            return True
        else:
            return False

    def get_amount_of_triples(self):
        # return an integer of total number of triples
        return self.kg_client.getTotalNumberOfTriples()

    def perform_query(self, query):
        """
            This function performs query to knowledge graph.
            Arguments:
                query - SPARQL Query string
        """
        response = str(self.kg_client.executeQuery(query))
        return json.loads(response)

    def perform_update(self, update):
        """
            This function performs SPARQL Update to knowledge graph.
            Arguments:
                update - SPARQL Update string
        """
        self.kg_client.executeUpdate(update)

    def get_all_instances_of_class(self, class_iri):
        """
            This function returns all instances of the given class.
            Arguments:
                class_iri - IRI of the class
        """
        # Prepare query string
        query = f"""SELECT ?iri WHERE {{ ?iri a <{class_iri}> }}"""

        # Perform query
        response = self.perform_query(query)

        return [list(r.values())[0] for r in response]

    def upload_ontology(self, file_path):
        """
            This function uploads ontology to knowledge graph.
            Arguments:
                filePath - the file path of ontology to be uploaded
        """
        javaFile = self.jpsBaseLib_view.java.io.File(file_path)
        self.kg_client.uploadFile(javaFile)

    def upload_file(self, local_file_path, filename_with_subdir: str=None) -> Tuple[str, float]:
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
            if (response.status_code == requests.status_codes.codes.OK):
                remote_file_path = response.headers['file']

                return remote_file_path, timestamp_upload
            else:
                raise Exception(f"ERROR: Local file ({local_file_path}) upload to file server <{self.fs_url}> failed with code {response.status_code} and response body: {str(response.content)}")

    def download_file(self, remote_file_path, downloaded_file_path):
        """This function downloads a file given the remote file path and the local file path to store the downloaded file."""
        response = requests.get(remote_file_path, auth=self.fs_auth)
        if (response.status_code == requests.status_codes.codes.OK):
            with open(downloaded_file_path, 'wb') as file_obj:
                for chunk in response.iter_content(chunk_size=128):
                    file_obj.write(chunk)
        else:
            raise Exception(f"ERROR: File <{remote_file_path}> download failed with code {response.status_code} and response body: {str(response.content)}")

    def upload_graph(self, g: Graph):
        update = """INSERT DATA {""" + g.serialize(format='nt') + "}"
        self.perform_update(update)

    def delete_and_insert_graphs(self, g_to_delete: Graph, g_to_insert: Graph):
        update = f"""DELETE {{ {g_to_delete.serialize(format='nt')} }}
                     INSERT {{ {g_to_insert.serialize(format='nt')} }}
                     WHERE {{ {g_to_delete.serialize(format='nt')} }}"""
        self.perform_update(update)
        # print(update)

    def check_if_triple_exist(self, s, p, o, data_type: str = None) -> bool:
        s = "?s" if s is None else f"<{utils.trim_iri(s)}>"
        p = "?p" if p is None else f"<{utils.trim_iri(p)}>"
        o = "?o" if o is None else f"<{utils.trim_iri(o)}>" if data_type is None else Literal(o, datatype=utils.trim_iri(data_type))._literal_n3()
        query = f"""ASK {{{s} {p} {o}.}}"""
        response = self.perform_query(query)
        return response[0]['ASK']

    def get_outgoing_and_attributes(self, node_iris: Set[str]) -> Dict[str, Any]:
        if isinstance(node_iris, str):
            node_iris = [node_iris]
        if isinstance(node_iris, list):
            node_iris = set(node_iris)
        query = f"""SELECT ?s ?p ?o WHERE {{VALUES ?s {{ {' '.join([f'<{utils.trim_iri(iri)}>' for iri in node_iris])} }} ?s ?p ?o.}}"""
        response = self.perform_query(query)
        result = {}
        for r in response:
            if r['s'] not in result:
                result[r['s']] = {}
            if r['p'] not in result[r['s']]:
                result[r['s']][r['p']] = set()
            result[r['s']][r['p']].add(r['o'])
        return result
