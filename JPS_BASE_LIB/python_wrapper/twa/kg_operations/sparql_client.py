import json
import requests
from datetime import datetime
from typing import Tuple, Dict, Any, Set, List
from rdflib import Graph, Literal

from twa.kg_operations.gateway import jpsBaseLibGW

from twa.data_model import utils
from twa.data_model.iris import PREFIX_RDFS, PREFIX_RDF, PREFIX_XSD, PREFIX_OWL

class PySparqlClient:
    """
    The purpose of this class is to provide a Python interface to the Java-based RemoteStoreClient for querying and updating the knowledge graph (triplestore).

    Attributes:
        jpsBaseLib_view (py4j.JavaGateway.JVM): The module view for the JpsBaseLib
        kg_client (RemoteStoreClient): The Java-based uk.ac.cam.cares.jps.base.query.RemoteStoreClient object
        query_endpoint (str): The SPARQL query endpoint of the knowledge graph
        update_endpoint (str): The SPARQL update endpoint of the knowledge graph
        fs_url (str): The URL of the fileserver
        fs_auth (str): The authentication information for the fileserver
    """

    def __init__(
        self,
        query_endpoint: str,
        update_endpoint: str,
        kg_user: str = None,
        kg_password: str = None,
        fs_url: str = None,
        fs_user: str = None,
        fs_pwd: str = None
    ):
        """
        The constructor for the PySparqlClient class.

        Args:
            query_endpoint (str): The SPARQL query endpoint of the knowledge graph
            update_endpoint (str): The SPARQL update endpoint of the knowledge graph
            kg_user (str): The username for the knowledge graph
            kg_password (str): The password for the knowledge graph
            fs_url (str): The URL of the fileserver
            fs_user (str): The username for the fileserver
            fs_pwd (str): The password for the fileserver
        """
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

    def check_instance_class(self, instance: str, instance_class: str) -> bool:
        """
        This method checks if the given instance is instantiated from the given instance class.

        Args:
            instance (str): IRI of an instance
            instance_class (str): IRI of the instance class to be checked against

        Returns:
            bool: True if the instance is instantiated from the given instance class, False otherwise
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

    def get_amount_of_triples(self) -> int:
        """
        This method returns the total number of triples in the knowledge graph.

        Returns:
            int: The total number of triples in the knowledge graph
        """
        # return an integer of total number of triples
        return self.kg_client.getTotalNumberOfTriples()

    def perform_query(self, query: str) -> Dict[str, Any]:
        """
        This function performs query to knowledge graph.

        Args:
            query (str): SPARQL Query string

        Returns:
            Dict[str, Any]: The response of the query
        """
        response = str(self.kg_client.executeQuery(query))
        return json.loads(response)

    def perform_update(self, update: str) -> None:
        """
        This function performs SPARQL Update to knowledge graph.

        Args:
            update (str): SPARQL Update string
        """
        self.kg_client.executeUpdate(update)

    def get_all_instances_of_class(self, class_iri: str) -> List[str]:
        """
        This function returns all instances of the given class.

        Args:
            class_iri (str): IRI of the class

        Returns:
            List[str]: List of IRIs of all instances of the given class
        """
        # Prepare query string
        query = f"""SELECT ?iri WHERE {{ ?iri a <{class_iri}> }}"""

        # Perform query
        response = self.perform_query(query)

        return [list(r.values())[0] for r in response]

    def upload_ontology(self, file_path: str) -> None:
        """
        This function uploads ontology to knowledge graph.

        Args:
            file_path (str): The file path of ontology to be uploaded
        """
        javaFile = self.jpsBaseLib_view.java.io.File(file_path)
        self.kg_client.uploadFile(javaFile)

    def upload_file(self, local_file_path: str, filename_with_subdir: str = None) -> Tuple[str, float]:
        """
        This function uploads the file at the given local file path to file server.

        Args:
            local_file_path (str): The local file path of the file to be uploaded
            filename_with_subdir (str): The filename with subdirectory in the file server

        Returns:
            Tuple[str, float]: The remote file path and the timestamp when the file was uploaded
        """
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

    def download_file(self, remote_file_path: str, downloaded_file_path: str) -> None:
        """
        This function downloads a file given the remote file path and the local file path to store the downloaded file.

        Args:
            remote_file_path (str): The remote file path of the file to be downloaded
            downloaded_file_path (str): The local file path to store the downloaded file
        """
        response = requests.get(remote_file_path, auth=self.fs_auth)
        if (response.status_code == requests.status_codes.codes.OK):
            with open(downloaded_file_path, 'wb') as file_obj:
                for chunk in response.iter_content(chunk_size=128):
                    file_obj.write(chunk)
        else:
            raise Exception(f"ERROR: File <{remote_file_path}> download failed with code {response.status_code} and response body: {str(response.content)}")

    def upload_graph(self, g: Graph) -> None:
        """
        This function uploads the given graph to the knowledge graph.

        Args:
            g (Graph): The rdflib.Graph object to be uploaded
        """
        update = f"""INSERT DATA {{ {g.serialize(format='nt')} }}"""
        self.perform_update(update)

    def delete_graph(self, g: Graph) -> None:
        """
        This function deletes the triples in the graph provided.

        Args:
            g (Graph): The rdflib.Graph object to be deleted
        """
        update = f"""DELETE DATA {{ {g.serialize(format='nt')} }}"""
        self.perform_update(update)

    def delete_and_insert_graphs(self, g_to_delete: Graph, g_to_insert: Graph) -> None:
        """
        This function deletes the triples in the first graph and inserts the triples in the second graph.

        Args:
            g_to_delete (Graph): The rdflib.Graph object to be deleted
            g_to_insert (Graph): The rdflib.Graph object to be inserted
        """
        update = f"""DELETE {{ {g_to_delete.serialize(format='nt')} }}
                     INSERT {{ {g_to_insert.serialize(format='nt')} }}
                     WHERE {{}}"""
        self.perform_update(update)

    def check_if_triple_exist(self, s: str, p: str, o: Any, data_type: str = None) -> bool:
        """
        This function checks if the given triple exists in the knowledge graph.

        Args:
            s (str): Subject IRI
            p (str): Predicate IRI
            o (Any): Object IRI or literal value
            data_type (str): Data type of the object literal

        Returns:
            bool: True if the triple exists, False otherwise
        """
        s = "?s" if s is None else f"<{utils.trim_iri(s)}>"
        p = "?p" if p is None else f"<{utils.trim_iri(p)}>"
        o = "?o" if o is None else f"<{utils.trim_iri(o)}>" if data_type is None else Literal(o, datatype=utils.trim_iri(data_type))._literal_n3()
        query = f"""ASK {{{s} {p} {o}.}}"""
        response = self.perform_query(query)
        return response[0]['ASK']

    def get_outgoing_and_attributes(self, node_iris: Set[str]) -> Dict[str, Dict[str, Set[Any]]]:
        """
        This function returns the outgoing edges and attributes of the given nodes in the knowledge graph.

        Args:
            node_iris (Set[str]): The set of IRIs of the nodes

        Returns:
            Dict[str, Dict[str, Set[Any]]]: The dictionary of the outgoing edges and attributes of the given nodes, where the key is the node IRI
        """
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
