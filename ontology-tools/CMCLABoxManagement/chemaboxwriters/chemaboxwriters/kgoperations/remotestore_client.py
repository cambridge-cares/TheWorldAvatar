from chemaboxwriters.kgoperations.javagateway import jpsBaseLibGW
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Any, List, Dict, Optional, Type
from SPARQLWrapper import SPARQLWrapper, JSON
from pprint import pformat
from abc import ABC, abstractmethod
import json

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")


class RemoteStoreClient(ABC):
    """Abstract remote store client interface."""

    def __init__(self, endpoint_url) -> None:
        self._store_client = self._create_store_client(endpoint_url)

    @abstractmethod
    def _create_store_client(self, endpoint_url) -> Any:
        pass

    @abstractmethod
    def execute_query(self, query_str: str) -> List:
        pass


class JPSRemoteStoreClient(RemoteStoreClient):
    """JPS remote store client."""

    def _create_store_client(self, endpoint_url: str) -> Any:

        return jpsBaseLib_view.RemoteStoreClient(endpoint_url)

    def execute_query(self, query_str: str) -> List:
        response = self._store_client.executeQuery(query_str)
        return json.loads(str(response))


class SPARQLWrapperRemoteStoreClient(RemoteStoreClient):
    """PARQLWrapper remote store client."""

    def _create_store_client(self, endpoint_url: str) -> Any:
        store_client = SPARQLWrapper(endpoint_url)
        store_client.setReturnFormat(JSON)
        return store_client

    def execute_query(self, query_str: str) -> List:
        self._store_client.setQuery(query_str)
        response = self._store_client.queryAndConvert()

        return self._sparql_wrapper_jps_client_response_adapter(response=response)

    def _sparql_wrapper_jps_client_response_adapter(self, response: Dict) -> List:

        results = []
        results_dict = {}
        for result_item in response["results"]["bindings"]:
            for key, item in result_item.items():
                results_dict.update({key: item["value"]})
        if results_dict:
            results.append(results_dict)
        return results


TRemoteStoreClient = Type[RemoteStoreClient]


class RemoteStoreClientContainer:
    """Remote store client container. Used to store multiple
    store clients. Its get_store_client method creates
    a store client of a given type if not present, otherwise
    it returns an existing store client instance.
    """

    def __init__(self, query_endpoints: Optional[Dict[str, str]]):
        self.query_endpoints = {}
        self.store_clients = {}

        if query_endpoints is not None:
            for prefix, url in query_endpoints.items():
                self.register_query_endpoint(prefix, url)

    def info(self) -> None:
        print("--------------------------------------------------")
        print("remote_store_client")
        print("query_endpoints:")
        print(pformat(self.query_endpoints))

    def register_query_endpoint(self, endpoint_prefix: str, endpoint_url: str) -> None:
        # this only registers endpoints, creation of store clients happens in the
        # get_store_client call.
        self.query_endpoints[endpoint_prefix] = endpoint_url
        self.store_clients[endpoint_prefix] = {}

    def execute_query(
        self,
        endpoint_prefix: str,
        query_str: str,
        store_client_class: TRemoteStoreClient = JPSRemoteStoreClient,
    ) -> List[Dict[str, Any]]:

        client = self.get_store_client(
            endpoint_prefix, store_client_class=store_client_class
        )
        response = client.execute_query(query_str)
        return json.loads(str(response))

    def get_store_client(
        self,
        endpoint_prefix: str,
        store_client_class: TRemoteStoreClient = JPSRemoteStoreClient,
    ) -> RemoteStoreClient:
        """Gets the store client for a given endpoint (via tis prefix) if exists,
        otherwise it creates one. Raises a MissingQueryEndpoint if the endpoint
        prefix is not registered. By default it creates the JPS remote store client.
        """

        endpoint_url = self.query_endpoints.get(endpoint_prefix)

        if endpoint_url is None:
            raise app_exceptions.MissingQueryEndpoint(
                (
                    f"The {endpoint_prefix} query endpoint does not exist. "
                    "Register it first with the register_query_endpoint method."
                )
            )

        if store_client_class.__name__ not in self.store_clients[endpoint_prefix]:
            self.store_clients[endpoint_prefix][
                store_client_class.__name__
            ] = self._create_store_client(
                endpoint_url, store_client_class=store_client_class
            )
        return self.store_clients[endpoint_prefix][store_client_class.__name__]

    def _create_store_client(
        self, endpoint_url: str, store_client_class: TRemoteStoreClient
    ) -> RemoteStoreClient:
        return store_client_class(endpoint_url=endpoint_url)


def get_store_client_container(
    query_endpoints: Optional[Dict[str, str]] = None
) -> RemoteStoreClientContainer:

    return RemoteStoreClientContainer(query_endpoints)
