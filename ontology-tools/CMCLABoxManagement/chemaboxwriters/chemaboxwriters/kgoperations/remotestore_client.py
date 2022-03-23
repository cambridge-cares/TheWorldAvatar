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
    def __init__(self, endpoint_url) -> None:
        self._store_client = self._create_store_client(endpoint_url)

    @abstractmethod
    def _create_store_client(self, endpoint_url)->Any:
        pass

    @abstractmethod
    def execute_query(self, query_str: str) -> List[Dict[str, Any]]:
        pass


class JPSRemoteStoreClient(RemoteStoreClient):
    def _create_store_client(self, endpoint_url: str) -> Any:

        return jpsBaseLib_view.RemoteStoreClient(endpoint_url)

    def execute_query(self, query_str: str) -> List[Dict[str, Any]]:
        response = self._store_client.executeQuery(query_str)
        return json.loads(str(response))


class SPARQLWrapperRemoteStoreClient(RemoteStoreClient):
    def _create_store_client(self, endpoint_url: str) -> Any:
        store_client = SPARQLWrapper(endpoint_url)
        store_client.setReturnFormat(JSON)
        return store_client

    def execute_query(self, query_str: str) -> List[Dict[str, Any]]:
        self._store_client.setQuery(query_str)
        response = self._store_client.queryAndConvert()
        return response


TRemoteStoreClient = Type[RemoteStoreClient]


class RemoteStoreClientContainer:
    def __init__(self, query_endpoints: Optional[Dict[str, str]]):
        self.query_endpoints = {}
        self.store_clients = {}

        if query_endpoints is not None:
            for prefix, url in query_endpoints.items():
                self.register_query_endpoint(prefix, url)

    def __str__(self) -> None:
        print("--------------------------------------------------")
        print("remote_store_client")
        print("query_endpoints:")
        print(pformat(self.query_endpoints))

    def register_query_endpoint(self, endpoint_prefix: str, endpoint_url: str) -> None:
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
        endpoint_url = self.query_endpoints.get(endpoint_prefix)

        if endpoint_url is None:
            raise app_exceptions.MissingQueryEndpoint(
                (
                    f"The {endpoint_prefix} query endpoint does not exist. "
                    "Register it first with the register_query_endpoint method."
                )
            )

        if store_client_class.__name__ not in self.store_clients[endpoint_prefix]:
            self.store_clients[endpoint_prefix][store_client_class.__name__] \
            = self._create_store_client(
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
