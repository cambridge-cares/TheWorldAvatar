from typing import List
import requests

from SPARQLWrapper import POST, SPARQLWrapper
from yarl import URL


class TriplesManager:
    @classmethod
    def create_namespace(
        cls,
        base_url: URL,
        namespace: str,
    ):
        xml_properties = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">
<properties>
    <entry key="com.bigdata.rdf.sail.namespace">{namespace}</entry>
</properties>""".format(
            namespace=namespace
        )
        namespace_url = base_url / "blazegraph/namespace"
        requests.post(
            str(namespace_url),
            headers={"Content-Type": "application/xml"},
            data=xml_properties,
        )

    @classmethod
    def delete_namespace(cls, base_url: URL, namespace: str):
        requests.delete(str(base_url / "blazegraph/namespace" / namespace))

    def __init__(
        self,
        base_url: URL,
        namespace: str,
        prefixes: str,
    ):
        self.base_url = base_url
        self.namespace = namespace
        self.create_namespace(base_url=base_url, namespace=namespace)

        self.sparql = SPARQLWrapper(
            str(base_url / "blazegraph/namespace" / namespace / "sparql")
        )
        self.sparql.setMethod(POST)
        self.prefixes = prefixes

    def insert(self, triples: List[str]):
        query = """{prefixes}
INSERT {{
{triples}
}}
WHERE {{}}""".format(
            prefixes=self.prefixes,
            triples="\n".join(triples),
        )
        print(query)
        self.sparql.setQuery(query)
        self.sparql.query()

    def delete_all(self):
        self.sparql.setQuery("DELETE { ?s ?p ?o } WHERE {}")
        self.sparql.query()
        self.delete_namespace(self.base_url, self.namespace)
