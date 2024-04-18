from functools import lru_cache
from typing import List, Optional

from pydantic.dataclasses import dataclass

from services.utils.rdf import extract_name
from services.core.kg import KgClient


@dataclass
class Node:
    IRI: str
    label: Optional[str] = None


@dataclass
class OutgoingLink:
    relation: str
    tail: Node


@dataclass
class IncomingLink:
    head: Node
    relation: str


@dataclass
class NodeAttr:
    relation: str
    literal: str


@dataclass
class NodeDoc:
    node: Node
    outgoing_links: List[OutgoingLink]
    incoming_links: List[IncomingLink]
    attributes: List[NodeAttr]


class NodeStore:
    @classmethod
    def linearize_doc(cls, doc: NodeDoc):
        doc_node_label = (
            doc.node.label if doc.node.label else extract_name(doc.node.IRI)
        )
        outgoing_triples = [
            (
                doc_node_label,
                extract_name(link.relation),
                link.tail.label if link.tail.label else extract_name(link.tail.IRI),
            )
            for link in doc.outgoing_links
        ]
        incoming_triples = [
            (
                link.head.label if link.head.label else extract_name(link.head.IRI),
                extract_name(link.relation),
                doc_node_label,
            )
            for link in doc.incoming_links
            if link.head.label
        ]
        attribute_triples = [
            (doc_node_label, extract_name(attr.relation), attr.literal)
            for attr in doc.attributes
        ]
        return "\n".join(
            [
                "{s} {p} {o}".format(s=s, p=p, o=o)
                for triples in [attribute_triples, outgoing_triples, incoming_triples]
                for s, p, o in triples
            ]
        )

    def __init__(self, kg_client: KgClient):
        self.kg_client = kg_client

    @lru_cache(maxsize=128)
    def get_node(self, iri: str):
        query = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT DISTINCT ?label WHERE {{ <{iri}> rdfs:label ?label . }} LIMIT 1".format(iri=iri)
        bindings = self.kg_client.query(query)["results"]["bindings"]
        if not bindings:
            return Node(IRI=iri)
        return Node(IRI=iri, label=bindings[0]["label"]["value"])

    def triples_gen(self):
        query = "SELECT DISTINCT ?s WHERE { ?s ?p ?o . }"
        iris = [
            x["s"]["value"] for x in self.kg_client.query(query)["results"]["bindings"]
        ]
        for iri in iris:
            query = "SELECT DISTINCT ?p ?o WHERE {{ <{head}> ?p ?o . }}".format(
                head=iri
            )
            bindings = self.kg_client.query(query)["results"]["bindings"]

            outgoing_links: List[OutgoingLink] = []
            attributes: List[NodeAttr] = []
            for binding in bindings:
                if binding["o"]["type"] == "literal":
                    attributes.append(
                        NodeAttr(
                            relation=binding["p"]["value"],
                            literal=binding["o"]["value"],
                        )
                    )
                else:
                    outgoing_links.append(
                        OutgoingLink(
                            relation=binding["p"]["value"],
                            tail=self.get_node(iri=binding["o"]["value"]),
                        )
                    )

            query = "SELECT DISTINCT ?s ?p WHERE {{ ?s ?p <{tail}> }}".format(tail=iri)
            incoming_links = [
                IncomingLink(
                    head=self.get_node(iri=binding["s"]["value"]),
                    relation=binding["p"]["value"],
                )
                for binding in self.kg_client.query(query)["results"]["bindings"]
            ]

            yield NodeDoc(
                node=self.get_node(iri=iri),
                outgoing_links=outgoing_links,
                incoming_links=incoming_links,
                attributes=attributes,
            )
