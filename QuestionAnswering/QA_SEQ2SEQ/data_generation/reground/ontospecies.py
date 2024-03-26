import json
import os
import random
from typing import List, Tuple

import networkx as nx
from constants.functions import StrOp
from reground.reground import Regrounder
from reground.utils import replace_nlq_literal, replace_sparql_literal
from constants.fs import ROOTDIR
from locate_then_ask.kg_client import KgClient


class OKRegrounder(Regrounder):
    PATH_TO_ENTITIES_FOR_REGROUNDING = os.path.join(
        ROOTDIR, "data", "entities_for_regrounding", "ontokin.json"
    )

    @classmethod
    def _query_species_for_regrounding(cls, kg_client: KgClient):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species (SAMPLE(?Identifier) AS ?SpeciesLabel) (COUNT(DISTINCT ?p) AS ?deg) WHERE {
  VALUES ?hasIdentifier { os:hasInChI os:hasIUPACName os:hasMolecularFormula os:hasSMILES }
  ?x a os:Species ; ?hasIdentifier [ os:value ?Identifier ] .
  { ?x ?p ?o } UNION { ?s ?p ?x }
  
}
GROUP BY ?Species
ORDER BY ASC(?deg)
LIMIT 10"""
        response_bindings = kg_client.query(query)["results"]["bindings"]
        return [x["label"]["value"] for x in response_bindings]

    @classmethod
    def _query_chemclass_for_regrounding(cls, kg_client: KgClient):
        query = """"""
        response_bindings = kg_client.query(query)["results"]["bindings"]
        eqns = [x["eqn"]["value"] for x in response_bindings]
        eqns = [x.replace("[=]", random.choice(["<=>", "="])) for x in eqns]
        return eqns

    @classmethod
    def _query_use_for_regrounding(cls, kg_client: KgClient):
        query = """"""
        response_bindings = kg_client.query(query)["results"]["bindings"]
        eqns = [x["eqn"]["value"] for x in response_bindings]
        eqns = [x.replace("[=]", random.choice(["<=>", "="])) for x in eqns]
        return eqns

    @classmethod
    def _query_entities_for_regrounding(cls):
        client = KgClient(
            "http://theworldavatar.com/blazegraph/namespace/ontokin/sparql"
        )
        data = dict(
            Species=cls._query_species_for_regrounding(client),
        )
        with open(cls.PATH_TO_ENTITIES_FOR_REGROUNDING, "w") as f:
            json.dump(data, f, indent=4)

    def __init__(self) -> None:
        if not os.path.exists(self.PATH_TO_ENTITIES_FOR_REGROUNDING):
            self._query_entities_for_regrounding()
        with open(self.PATH_TO_ENTITIES_FOR_REGROUNDING, "r") as f:
            self.entities_for_regrounding = json.load(f)

    def reground(
        self, query_graph: nx.DiGraph, query_sparql: str, paraphrases: List[str]
    ):
        chemclasses = []
        uses = []

        for _, o, p in query_graph.edges("Species", data="label"):
            if p == "func" and query_graph.nodes[o].get("operator") is StrOp.VALUES:
                species = query_graph.nodes[o]["operand"]
            else:
                species = []

        for _, o, p in query_graph.edges(data="label"):
            if p.endswith("os:hasUse/rdfs:label"):
                chemclasses.append(query_graph.nodes[o]["label"])
            elif p.endswith("op:hasDOI"):
                uses.append(query_graph.nodes[o]["label"])

        if not any([species, chemclasses, uses]):
            return [(query_sparql, p) for p in paraphrases]

        pairs: List[Tuple[str, str]] = []
        for paraphrase in paraphrases:
            _query_sparql = str(query_sparql)
            for s in species:
                _s = random.choice(self.entities_for_regrounding["Species"])
                paraphrase = replace_nlq_literal(paraphrase, old=s, new=_s)
                _query_sparql = replace_sparql_literal(_query_sparql, old=s, new=_s)
            for c in chemclasses:
                _c = random.choice(self.entities_for_regrounding["ChemicalClass"])
                paraphrase = replace_nlq_literal(paraphrase, old=c, new=_c)
                _query_sparql = replace_sparql_literal(_query_sparql, old=c, new=_c)
            for u in uses:
                _u = random.choice(self.entities_for_regrounding["Use"])
                paraphrase = replace_nlq_literal(paraphrase, old=u, new=_u)
                _query_sparql = replace_sparql_literal(_query_sparql, old=u, new=_u)

            pairs.append((_query_sparql, paraphrase))

        return pairs
