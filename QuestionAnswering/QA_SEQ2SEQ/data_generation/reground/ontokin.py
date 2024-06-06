import json
import os
import random
from typing import List, Tuple

import networkx as nx
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
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT ?label (COUNT(*) AS ?deg) WHERE {
?x a okin:Species ; okin:belongsToPhase/a okin:GasPhase ; rdfs:label ?label .
{ ?x ?p ?o } UNION { ?s ?p ?x }
}
GROUP BY ?label
ORDER BY DESC(?deg) 
LIMIT 100"""
        response_bindings = kg_client.query(query)["results"]["bindings"]
        return [x["label"]["value"] for x in response_bindings]

    @classmethod
    def _query_eqn_for_regrounding(cls, kg_client: KgClient):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT ?eqn (COUNT(*) AS ?deg) WHERE {
?x a/rdfs:subClassOf* okin:GasPhaseReaction ; okin:hasEquation ?eqn .
{ ?x ?p ?o } UNION { ?s ?p ?x }
}
GROUP BY ?eqn
ORDER BY DESC(?deg) 
LIMIT 100"""
        response_bindings = kg_client.query(query)["results"]["bindings"]
        eqns = [x["eqn"]["value"] for x in response_bindings]
        eqns = [x.replace("[=]", random.choice(["<=>", "="])) for x in eqns]
        return eqns

    @classmethod
    def _query_doi_for_regrounding(cls):
        with open(
            os.path.join(ROOTDIR, "data", "entities_for_regrounding", "doi.txt"), "r"
        ) as f:
            data = [x.strip() for x in f.readlines()]
        return [x for x in data if x]

    @classmethod
    def _query_entities_for_regrounding(cls):
        client = KgClient(
            "http://theworldavatar.com/blazegraph/namespace/ontokin/sparql"
        )
        data = dict(
            Species=cls._query_species_for_regrounding(client),
            Equations=cls._query_eqn_for_regrounding(client),
            DOIs=cls._query_doi_for_regrounding(),
        )
        with open(cls.PATH_TO_ENTITIES_FOR_REGROUNDING, "w") as f:
            json.dump(data, f, indent=4)

    def __init__(self) -> None:
        if not os.path.exists(self.PATH_TO_ENTITIES_FOR_REGROUNDING):
            self._query_entities_for_regrounding()
        with open(self.PATH_TO_ENTITIES_FOR_REGROUNDING, "r") as f:
            self.entities_for_regrounding = json.load(f)

    def reground(self, query_graph: nx.DiGraph, query_sparql: str, paraphrases: List[str]):
        species = []
        eqns = []
        dois = []

        for _, o, p in query_graph.edges(data="label"):
            if p.endswith("skos:altLabel"):
                species.append(query_graph.nodes[o]["label"])
            elif p.endswith("okin:hasEquation"):
                eqns.append(query_graph.nodes[o]["label"])
            elif p.endswith("op:hasDOI"):
                dois.append(query_graph.nodes[o]["label"])

        if not any([species, eqns, dois]):
            return [(query_sparql, p) for p in paraphrases]

        pairs: List[Tuple[str, str]] = []
        for paraphrase in paraphrases:
            _query_sparql = str(query_sparql)
            for s in species:
                _s = random.choice(self.entities_for_regrounding["Species"])
                paraphrase = replace_nlq_literal(paraphrase, old=s, new=_s)
                _query_sparql = replace_sparql_literal(_query_sparql, old=s, new=_s)
            for e in eqns:
                _e = random.choice(self.entities_for_regrounding["Equation"])
                paraphrase = replace_nlq_literal(paraphrase, old=e, new=_e)
                _query_sparql = replace_sparql_literal(_query_sparql, old=e, new=_e)
            for d in dois:
                _d = random.choice(self.entities_for_regrounding["DOI"])
                paraphrase = replace_nlq_literal(paraphrase, old=d, new=_d)
                _query_sparql = replace_sparql_literal(_query_sparql, old=d, new=_d)

            pairs.append((_query_sparql, paraphrase))

        return pairs
