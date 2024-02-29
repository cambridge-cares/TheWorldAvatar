import random

from constants.ontocompchem import OCC_RESULT_KEYS, OCC_RESULT_LABELS
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph


class OCCAsker:
    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def ask(self, query_graph: QueryGraph, verbalization: str, attr_num: int):
        attrs = random.sample(OCC_RESULT_KEYS, k=attr_num)
        for key in attrs:
            cls_iri = "occ:" + key

            query_graph.add_question_node(key)
            query_graph.add_iri_node(iri=cls_iri, prefixed=True)
            query_graph.add_triples([
                ("MolecularComputation", "occ:hasResult", key),
                (key, "a", cls_iri)
            ])

        comp_params = []
        lots = query_graph.get_objs(
            subj="MolecularComputation",
            predicate="occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label",
        )
        if random.getrandbits(1) and len(lots) == 0:
            comp_params.append("LevelOfTheory")

        bss = query_graph.get_objs(
            subj="MolecularComputation",
            predicate="occ:hasMethodology/occ:hasBasisSet/rdfs:label",
        )
        if random.getrandbits(1) and len(bss) == 0:
            comp_params.append("BasisSet")

        random.shuffle(comp_params)
        verbn_comp_params = []
        for key in comp_params:
            node = key + "Label"
            if key == "LevelOfTheory":
                predicate = "occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label"
                verbn_comp_params.append("level of theory")
            elif key == "BasisSet":
                predicate = "occ:hasMethodology/occ:hasBasisSet/rdfs:label"
                verbn_comp_params.append("basis set")
            else:
                raise Exception("Unexpected key: " + key)

            query_graph.add_question_node(node)
            query_graph.add_triple("MolecularComputation", predicate, node)

        verbn_attrs = [random.choice(OCC_RESULT_LABELS[x]) for x in attrs]
        template = "{V} the {attrs} of {species}"
        verbalization = template.format(
            V=random.choice(["What is", "Compare"]) if verbn_comp_params else "What is",
            attrs=" and ".join(verbn_attrs), species=verbalization
        )

        if len(verbn_comp_params) > 0:
            template = random.choice(
                [
                    " across all {comp_params}",
                    ", specifying the {comp_params} at which the calculations are performed",
                ]
            )
            verbalization += template.format(
                comp_params=" and ".join(verbn_comp_params),
            )

        query_sparql = self.graph2sparql.convert(query_graph)

        return query_sparql,verbalization
