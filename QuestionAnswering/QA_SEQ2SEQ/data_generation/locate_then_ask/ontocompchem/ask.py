import copy
import random

from constants.nl import PLURAL_ADJS
from constants.ontocompchem import OCC_RESULT_KEYS, OCC_RESULT_LABELS
from locate_then_ask.data_model import AskDatum
from locate_then_ask.ontocompchem.graph2sparql import OCCGraph2Sparql
from locate_then_ask.query_graph import QueryGraph, get_objs


class OCCAsker:
    def __init__(self):
        self.graph2sparql = OCCGraph2Sparql()

    def ask(self, query_graph: QueryGraph, verbalization: str, attr_num: int):
        query_graph = copy.deepcopy(query_graph)

        attrs = random.sample(OCC_RESULT_KEYS, k=attr_num)
        for key in attrs:
            cls_node = "occ:" + key
            query_graph.add_nodes_from(
                [
                    (key, dict(question_node=True)),
                    (cls_node, dict(iri=cls_node, prefixed=True, template_node=True)),
                ]
            )
            query_graph.add_edges_from(
                [
                    ("MolecularComputation", key, dict(label="occ:hasResult")),
                    (key, cls_node, dict(label="a")),
                ]
            )

        comp_params = []
        lots = get_objs(
            query_graph=query_graph,
            subj="MolecularComputation",
            predicate="occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label",
        )
        if random.getrandbits(1) and len(lots) == 0:
            comp_params.append("LevelOfTheory")

        bss = get_objs(
            query_graph=query_graph,
            subj="MolecularComputation",
            predicate="occ:hasMethodology/occ:hasBasisSet/rdfs:label",
        )
        if random.getrandbits(1) and len(bss) == 0:
            comp_params.append("BasisSet")

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

            query_graph.add_node(node, question_node=True)
            query_graph.add_edge("MolecularComputation", node, label=predicate)

        verbn_attrs = [random.choice(OCC_RESULT_LABELS[x]) for x in attrs]
        template = "What is the {attrs} of {species}"
        verbalization = template.format(
            attrs=" and ".join(verbn_attrs), species=verbalization
        )

        if len(verbn_comp_params) > 0:
            template = random.choice([
                " across all {comp_params}",
                ", specifying the {comp_params} at which the calculations are performed"
            ])
            verbalization += template.format(
                comp_params=" and ".join(verbn_comp_params),
            )

        query_sparql = self.graph2sparql.convert(query_graph)

        return AskDatum(
            query_graph=query_graph,
            query_sparql=query_sparql,
            verbalization=verbalization,
        )
