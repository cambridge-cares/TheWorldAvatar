import random

import numpy as np
from constants.functions import AGG_OP_LABELS, EXTREMUM_OPS, AggOp
from constants.om import OM_KEY_LABELS

from constants.plot import PLOT_ATTR_2_PRED, PLOT_ATTR_LABELS, OPltPlotAttrKey
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph
from utils.numerical import normalize_1d


class OPltPlotAsker:
    NUMERICAL_KEYS = set(
        [
            OPltPlotAttrKey.GROSS_PLOT_RATIO,
            OPltPlotAttrKey.GROSS_FLOOR_AREA,
            OPltPlotAttrKey.PLOT_AREA,
        ]
    )

    KEY2UNIT = {
        OPltPlotAttrKey.GROSS_PLOT_RATIO: "om:one",
        OPltPlotAttrKey.GROSS_FLOOR_AREA: "om:squareMetre",
        OPltPlotAttrKey.PLOT_AREA: "om:squareMetre",
    }

    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def _find_unsampled_keys(self, query_graph: QueryGraph):
        sampled_attr_keys = tuple(
            key for _, _, key in query_graph.edges(data="key") if key is not None
        )
        return tuple(
            x
            for x in OPltPlotAttrKey
            if x not in sampled_attr_keys
            and x is not OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL
        )

    def _find_unsampled_numerical_keys(self, query_graph: QueryGraph):
        sampled_attr_keys = tuple(
            key for _, key in query_graph.nodes(data="key") if key is not None
        )
        return [k for k in self.NUMERICAL_KEYS if k not in sampled_attr_keys]

    def _ask_attr(self, query_graph: QueryGraph, key: OPltPlotAttrKey):
        query_graph.add_question_node(key.value)
        query_graph.add_triple("Plot", PLOT_ATTR_2_PRED[key], key.value)
        verbn = random.choice(PLOT_ATTR_LABELS[key])

        if key is OPltPlotAttrKey.GROSS_PLOT_RATIO:
            # ask also is_awaiting_detailed_gpr_evaluation
            key2 = OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL
            query_graph.add_question_node(key2.value)
            query_graph.add_triple("Plot", PLOT_ATTR_2_PRED[key2], key2.value)

        return verbn

    def ask_count(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Plot", agg=AggOp.COUNT)

        query_sparql = self.graph2sparql.convert(query_graph)
        template = random.choice(
            [
                "How many {located} are there",
                "For {located}, how many of them are there",
            ]
        )
        verbalization = template.format(located=verbalization)

        return query_sparql, verbalization

    def ask_attrs(self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1):
        unsampled_attr_keys = self._find_unsampled_keys(query_graph)

        verbns = []
        for key in random.sample(
            unsampled_attr_keys, k=min(attr_num, len(unsampled_attr_keys))
        ):
            verbn = self._ask_attr(query_graph, key)
            verbns.append(verbn)

        query_sparql = self.graph2sparql.convert(query_graph)
        template = random.choice(
            [
                "What is the {attrs} of the {located}",
                "For the {located}, what is its {attrs}",
            ]
        )
        verbalization = template.format(
            attrs=" and ".join(verbns), located=verbalization
        )

        return query_sparql, verbalization

    def ask_agg(self, query_graph: QueryGraph, verbalization: str, attr_num: int = 1):
        candidates = self._find_unsampled_numerical_keys(query_graph)
        assert len(candidates) > 0

        verbns = []
        for key in random.sample(candidates, k=min(len(candidates), attr_num)):
            bn = query_graph.make_blank_node()
            value_node = key.value + "NumericalValue"
            unit = self.KEY2UNIT[key]
            query_graph.add_iri_node(unit, prefixed=True)
            query_graph.add_triples(
                [
                    ("Plot", PLOT_ATTR_2_PRED[key], bn),
                    (bn, "om:hasNumericalValue", value_node),
                    (bn, "om:hasUnit", unit),
                ]
            )

            agg = np.random.choice(
                [AggOp.MIN, AggOp.MAX, AggOp.AVG], p=normalize_1d([2, 2, 1])
            )
            query_graph.add_question_node(value_node, agg=agg)

            template = "{modifier} {attr}"
            verbn = template.format(
                modifier=random.choice(AGG_OP_LABELS[agg]),
                attr=random.choice(PLOT_ATTR_LABELS[key]),
            )
            if random.getrandbits(1):
                verbn += " in {unit}".format(
                    unit=random.choice(OM_KEY_LABELS[unit[len("om:") :]])
                )
            verbns.append(verbn)

        query_sparql = self.graph2sparql.convert(query_graph)
        template = random.choice(
            [
                "What is the {attrs} of the {located}",
                "For the {located}, what is its {attrs}",
            ]
        )
        verbalization = template.format(
            attrs=" and ".join(verbns),
            located=verbalization,
        )

        return query_sparql, verbalization

    def ask_attr_byExtremeAttr(
        self, query_graph: QueryGraph, verbalization: str, limit: int = 1
    ):
        extr_attr_key = random.choice(self._find_unsampled_numerical_keys(query_graph))
        modifier = random.choice(EXTREMUM_OPS)

        bn = query_graph.make_blank_node()
        value_node = extr_attr_key.value + "NumericalValue"
        unit = self.KEY2UNIT[extr_attr_key]
        query_graph.add_iri_node(unit, prefixed=True)
        query_graph.add_triples(
            [
                (
                    "Plot",
                    PLOT_ATTR_2_PRED[extr_attr_key],
                    bn,
                ),
                (bn, "om:hasNumericalValue", value_node),
                (bn, "om:hasUnit", unit),
            ]
        )

        qn_key = random.choice(
            [
                x
                for x in self._find_unsampled_keys(query_graph)
                if x is not extr_attr_key
            ]
        )
        qn_attr_verbn = self._ask_attr(query_graph, qn_key)
        query_graph.add_groupby(qn_key.value)
        query_graph.add_orderby(value_node, desc=modifier is AggOp.MAX)
        query_graph.set_limit(limit)

        query_sparql = self.graph2sparql.convert(query_graph)
        template = (
            "What are the {qn_attr} of {limit} {located} and with the {modifier} {attr}"
        )
        verbalization = template.format(
            limit=limit,
            qn_attr=qn_attr_verbn,
            located=verbalization,
            modifier=random.choice(AGG_OP_LABELS[modifier]),
            attr=random.choice(PLOT_ATTR_LABELS[extr_attr_key]),
        )

        return query_sparql, verbalization
