import random

import numpy as np
from constants.functions import AGG_OP_LABELS, EXTREME_FREQ_LABELS, EXTREMUM_OPS, AggOp
from constants.om import OM_KEY_LABELS

from constants.ontobuiltenv import OBE_ATTR_LABELS, OBEAttrKey
from locate_then_ask.graph2sparql import Graph2Sparql
from locate_then_ask.query_graph import QueryGraph
from utils.numerical import normalize_1d


class OBEAsker:
    NUMERICAL_KEYS = [
        OBEAttrKey.TOTAL_FLOOR_AREA,
        OBEAttrKey.MARKET_VALUE,
        OBEAttrKey.GROUND_ELEVATION,
    ]

    KEY2UNIT = {
        OBEAttrKey.TOTAL_FLOOR_AREA: "om:squareMetre",
        OBEAttrKey.MARKET_VALUE: "om:poundSterling",
        OBEAttrKey.GROUND_ELEVATION: "om:metre",
    }

    DISCRETE_ATTRS = [
        OBEAttrKey.ADDRESS,
        OBEAttrKey.BUILT_FORM,
        OBEAttrKey.ENERGY_RATING,
        OBEAttrKey.NUMBER_OF_HABITABLE_ROOMS,
        OBEAttrKey.PROPERTY_TYPE,
        OBEAttrKey.PROPERTY_USAGE,
    ]

    def __init__(self):
        self.graph2sparql = Graph2Sparql()

    def _find_unsampled_keys(self, query_graph: QueryGraph):
        sampled_attr_keys = tuple(
            key for _, key in query_graph.nodes(data="key") if key is not None
        )
        assert all(
            isinstance(key, OBEAttrKey) for key in sampled_attr_keys
        ), sampled_attr_keys

        return tuple(x for x in OBEAttrKey if x not in sampled_attr_keys)

    def _find_unsampled_numerical_keys(self, query_graph: QueryGraph):
        sampled_attr_keys = tuple(
            key for _, key in query_graph.nodes(data="key") if key is not None
        )
        assert all(
            isinstance(key, OBEAttrKey) for key in sampled_attr_keys
        ), sampled_attr_keys

        return [k for k in self.NUMERICAL_KEYS if k not in sampled_attr_keys]

    def _ask_attr(self, query_graph: QueryGraph, key: OBEAttrKey):
        if key is OBEAttrKey.ADDRESS and random.random() < 1 / 3:
            q = random.choice(["postalcode", "street"])
            if q == "postalcode":
                query_graph.add_question_node("PostalCodeLabel")
                query_graph.add_triple(
                    "Property",
                    "obe:hasAddress/obe:hasPostalCode/rdfs:label",
                    "PostalCodeLabel",
                )

                return "postal code"
            elif q == "street":
                query_graph.add_question_node("Street")
                query_graph.add_triple(
                    "Property", "obe:hasAddress/ict:hasStreet", "Street"
                )

                return "street"
            else:
                raise Exception("Unexpected value: " + q)
        elif key in [
            OBEAttrKey.BUILT_FORM,
            OBEAttrKey.PROPERTY_USAGE,
            OBEAttrKey.PROPERTY_TYPE,
        ]:
            qnode = key.value + "Type"
            query_graph.add_question_node(qnode)
            query_graph.add_triple(
                "Property", "obe:has{key}/a".format(key=key.value), qnode
            )

            return random.choice(OBE_ATTR_LABELS[key])
        else:
            query_graph.add_question_node(key.value)
            query_graph.add_triple("Property", "obe:has" + key.value, key.value)

            return random.choice(OBE_ATTR_LABELS[key])

    def ask_name(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Property")

        query_sparql = self.graph2sparql.convert(query_graph)
        verbalization = "What is the " + verbalization

        return query_sparql, verbalization

    def ask_count(self, query_graph: QueryGraph, verbalization: str):
        query_graph.add_question_node("Property", agg=AggOp.COUNT)

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
                    ("Property", "obe:has{key}/om:hasValue".format(key=key.value), bn),
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
                attr=random.choice(OBE_ATTR_LABELS[key]),
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

    def ask_name_byExtremeAttr(
        self, query_graph: QueryGraph, verbalization: str, limit: int = 1
    ):
        key = random.choice(self._find_unsampled_numerical_keys(query_graph))
        modifier = random.choice(EXTREMUM_OPS)

        bn = query_graph.make_blank_node()
        value_node = key.value + "NumericalValue"
        unit = self.KEY2UNIT[key]
        query_graph.add_iri_node(unit, prefixed=True)
        query_graph.add_triples(
            [
                ("Property", "obe:has{key}/om:hasValue".format(key=key.value), bn),
                (bn, "om:hasNumericalValue", value_node),
                (bn, "om:hasUnit", unit),
            ]
        )
        query_graph.add_question_node("Property")

        query_graph.add_groupby("Property")
        query_graph.add_orderby(value_node, desc=modifier is AggOp.MAX)
        query_graph.set_limit(limit)

        query_sparql = self.graph2sparql.convert(query_graph)
        template = "What are the {limit} {located} and with the {modifier} {attr}"
        verbalization = template.format(
            limit=limit,
            located=verbalization,
            modifier=random.choice(AGG_OP_LABELS[modifier]),
            attr=random.choice(OBE_ATTR_LABELS[key]),
        )

        return query_sparql, verbalization

    def ask_attr_byEntityFreq(
        self, query_graph: QueryGraph, verbalization: str, limit: int = 1
    ):
        sampling_frame = [
            x
            for x in self.DISCRETE_ATTRS
            if x in self._find_unsampled_keys(query_graph)
        ]
        key = random.choice(sampling_frame)
        modifier = random.choice(EXTREMUM_OPS)

        attr_verbn = self._ask_attr(query_graph, key)
        query_graph.add_question_node("Property", agg=AggOp.COUNT)
        query_graph.add_orderby("PropertyCount", desc=modifier is AggOp.MAX)
        query_graph.set_limit(limit)

        query_sparql = self.graph2sparql.convert(query_graph)
        template = "What are the {limit} {modifier} {attr} among {located}"
        verbalization = template.format(
            limit=limit,
            modifier=random.choice(EXTREME_FREQ_LABELS[modifier]),
            attr=attr_verbn,
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
                    "Property",
                    "obe:has{key}/om:hasValue".format(key=extr_attr_key.value),
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
        query_graph.add_groupby(
            next(
                n
                for n, question_node in query_graph.nodes(data="question_node")
                if question_node
            )
        )
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
            attr=random.choice(OBE_ATTR_LABELS[extr_attr_key]),
        )

        return query_sparql, verbalization

    def ask_discreteAttr_byExtremeAvgAttr(
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
                    "Property",
                    "obe:has{key}/om:hasValue".format(key=extr_attr_key.value),
                    bn,
                ),
                (bn, "om:hasNumericalValue", value_node),
                (bn, "om:hasUnit", unit),
            ]
        )

        qn_key = random.choice(
            [
                x
                for x in self.DISCRETE_ATTRS
                if x in self._find_unsampled_keys(query_graph)
                and x is not extr_attr_key
            ]
        )
        qn_attr_verbn = self._ask_attr(query_graph, qn_key)
        query_graph.add_groupby(
            next(
                n
                for n, question_node in query_graph.nodes(data="question_node")
                if question_node
            )
        )
        query_graph.add_question_node(value_node, agg=AggOp.AVG)
        query_graph.add_orderby(value_node + "Avg", desc=modifier is AggOp.MAX)
        query_graph.set_limit(limit)

        query_sparql = self.graph2sparql.convert(query_graph)

        template = "Among {located}, what are the {limit} {qn_attr} with the {modifier} average {attr}"
        verbalization = template.format(
            limit=limit,
            qn_attr=qn_attr_verbn,
            located=verbalization,
            modifier=random.choice(AGG_OP_LABELS[modifier]),
            attr=random.choice(OBE_ATTR_LABELS[extr_attr_key]),
        )

        return query_sparql, verbalization
