import random

from constants.functions import OBE_NUM_OPS, NumOp
from constants.namespaces import OM
from constants.om import OM_KEY_LABELS
from constants.ontobuiltenv import OBE_ATTR_LABELS, OBEAttrKey
from locate_then_ask.ontobuiltenv.model import OmMeasure
from locate_then_ask.query_graph import QueryGraph
from utils.numerical import make_operand_and_verbn


class OBEOmMeasureLocator:
    def locate(self, query_graph: QueryGraph, key: OBEAttrKey, measure: OmMeasure):
        operator = random.choice(OBE_NUM_OPS)
        operand, op_verbn = make_operand_and_verbn(
            operator, value=measure.numerical_value
        )

        measurevalue_node = query_graph.make_blank_node(key=key)
        numval_node = key.value + "NumericalValue"
        query_graph.add_literal_node(numval_node)

        assert measure.unit_iri.startswith(OM), measure.unit_iri
        unit = measure.unit_iri[len(OM) :]
        unit_node = "om:" + unit
        query_graph.add_iri_node(unit_node, prefixed=True)
        unit_verbn = random.choice(OM_KEY_LABELS[unit])

        query_graph.add_func(
            target_node=numval_node,
            operator=operator,
            operand=operand
        )
        query_graph.add_triples(
            [
                (
                    "Property",
                    "obe:has{key}/om:hasValue".format(key=key.value),
                    measurevalue_node,
                ),
                (measurevalue_node, "om:hasNumericalValue", numval_node),
                (measurevalue_node, "om:hasUnit", unit_node),
            ]
        )

        verbn = "{key} is {op} {unit}".format(
            key=random.choice(OBE_ATTR_LABELS[key]), op=op_verbn, unit=unit_verbn
        )

        return verbn
