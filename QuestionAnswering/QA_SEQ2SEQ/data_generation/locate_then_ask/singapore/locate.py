import random
from typing import Literal

from constants.functions import OBE_NUM_OPS
from constants.namespaces import NAMESPACE2PREFIX, OM, OZNG
from constants.om import OM_KEY_LABELS
from constants.plot import OZNG_LANDUSETYPE_LABELS, PLOT_ATTR_LABELS
from locate_then_ask.model import OmMeasure
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.singapore.entity_store import SgEntityStore
from utils.numerical import make_operand_and_verbn


class OPltPlotLocator:
    def __init__(self, store: SgEntityStore):
        self.store = store

    def locate_concept(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Plot", iri=entity_iri)
        return query_graph, random.choice(["plot", "land plot"])

    def _locate_measure(
        self,
        query_graph: QueryGraph,
        key: Literal["GrossPlotRatio", "PlotArea", "GrossFloorArea"],
        measure: OmMeasure,
    ):
        operator = random.choice(OBE_NUM_OPS)
        operand, op_verbn = make_operand_and_verbn(
            operator, value=measure.numerical_value
        )

        measurevalue_node = query_graph.make_blank_node(key=key)
        numval_node = key + "NumericalValue"
        query_graph.add_literal_node(numval_node)

        assert measure.unit_iri.startswith(OM), measure.unit_iri
        unit = measure.unit_iri[len(OM) :]
        unit_node = "om:" + unit
        query_graph.add_iri_node(unit_node, prefixed=True)
        unit_verbn = random.choice(OM_KEY_LABELS[unit])

        if key == "GrossPlotRatio":
            pred = "^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue"
        elif key == "PlotArea":
            pred = "oplt:hasPlotArea/om:hasValue"
        elif key == "GrossFloorArea":
            pred = "oplt:hasMaximumPermittedGPR/om:hasValue"
        else:
            raise Exception("Unrecognised key: " + key)

        query_graph.add_func(
            target_node=numval_node, operator=operator, operand=operand
        )
        query_graph.add_triples(
            [
                (
                    "Plot",
                    pred,
                    measurevalue_node,
                ),
                (measurevalue_node, "om:hasNumericalValue", numval_node),
                (measurevalue_node, "om:hasUnit", unit_node),
            ]
        )

        verbn = "whose {key} is {op} {unit}".format(
            key=random.choice(PLOT_ATTR_LABELS[key]), op=op_verbn, unit=unit_verbn
        )

        return verbn

    def locate_attrs(self, entity_iri: str, attr_num: int):
        query_graph, concept = self.locate_concept(entity_iri)

        entity = self.store.get(entity_iri)
        keys = entity.get_nonnone_keys()
        conds = []
        for k in random.sample(keys, k=attr_num):
            if k == "land_use_type":
                assert entity.land_use_type is not None
                assert entity.land_use_type.startswith(OZNG)
                clsname = entity.land_use_type[len(OZNG) :]
                query_graph.add_triple(
                    "Plot", "ozng:hasLandUseType/a", NAMESPACE2PREFIX[OZNG] + clsname
                )
                cond = "whose {attr} is {value}".format(
                    attr=random.choice(["land use type", "land use classification"]),
                    value=random.choice(OZNG_LANDUSETYPE_LABELS[clsname]),
                )
            elif k == "gross_plot_ratio":
                assert entity.gross_plot_ratio is not None
                cond = self._locate_measure(
                    query_graph, key="GrossPlotRatio", measure=entity.gross_plot_ratio
                )
            elif k == "is_awaiting_detailed_gpr_eval":
                assert entity.is_awaiting_detailed_gpr_eval is not None
                if entity.is_awaiting_detailed_gpr_eval:
                    val = "true^^xsd:boolean"
                else:
                    val = "false^^xsd:boolean"
                query_graph.add_triple(
                    "Plot", "oplnrgl:isAwaitingDetailedGPREvaluation", val
                )
                cond = "which is awaiting detailed gross plot ratio evaluation"
            elif k == "plot_area":
                assert entity.plot_area is not None
                cond = self._locate_measure(
                    query_graph, key="PlotArea", measure=entity.plot_area
                )
            elif k == "gross_floor_area":
                assert entity.gross_floor_area is not None
                cond = self._locate_measure(
                    query_graph, key="GrossFloorArea", measure=entity.gross_floor_area
                )
            else:
                raise Exception("Unexpected key: " + k)
            conds.append(cond)

        return query_graph, "{concept}, {attrs}".format(
            concept=concept, attrs=", and ".join(conds)
        )
