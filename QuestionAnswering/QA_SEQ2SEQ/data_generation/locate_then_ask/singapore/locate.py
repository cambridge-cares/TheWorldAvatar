import random

import numpy as np

from constants.functions import OBE_NUM_OPS
from constants.namespaces import NAMESPACE2PREFIX, OM, OZNG
from constants.om import OM_KEY_LABELS
from constants.plot import (
    OZNG_LANDUSETYPE_LABELS,
    PLOT_ATTR_2_PRED,
    PLOT_ATTR_LABELS,
    OPltPlotAttrKey,
)
from locate_then_ask.model import OmMeasure
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.singapore.entity_store import SgEntityStore
from utils.numerical import make_operand_and_verbn, normalize_1d


class OPltPlotLocator:
    ATTR_KEY_WEIGHTS = {
        OPltPlotAttrKey.LAND_USE_TYPE_TYPE: 29,
        OPltPlotAttrKey.GROSS_PLOT_RATIO: 1,
        OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL: 1,
        OPltPlotAttrKey.PLOT_AREA: 1,
        OPltPlotAttrKey.GROSS_FLOOR_AREA: 1,
    }

    def __init__(self, store: SgEntityStore):
        self.store = store

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Plot", iri=entity_iri)
        return query_graph, random.choice(["plot", "land plot"])

    def _locate_measure(
        self,
        query_graph: QueryGraph,
        key: OPltPlotAttrKey,
        measure: OmMeasure,
    ):
        operator = random.choice(OBE_NUM_OPS)
        operand, op_verbn = make_operand_and_verbn(
            operator, value=measure.numerical_value
        )

        measurevalue_node = query_graph.make_blank_node()
        numval_node = key.value + "NumericalValue"
        query_graph.add_literal_node(numval_node)

        assert measure.unit_iri.startswith(OM), measure.unit_iri
        unit = measure.unit_iri[len(OM) :]
        unit_node = "om:" + unit
        query_graph.add_iri_node(unit_node, prefixed=True)
        unit_verbn = random.choice(OM_KEY_LABELS[unit])

        query_graph.add_func(
            target_node=numval_node, operator=operator, operand=operand
        )
        query_graph.add_triples(
            [
                ("Plot", PLOT_ATTR_2_PRED[key], measurevalue_node, dict(key=key)),
                (measurevalue_node, "om:hasNumericalValue", numval_node),
                (measurevalue_node, "om:hasUnit", unit_node),
            ]
        )

        verbn = "whose {key} is {op} {unit}".format(
            key=random.choice(PLOT_ATTR_LABELS[key]), op=op_verbn, unit=unit_verbn
        )

        return verbn

    def locate_concept_and_literal_multi(self, entity_iri: str, cond_num: int):
        query_graph, concept = self.locate_concept_name(entity_iri)

        entity = self.store.get(entity_iri)
        keys = entity.get_nonnone_keys()
        weights = [self.ATTR_KEY_WEIGHTS[k] for k in keys]

        conds = []        
        for k in np.random.choice(
            keys, size=min(cond_num, len(keys)), p=normalize_1d(weights), replace=False
        ):
            if k is OPltPlotAttrKey.LAND_USE_TYPE_TYPE:
                assert entity.land_use_type_type is not None
                assert entity.land_use_type_type.startswith(OZNG)
                clsname = entity.land_use_type_type[len(OZNG) :]
                clsname_node = "{prefix}:{name}".format(
                    prefix=NAMESPACE2PREFIX[OZNG], name=clsname
                )
                query_graph.add_iri_node(clsname_node, prefixed=True)
                query_graph.add_triple("Plot", PLOT_ATTR_2_PRED[k], clsname_node, key=k)
                cond = "whose {attr} is {value}".format(
                    attr=random.choice(["land use type", "land use classification"]),
                    value=random.choice(OZNG_LANDUSETYPE_LABELS[clsname]),
                )
            elif k is OPltPlotAttrKey.GROSS_PLOT_RATIO:
                assert entity.gross_plot_ratio is not None
                cond = self._locate_measure(
                    query_graph, key=k, measure=entity.gross_plot_ratio
                )
            elif k is OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL:
                assert entity.is_awaiting_detailed_gpr_eval is not None
                literal_node = query_graph.make_literal_node(
                    entity.is_awaiting_detailed_gpr_eval
                )
                query_graph.add_triple("Plot", PLOT_ATTR_2_PRED[k], literal_node, key=k)
                if entity.is_awaiting_detailed_gpr_eval:
                    cond = "which is awaiting detailed gross plot ratio evaluation"
                else:
                    cond = "which is not awaiting detailed gross plot ratio evaluation"
            elif k is OPltPlotAttrKey.PLOT_AREA:
                assert entity.plot_area is not None
                cond = self._locate_measure(
                    query_graph, key=k, measure=entity.plot_area
                )
            elif k is OPltPlotAttrKey.GROSS_FLOOR_AREA:
                assert entity.gross_floor_area is not None
                cond = self._locate_measure(
                    query_graph, key=k, measure=entity.gross_floor_area
                )
            else:
                raise Exception("Unexpected key: " + k)
            conds.append(cond)

        return query_graph, "{concept}, {attrs}".format(
            concept=concept, attrs=", and ".join(conds)
        )
