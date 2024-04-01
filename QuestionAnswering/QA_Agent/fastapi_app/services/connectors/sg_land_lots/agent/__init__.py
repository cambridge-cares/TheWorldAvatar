import logging
from typing import Annotated, Optional, Tuple

from fastapi import Depends

from services.utils.rdf import flatten_sparql_response
from model.aggregate import AggregateOperator
from model.constraint import (
    CompoundComparativeConstraint,
    NumericalConstraint,
    ExtremeValueConstraint,
)
from model.qa import QAData
from services.core.kg import KgClient
from services.connectors.sg import get_sg_ontopClient
from ..constants import PLOT_NUM_ATTR_PREDICATES
from ..model import PlotAttrKey, PlotNumAttrKey
from ..kg import get_sgLandLots_bgClient
from .make_sparql import SGLandLotsSPARQLMaker

logger = logging.getLogger(__name__)


class SGLandLotsAgent:
    def __init__(
        self,
        bg_client: KgClient,
        ontop_client: KgClient,
        sparql_maker: SGLandLotsSPARQLMaker
    ):
        self.bg_client = bg_client
        # self.ontop_client = ontop_client
        self.ontop_client = KgClient("http://174.138.23.221:3838/ontop/ui/sparql")
        self.sparql_maker = sparql_maker

    def _make_clauses_for_constraint(
        self, key: PlotNumAttrKey, constraint: NumericalConstraint
    ):
        where_patterns = []
        orderby = None

        valuenode = "?{key}NumericalValue".format(key=key.value)
        where_patterns.append(
            "?IRI {pred}/om:hasNumericalValue {valuenode} .".format(
                pred=PLOT_NUM_ATTR_PREDICATES[key], valuenode=valuenode
            )
        )

        if isinstance(constraint, CompoundComparativeConstraint):
            atomic_constraints = [
                "{valuenode} {operator} {operand}".format(
                    valuenode=valuenode, operator=x.operator.value, operand=x.operand
                )
                for x in constraint.constraints
            ]
            if constraint.logical_operator:
                delimiter = constraint.logical_operator.value
            else:
                delimiter = "&&"
            exprn = delimiter.join(atomic_constraints)
            filter_pattern = "FILTER ( {exprn} )".format(exprn=exprn)
            where_patterns.append(filter_pattern)
        elif constraint is ExtremeValueConstraint.MAX:
            orderby = "DESC({var})".format(var=valuenode)
        else:
            orderby = valuenode

        return where_patterns, orderby

    def lookup_plot_attribute(
        self,
        attr_key: PlotAttrKey,
        land_use_type: Optional[str] = None,
    ):
        query = self.sparql_maker.lookup_plot_attribute(attr_key, land_use_type)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)

    def count_plots(self, land_use_type: Optional[str] = None):
        query = self.sparql_maker.count_plots(land_use_type)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)

    def compute_aggregate_plot_attribute(
        self,
        attr_agg: Tuple[PlotNumAttrKey, AggregateOperator],
        land_use_type: Optional[str] = None,
    ):
        query = self.sparql_maker(attr_agg, land_use_type)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)


def get_sgLandLots_agent(
    bg_client: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
):
    return SGLandLotsAgent(
        bg_client=bg_client,
        ontop_client=ontop_client,
    )
