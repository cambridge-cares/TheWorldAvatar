from typing import Annotated, Optional, Tuple

from fastapi import Depends

from model.qa import QAData
from model.aggregate import AggregateOperator
from services.utils.rdf import add_label_to_sparql_resposne, flatten_sparql_response
from services.core.kg import KgClient
from services.core.label_store import LabelStore
from services.connectors.sg import get_sg_ontopClient
from ..model import DataCentreAttrKey, DataCentreConstraints
from .make_sparql import SGDataCentresSPARQLMaker, get_sgDataCentres_sparqlMaker
from .label_store import get_sgDataCentres_labesStore


class SGDataCentresAgent:
    def __init__(
        self,
        ontop_client: KgClient,
        label_store: LabelStore,
        sparql_maker: SGDataCentresSPARQLMaker,
    ):
        self.ontop_client = ontop_client
        self.label_store = label_store
        self.sparql_maker = sparql_maker

    def lookup_dataCentre_attribute(self, name: str, attr_key: DataCentreAttrKey):
        iris = self.label_store.link_entity(name)

        query = self.sparql_maker.lookup_dataCentre_attribute(
            iris=iris, attr_key=attr_key
        )
        res = self.ontop_client.query(query)

        vars, bindings = flatten_sparql_response(res)
        add_label_to_sparql_resposne(self.label_store, vars=vars, bindings=bindings)

        return QAData(vars=vars, bindings=bindings)

    def find_dataCentres(self, constraints: Optional[DataCentreConstraints] = None, limit: Optional[int] = None):
        query = self.sparql_maker.find_dataCentres(constraints, limit)
        res = self.ontop_client.query(query)

        vars, bindings = flatten_sparql_response(res)
        add_label_to_sparql_resposne(self.label_store, vars=vars, bindings=bindings)

        return QAData(vars=vars, bindings=bindings)

    def count_dataCentres(self):
        query = self.sparql_maker.count_dataCentres()
        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)
        return QAData(vars=vars, bindings=bindings)

    def compute_aggregate_dataCentres_attribute(
        self, attr_agg: Tuple[DataCentreAttrKey, AggregateOperator]
    ):
        query = self.compute_aggregate_dataCentres_attribute(attr_agg)
        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)
        return QAData(vars=vars, bindings=bindings)


def get_sgDataCentres_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    label_store: Annotated[LabelStore, Depends(get_sgDataCentres_labesStore)],
    sparql_maker: Annotated[
        SGDataCentresSPARQLMaker, Depends(get_sgDataCentres_sparqlMaker)
    ],
):
    return SGDataCentresAgent(
        ontop_client=ontop_client, label_store=label_store, sparql_maker=sparql_maker
    )
