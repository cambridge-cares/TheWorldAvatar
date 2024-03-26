from functools import cache, lru_cache
from typing import Annotated, List, Optional, Tuple

from fastapi import Depends

from model.qa import QAData
from model.aggregate import AggregateOperator
from services.utils.rdf import add_label_to_sparql_resposne, flatten_sparql_response
from services.core.kg import KgClient
from services.core.label_store import LabelStore
from services.connectors.sg import get_sg_ontopClient
from services.connectors.sg_companies.agent.label_store import (
    get_sgCompanies_labesStore,
)
from ..model import DataCentreAttrKey, DataCentreConstraints
from .make_sparql import SGDataCentresSPARQLMaker, get_sgDataCentres_sparqlMaker
from .label_store import get_sgDataCentres_labelStore


class SGDataCentresAgent:
    def __init__(
        self,
        ontop_client: KgClient,
        company_label_store: LabelStore,
        data_centre_label_store: LabelStore,
        sparql_maker: SGDataCentresSPARQLMaker,
    ):
        self.ontop_client = ontop_client
        self.company_label_store = company_label_store
        self.data_centre_label_store = data_centre_label_store
        self.sparql_maker = sparql_maker

    def _lookup_dataCentres(self, company_iris: List[str]):
        query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT ?DataCentre WHERE {{
?Company ontocompany:hasDataCentre ?DataCentre .
FILTER ( ?Company IN ({companies}) )
}}""".format(
            companies=", ".join(["<{iri}>".format(iri=iri) for iri in company_iris])
        )
        res = self.ontop_client.query(query)
        return [x["DataCentre"]["value"] for x in res["results"]["bindings"]]

    @lru_cache(maxsize=128)
    def _lookup_company_name(self, data_centre_iri: str):
        query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT ?CompanyLabel WHERE {{
VALUES ?DataCentre {{ <{iri}> }}
?Company ontocompany:hasDataCentre ?DataCentre .
?Company rdfs:label ?CompanyLabel .
}}""".format(
            iri=data_centre_iri)
        res = self.ontop_client.query(query)
        bindings = res["results"]["bindings"]
        if bindings:
            return bindings[0]["CompanyLabel"]["value"]
        return None

    def _add_company_label(self, vars: List[str], bindings: List[dict]):
        try:
            iri_idx = vars.index("IRI")
        except ValueError:
            raise ValueError("IRI must be present, found: " + str(vars))

        vars.insert(iri_idx + 1, "Company")
        for binding in bindings:
            binding["Company"] = self._lookup_company_name(binding["IRI"])

    def lookup_dataCentre_attribute(
        self,
        attr_key: DataCentreAttrKey,
        company: Optional[str] = None,
        name: Optional[str] = None,
    ):
        if company is None and name is None:
            raise ValueError("Either `company` or name must be present.")

        iris = []
        if company:
            company_iris = self.company_label_store.link_entity(company)
            data_centre_iris = self._lookup_dataCentres(company_iris)
            iris.extend(data_centre_iris)
        if name:
            data_centre_iris = self.data_centre_label_store.link_entity(name)
            iris.extend(data_centre_iris)

        query = self.sparql_maker.lookup_dataCentre_attribute(
            iris=iris, attr_key=attr_key
        )
        res = self.ontop_client.query(query)

        vars, bindings = flatten_sparql_response(res)
        add_label_to_sparql_resposne(
            self.data_centre_label_store,
            vars=vars,
            bindings=bindings,
            label_header="DataCentreName",
        )
        self._add_company_label(vars=vars, bindings=bindings)

        return QAData(vars=vars, bindings=bindings)

    def find_dataCentres(
        self,
        constraints: Optional[DataCentreConstraints] = None,
        limit: Optional[int] = None,
    ):
        query = self.sparql_maker.find_dataCentres(constraints, limit)
        res = self.ontop_client.query(query)

        vars, bindings = flatten_sparql_response(res)
        add_label_to_sparql_resposne(
            self.data_centre_label_store,
            vars=vars,
            bindings=bindings,
            label_header="DataCentreName",
        )
        self._add_company_label(vars=vars, bindings=bindings)

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
    company_label_store: Annotated[LabelStore, Depends(get_sgCompanies_labesStore)],
    data_centre_label_store: Annotated[
        LabelStore, Depends(get_sgDataCentres_labelStore)
    ],
    sparql_maker: Annotated[
        SGDataCentresSPARQLMaker, Depends(get_sgDataCentres_sparqlMaker)
    ],
):
    return SGDataCentresAgent(
        ontop_client=ontop_client,
        company_label_store=company_label_store,
        data_centre_label_store=data_centre_label_store,
        sparql_maker=sparql_maker,
    )
