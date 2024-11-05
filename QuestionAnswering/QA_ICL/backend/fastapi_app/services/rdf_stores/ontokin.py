from collections import defaultdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from rdflib import URIRef
from constants.namespace import ONTOKIN
from model.kg.ontokin import (
    OntokinArrheniusModel,
    OntokinKineticModelBase,
    OntokinLindemannModel,
    OntokinMechanismBase,
    OntokinMultiArrheniusModel,
    OntokinPDepArrheniusModel,
    OntokinReactionBase,
    OntokinThermoModel,
    OntokinThreeBodyReactionModel,
    OntokinTransportModel,
    OntokinTroeModel,
)
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient, get_ontokin_endpoint


class OntokinRDFStore(Cls2NodeGetter, RDFStore):
    KINETIC_MODEL_URI2CLS: dict[URIRef, type[OntokinKineticModelBase]] = {
        ONTOKIN.ArrheniusModel: OntokinArrheniusModel,
        ONTOKIN.PDepArrheniusModel: OntokinPDepArrheniusModel,
        ONTOKIN.MultiArrheniusModel: OntokinMultiArrheniusModel,
        ONTOKIN.ThreeBodyReactionModel: OntokinThreeBodyReactionModel,
        ONTOKIN.LindemannModel: OntokinLindemannModel,
        ONTOKIN.TroeModel: OntokinTroeModel,
    }

    @property
    def cls2getter(self):
        return {
            "okin:ReactionMechanism": self.get_mechanisms,
            "ocape:ChemicalReaction": self.get_reactions,
            "okin:KineticModel": self.get_kinetic_models,
            "okin:ThermoModel": self.get_thermo_models,
            "okin:TransportModel": self.get_transport_models,
        }

    def get_mechanisms(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntokinMechanismBase, iris)

    def get_reactions(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntokinReactionBase, iris)

    def get_kinetic_models(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        if not iris:
            lst: list[OntokinKineticModelBase | None] = []
            return lst

        query = """SELECT * 
WHERE {{
    VALUES ?KineticModel {{ {values} }}
    ?KineticModel a ?Type .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        type2iris: defaultdict[str, list[str]] = defaultdict(list)
        for binding in bindings:
            type2iris[binding["Type"]].append(binding["KineticModel"])

        iri2model: dict[str, OntokinKineticModelBase] = dict()
        for type, same_type_iris in type2iris.items():
            model_cls = self.KINETIC_MODEL_URI2CLS.get(URIRef(type))
            if not model_cls:
                continue
            models = self.get_many(model_cls, iris=same_type_iris)
            iri2model.update(
                {iri: model for iri, model in zip(same_type_iris, models) if model}
            )

        return [iri2model.get(iri) for iri in iris]

    def get_thermo_models(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntokinThermoModel, iris)

    def get_transport_models(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntokinTransportModel, iris)


@cache
def get_ontokin_rdfStore(endpoint: Annotated[str, Depends(get_ontokin_endpoint)]):
    return OntokinRDFStore(endpoint)
