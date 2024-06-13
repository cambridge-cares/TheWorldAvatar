from collections import defaultdict
from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends
from rdflib import URIRef
from constants.namespace import ONTOKIN
from model.ontokin import (
    OntokinArrheniusModel,
    OntokinKineticModel,
    OntokinLindemannModel,
    OntokinMechanism,
    OntokinMultiArrheniusModel,
    OntokinPDepArrheniusModel,
    OntokinReaction,
    OntokinThreeBodyReactionModel,
    OntokinTroeModel,
)
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2GetterRDFStore
from services.sparql import SparqlClient, get_ontokin_endpoint


class OntokinRDFStore(Cls2GetterRDFStore):
    def __init__(self, ontokin_endpoint: str):
        self.rdf_store = RDFStore(ontokin_endpoint)
        self.sparql_client = SparqlClient(ontokin_endpoint)

    @property
    def cls2getter(self):
        return {
            "okin:ReactionMechanism": self.get_mechanisms,
            "ocape:ChemicalReaction": self.get_reactions,
            "okin:KineticModel": self.get_kinetic_models,
        }

    def get_mechanisms(self, iris: Sequence[str]):
        return self.rdf_store.getMany(OntokinMechanism, iris)

    def get_reactions(self, iris: Sequence[str]):
        return self.rdf_store.getMany(OntokinReaction, iris)

    def get_kinetic_models(self, iris: Sequence[str]):
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

        iri2model: dict[str, OntokinKineticModel] = dict()
        for type, same_type_iris in type2iris.items():
            type = URIRef(type)
            if type == ONTOKIN.ArrheniusModel:
                model_cls = OntokinArrheniusModel
            elif type == ONTOKIN.PDepArrheniusModel:
                model_cls = OntokinPDepArrheniusModel
            elif type == ONTOKIN.MultiArrheniusModel:
                model_cls = OntokinMultiArrheniusModel
            elif type == ONTOKIN.ThreeBodyReactionModel:
                model_cls = OntokinThreeBodyReactionModel
            elif type == ONTOKIN.LindemannModel:
                model_cls = OntokinLindemannModel
            elif type == ONTOKIN.TroeModel:
                model_cls = OntokinTroeModel
            else:
                continue
            models = self.rdf_store.getMany(model_cls, iris=same_type_iris)
            iri2model.update({iri: model for iri, model in zip(same_type_iris, models)})

        return [iri2model.get(iri) for iri in iris]


@cache
def get_ontokin_rdfStore(endpoint: Annotated[str, Depends(get_ontokin_endpoint)]):
    return OntokinRDFStore(endpoint)
