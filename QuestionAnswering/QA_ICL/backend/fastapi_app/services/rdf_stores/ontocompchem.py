from collections import defaultdict
from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends
from rdflib import URIRef

from constants.namespace import ONTOCOMPCHEM
from model.ontocompchem import (
    OntocompchemFrequencies,
    OntocompchemHOMOEnergy,
    OntocompchemHOMOMinus1Energy,
    OntocompchemHOMOMinus2Energy,
    OntocompchemLUMOEnergy,
    OntocompchemLUMOPlus1Energy,
    OntocompchemLUMOPlus2Energy,
    OntocompchemOptimizedGeometry,
    OntocompchemRotationalConstants,
    OntocompchemRotationalSymmetryNumber,
    OntocompchemSCFEnergy,
    OntocompchemTotalEnergy,
    OntocompchemTotalEnthalpy,
    OntocompchemTotalGibbsFreeEnergy,
    OntocompchemZeroPointEnergy,
)
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2GetterRDFStore
from services.sparql import SparqlClient, get_ontocompchem_endpoint


class OntocompchemRDFStore(Cls2GetterRDFStore):
    URI2CLS = {
        ONTOCOMPCHEM.OptimizedGeometry: OntocompchemOptimizedGeometry,
        ONTOCOMPCHEM.RotationalConstants: OntocompchemRotationalConstants,
        ONTOCOMPCHEM.Frequencies: OntocompchemFrequencies,
        ONTOCOMPCHEM.HOMOEnergy: OntocompchemHOMOEnergy,
        ONTOCOMPCHEM.HOMOMinus1Energy: OntocompchemHOMOMinus1Energy,
        ONTOCOMPCHEM.HOMOMinus2Energy: OntocompchemHOMOMinus2Energy,
        ONTOCOMPCHEM.LUMOEnergy: OntocompchemLUMOEnergy,
        ONTOCOMPCHEM.LUMOPlus1Energy: OntocompchemLUMOPlus1Energy,
        ONTOCOMPCHEM.LUMOPlus2Energy: OntocompchemLUMOPlus2Energy,
        ONTOCOMPCHEM.RotationalSymmetryNumber: OntocompchemRotationalSymmetryNumber,
        ONTOCOMPCHEM.SCFEnergy: OntocompchemSCFEnergy,
        ONTOCOMPCHEM.TotalEnergy: OntocompchemTotalEnergy,
        ONTOCOMPCHEM.TotalEnthalpy: OntocompchemTotalEnthalpy,
        ONTOCOMPCHEM.TotalGibbsFreeEnergy: OntocompchemTotalGibbsFreeEnergy,
        ONTOCOMPCHEM.ZeroPointEnergy: OntocompchemZeroPointEnergy,
    }

    def __init__(self, ontocompchem_endpoint: str):
        self.rdf_store = RDFStore(ontocompchem_endpoint)
        self.sparql_client = SparqlClient(ontocompchem_endpoint)

    @property
    def cls2getter(self):
        return {
            "occ:CalculationResult": self.get_calculation_results,
        }

    def get_calculation_results(self, iris: Sequence[str]):
        query = """SELECT * 
WHERE {{
    VALUES ?iri {{ {iris} }}
    ?iri a ?type .
}}""".format(
            iris=" ".join(f"<{iri}>" for iri in iris)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        type2iris: defaultdict[str, list[str]] = defaultdict(list)
        for binding in bindings:
            type2iris[binding["type"]].append(binding["iri"])

        iri2model = dict()
        for type, same_type_iris in type2iris.items():
            type = URIRef(type)
            model_cls = self.URI2CLS.get(type)
            if model_cls is None:
                continue
            models = self.rdf_store.getMany(model_cls, iris=same_type_iris)
            iri2model.update({iri: model for iri, model in zip(same_type_iris, models)})


@cache
def get_ontocompchem_rdfStore(
    endpoint: Annotated[str, Depends(get_ontocompchem_endpoint)]
):
    return OntocompchemRDFStore(endpoint)
