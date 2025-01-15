from collections import defaultdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from rdflib import URIRef

from constants.namespace import GC, ONTOCOMPCHEM, ONTOSPECIES
from model.kg.ontocompchem import (
    OntocompchemCalculationResult,
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
from model.kg.ontospecies import GcAtom, OntospeciesHasValueHasUnit
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from services.sparql import (
    SparqlClient,
    get_ontocompchem_endpoint,
    get_ontospecies_endpoint,
)


class OntocompchemRDFStore(Cls2NodeGetter, RDFStore):
    URI2CLS: dict[URIRef, type[OntocompchemCalculationResult]] = {
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

    def __init__(
        self,
        ontocompchem_endpoint: str,
        ontospecies_endpoint: str,
        ontospecies_store: OntospeciesRDFStore,
    ):
        super().__init__(ontocompchem_endpoint)
        self.ontospecies_client = SparqlClient(ontospecies_endpoint)
        self.ontospecies_store = ontospecies_store

    @property
    def cls2getter(self):
        return {
            "occ:CalculationResult": self.get_calculation_results,
        }

    def get_optimized_geometries(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        query = f"""PREFIX os: <{ONTOSPECIES}>

SELECT DISTINCT ?OptimizedGeometry ?Atom ?X ?Xvalue ?Xunit ?Y ?Yvalue ?Yunit ?Z ?Zvalue ?Zunit
WHERE {{
VALUES ?OptimizedGeometry {{ {" ".join(f"<{iri}>" for iri in iris)} }}
    ?OptimizedGeometry ^os:fromGeometry ?X, ?Y, ?Z .
    ?Atom os:hasXCoordinate ?X ;
        os:hasYCoordinate ?Y ;
        os:hasZCoordinate ?Z .
    ?X os:value ?Xvalue ; os:unit ?Xunit .
    ?Y os:value ?Yvalue ; os:unit ?Yunit .
    ?Z os:value ?Zvalue ; os:unit ?Zunit .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iri2atoms: defaultdict[
            str,
            list[
                tuple[
                    str,
                    OntospeciesHasValueHasUnit,
                    OntospeciesHasValueHasUnit,
                    OntospeciesHasValueHasUnit,
                ]
            ],
        ] = defaultdict(list)
        for binding in bindings:
            iri2atoms[binding["OptimizedGeometry"]].append(
                (
                    binding["Atom"],
                    *(
                        OntospeciesHasValueHasUnit(
                            IRI=binding[x],
                            value=binding[f"{x}value"],
                            unit=binding[f"{x}unit"],
                        )
                        for x in ["X", "Y", "Z"]
                    ),
                )
            )

        query = f"""PREFIX gc: <{GC}>

SELECT *
WHERE {{
VALUES ?Atom {{ {" ".join(
                f"<{iri}>" for atoms in iri2atoms.values() for iri, *_ in atoms
            )} }}
?Atom gc:isElement ?Element .
}}"""
        _, bindings = self.ontospecies_client.querySelectThenFlatten(query)
        atom2element = {binding["Atom"]: binding["Element"] for binding in bindings}

        element_iris = list(atom2element.values())
        element_models = self.ontospecies_store.get_elements_many(iris=element_iris)
        element_iri2model = {
            iri: model for iri, model in zip(element_iris, element_models) if model
        }
        atom2element = {
            atom: element_iri2model.get(element)
            for atom, element in atom2element.items()
        }

        return [
            (
                OntocompchemOptimizedGeometry(
                    IRI=iri,
                    atom=[
                        GcAtom(
                            IRI=atom_iri,
                            x=x,
                            y=y,
                            z=z,
                            element=atom2element[atom_iri],
                        )
                        for atom_iri, x, y, z in iri2atoms[iri]
                        if atom_iri in atom2element
                    ],
                )
                if iri in iri2atoms
                else None
            )
            for iri in iris
        ]

    def get_calculation_results(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        if not iris:
            lst: list[OntocompchemCalculationResult | None] = []
            return lst

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

        iri2model: dict[str, OntocompchemCalculationResult] = dict()
        for type, same_type_iris in type2iris.items():
            type = URIRef(type)
            model_cls = self.URI2CLS.get(type)
            if model_cls is None:
                continue
            if model_cls is OntocompchemOptimizedGeometry:
                models = self.get_optimized_geometries(iris=same_type_iris)
            else:
                models = self.get_many(model_cls, iris=same_type_iris)
            iri2model.update(
                {iri: model for iri, model in zip(same_type_iris, models) if model}
            )

        return [iri2model.get(iri) for iri in iris]


@cache
def get_ontocompchem_rdfStore(
    ontocompchem_endpoint: Annotated[str, Depends(get_ontocompchem_endpoint)],
    ontospecies_endpoint: Annotated[str, Depends(get_ontospecies_endpoint)],
    ontospecies_store: Annotated[str, Depends(get_ontospecies_rdfStore)],
):
    return OntocompchemRDFStore(
        ontocompchem_endpoint=ontocompchem_endpoint,
        ontospecies_endpoint=ontospecies_endpoint,
        ontospecies_store=ontospecies_store,
    )
