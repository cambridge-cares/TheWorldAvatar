from collections import defaultdict
from functools import cache
from typing import Annotated

from fastapi import Depends

from model.qa import ChemicalStructureData
from services.mol_vis.cif import CIFManager, get_cif_manager
from services.mol_vis.xyz import XYZManager, get_xyz_manager
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)


class VisualisationDataStore:
    def __init__(
        self,
        xyz_manager: XYZManager,
        cif_manager: CIFManager,
        ontospecies_store: OntospeciesRDFStore,
        ontozeolite_store: OntozeoliteRDFStore,
    ):
        self.xyz_manager = xyz_manager
        self.cif_manager = cif_manager
        self.ontospecies_store = ontospecies_store
        self.ontozeolite_store = ontozeolite_store

    def get(self, cls: str | None | list[str | None], iris: list[str]):
        if isinstance(cls, str):
            return self._get(cls, iris)

        cls2iris: defaultdict[str, list[str]] = defaultdict(list)
        for c, iri in zip(cls, iris):
            cls2iris[c].append(iri)

        iri2datum: dict[str, ChemicalStructureData | None] = dict()
        for c, same_type_iris in cls2iris.items():
            data = self._get(cls=c, iris=same_type_iris)
            iri2datum.update({iri: datum for iri, datum in zip(same_type_iris, data)})

        return [iri2datum.get(iri) for iri in iris]

    def _get(self, cls: str, iris: list[str]):
        if cls == "os:Species":
            type = "xyz"
            vis_data = self.xyz_manager.get(iris)
            models = self.ontospecies_store.get_species(iris)
            labels = [
                (
                    "{} ({})".format(model.label, model.IUPAC_name.value)
                    if model.IUPAC_name
                    else model.label if model else None
                )
                for model in models
            ]
        elif cls in ["zeo:ZeoliteFramework", "zeo:ZeoliticMaterial"]:
            type = "cif"
            vis_data = self.cif_manager.get(iris)
            if cls == "zeo:ZeoliteFramework":
                models = self.ontozeolite_store.get_zeolite_frameworks(iris)
                labels = [model.code if model else None for model in models]
            else:
                models = self.ontozeolite_store.get_zeolitic_materials(iris)
                labels = [model.chemical_formula if model else None for model in models]
        else:
            type = None
            vis_data = [None for _ in iris]
            labels = [None for _ in iris]

        return [
            (
                ChemicalStructureData(type=type, label=label, iri=iri, data=vis_datum)
                if vis_datum and type
                else None
            )
            for iri, vis_datum, label in zip(iris, vis_data, labels)
        ]


@cache
def get_visData_store(
    xyz_manager: Annotated[XYZManager, Depends(get_xyz_manager)],
    cif_manager: Annotated[CIFManager, Depends(get_cif_manager)],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
):
    return VisualisationDataStore(
        xyz_manager=xyz_manager,
        cif_manager=cif_manager,
        ontospecies_store=ontospecies_store,
        ontozeolite_store=ontozeolite_store,
    )
