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

    def get(self, cls: str, iris: list[str]):
        if cls == "os:Species":
            vis_data = self.xyz_manager.get(iris)
            models = self.ontospecies_store.get_species(iris)
            labels = [model.label if model else None for model in models]
        elif cls in ["zeo:ZeoliteFramework", "zeo:ZeoliteMaterial"]:
            vis_data = self.cif_manager.get(iris)
            if cls == "zeo:ZeoliteFramework":
                models = self.ontozeolite_store.get_zeolite_frameworks(iris)
                labels = [model.framework_code if model else None for model in models]
            else:
                models = self.ontozeolite_store.get_zeolitic_materials(iris)
                labels = [model.chemical_formula if model else None for model in models]
        else:
            vis_data = [None for _ in iris]
            labels = [None for _ in iris]

        return [
            (
                ChemicalStructureData(type="xyz", label=label, iri=iri, data=vis_datum)
                if vis_datum
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
