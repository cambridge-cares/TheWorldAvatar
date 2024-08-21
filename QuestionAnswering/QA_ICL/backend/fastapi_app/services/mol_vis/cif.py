from collections import defaultdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from pydantic import TypeAdapter

from constants.namespace import ONTOCRYSTAL, ONTOZEOLITE
from model.mol_vis import CIFAtomSite, CIF, CIFUnitCell
from services.sparql import SparqlClient, get_ontozeolite_endpoint


class CIFManager:
    ATOM_SITE_LABEL_MAPPINGS = {"T": "Si", "Hfix": "H", "HO": "O"}

    def __init__(self, ontozeolite_endpoint: str):
        self.sparql_client = SparqlClient(ontozeolite_endpoint)
        self.atoms_adapter = TypeAdapter(list[CIFAtomSite])

    def _atom_site_label2symbol(self, label: str):
        label = "".join(c for c in label if c.isalpha())
        return self.ATOM_SITE_LABEL_MAPPINGS.get(label, label)

    def get(self, iris: list[str]):
        none_lst: list[str | None] = [None for _ in iris]
        if not iris:
            return none_lst

        unique_iris = set(iris)

        query = f"""PREFIX zeo: <{ONTOZEOLITE}>
PREFIX ocr: <{ONTOCRYSTAL}>

SELECT ?zeo ?name ?a ?b ?c ?alpha ?beta ?gamma
WHERE {{
    VALUES ?zeo {{ {" ".join(f"<{iri}>" for iri in unique_iris)} }}
    ?zeo zeo:hasFrameworkCode|zeo:hasChemicalFormula ?name .
    ?zeo ocr:hasCrystalInformation/ocr:hasUnitCell [
        ocr:hasUnitCellLengths/ocr:hasVectorComponent [
            ocr:hasComponentLabel "a" ; 
            ocr:hasComponentValue ?a
        ], [
            ocr:hasComponentLabel "b" ;
            ocr:hasComponentValue ?b
        ], [
            ocr:hasComponentLabel "c" ;
            ocr:hasComponentValue ?c
        ] ;
        ocr:hasUnitCellAngles/ocr:hasVectorComponent [
            ocr:hasComponentLabel "alpha" ;
            ocr:hasComponentValue ?alpha
        ], [
            ocr:hasComponentLabel "beta" ;
            ocr:hasComponentValue ?beta
        ], [
            ocr:hasComponentLabel "gamma" ;
            ocr:hasComponentValue ?gamma
        ]
    ] .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        iri2name = {binding["zeo"]: binding["name"] for binding in bindings}
        iri2unitcell = {
            binding["zeo"]: CIFUnitCell.model_validate(binding) for binding in bindings
        }

        query = f"""PREFIX zeo: <{ONTOZEOLITE}>
PREFIX ocr: <{ONTOCRYSTAL}>

SELECT ?zeo ?fract_x ?fract_y ?fract_z ?label
WHERE {{
    VALUES ?zeo {{ {" ".join(f"<{iri}>" for iri in unique_iris)} }}
    ?zeo ocr:hasCrystalInformation/ocr:hasAtomicStructure/ocr:hasAtomSite ?AtomSite .
    ?AtomSite ocr:hasFractionalPosition/ocr:hasVectorComponent [
        ocr:hasComponentLabel "x" ; 
        ocr:hasComponentValue ?fract_x
    ], [
        ocr:hasComponentLabel "y" ; 
        ocr:hasComponentValue ?fract_y
    ], [
        ocr:hasComponentLabel "z" ; 
        ocr:hasComponentValue ?fract_z
    ] .
    OPTIONAL {{
        ?AtomSite ocr:hasAtomSiteLabel ?label .
    }}
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        iri2atoms = defaultdict(list)
        for binding in bindings:
            binding["symbol"] = self._atom_site_label2symbol(binding["label"])
            iri2atoms[binding["zeo"]].append(CIFAtomSite.model_validate(binding))

        def make_cif(data: dict):
            try:
                return CIF.model_validate(data).to_cif_str()
            except:
                return None

        iri2cif = {
            iri: make_cif(
                {
                    "name": iri2name.get(iri),
                    "unit_cell": iri2unitcell.get(iri),
                    "atoms": iri2atoms[iri],
                }
            )
            for iri in unique_iris
        }

        return [iri2cif.get(iri) for iri in iris]


@cache
def get_cif_manager(endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]):
    return CIFManager(ontozeolite_endpoint=endpoint)
