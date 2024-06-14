from collections import defaultdict
from functools import cache
from typing import Annotated

from fastapi import Depends
from pydantic import TypeAdapter

from model.mol_vis import AtomFracCoords, CrystalInfo, UnitCellParams
from services.sparql import SparqlClient, get_ontozeolite_endpoint


class CIFManager:
    ATOM_SITE_LABEL_MAPPINGS = {"T": "Si", "Hfix": "H", "HO": "O"}

    def __init__(self, ontozeolite_endpoint: str):
        self.sparql_client = SparqlClient(ontozeolite_endpoint)
        self.atoms_adapter = TypeAdapter(list[AtomFracCoords])

    def _process_atom_site_label(self, label: str):
        label = "".join(c for c in label if c.isalpha())
        return self.ATOM_SITE_LABEL_MAPPINGS.get(label, label)

    def get(self, iris: list[str]):
        if not iris:
            return [None for _ in iris]

        unique_iris = list(set(iris))

        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>

SELECT ?zeo ?name ?a ?b ?c ?alpha ?beta ?gamma
WHERE {{
    VALUES ?zeo {{ {iris} }}
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
}}""".format(
            iris=" ".join(f"<{iri}>" for iri in unique_iris)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        iri2name = {binding["zeo"]: binding["name"] for binding in bindings}
        iri2unitcell = {
            binding["zeo"]: UnitCellParams.model_validate(binding)
            for binding in bindings
        }

        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>

SELECT ?x ?y ?z ?symbol
WHERE {{
    VALUES ?zeo {{ {iris} }}
    ?zeo ocr:hasCrystalInformation/ocr:hasAtomicStructure/ocr:hasAtomSite ?AtomSite .
    ?AtomSite ocr:hasFractionalPosition/ocr:hasVectorComponent [
        ocr:hasComponentLabel "x" ; 
        ocr:hasComponentValue ?x
    ], [
        ocr:hasComponentLabel "y" ; 
        ocr:hasComponentValue ?y
    ], [
        ocr:hasComponentLabel "z" ; 
        ocr:hasComponentValue ?z
    ] .
    OPTIONAL {{
        ?AtomSite ocr:hasAtomSiteLabel ?symbol .
    }}
}}""".format(
            iris=" ".join(f"<{iri}>" for iri in unique_iris)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        iri2atoms = defaultdict(list)
        for binding in bindings:
            atom = AtomFracCoords.model_validate(binding)
            atom.symbol = self._process_atom_site_label(atom.symbol)
            iri2atoms[binding["zeo"]].append(atom)

        def make_cif(data: dict):
            try:
                return CrystalInfo.model_validate(data).to_cif_str()
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
