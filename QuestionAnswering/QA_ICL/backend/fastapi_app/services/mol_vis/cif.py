from functools import cache, lru_cache
from typing import Annotated

from fastapi import Depends
from pydantic import TypeAdapter

from model.mol_vis import AtomFracCoords, CrystalInfo, UnitCellParams
from services.kg import KgClient, get_ontozeolite_bgClient


class CIFManager:
    ATOM_SITE_LABEL_MAPPINGS = {"T": "Si", "Hfix": "H", "HO": "O"}

    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client
        self.unit_cell_adapter = TypeAdapter(UnitCellParams)
        self.atoms_adapter = TypeAdapter(list[AtomFracCoords])

    def _process_atom_site_label(self, label: str):
        label = "".join(c for c in label if c.isalpha())
        return self.ATOM_SITE_LABEL_MAPPINGS.get(label, label)

    @lru_cache(maxsize=128)
    def get(self, iri: str):
        query = f"""PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>

SELECT ?name ?a ?b ?c ?alpha ?beta ?gamma
WHERE {{
    VALUES ?zeo {{ <{iri}> }}
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
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        if not bindings:
            return None

        binding = bindings[0]
        name = binding["name"]
        unit_cell_params = self.unit_cell_adapter.validate_python(binding)

        query = f"""PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>

SELECT ?x ?y ?z ?symbol
WHERE {{
    VALUES ?zeo {{ <{iri}> }}
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
}}"""
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        atoms = self.atoms_adapter.validate_python(bindings)
        for atom in atoms:
            atom.symbol = self._process_atom_site_label(atom.symbol)

        crystal_info = CrystalInfo(name=name, unit_cell=unit_cell_params, atoms=atoms)
        return crystal_info.to_cif_str()


@cache
def get_cif_manager(bg_client: Annotated[KgClient, Depends(get_ontozeolite_bgClient)]):
    return CIFManager(bg_client=bg_client)
