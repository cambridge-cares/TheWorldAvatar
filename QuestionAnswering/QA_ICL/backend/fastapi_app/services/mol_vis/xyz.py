from functools import cache
from typing import Annotated

from fastapi import Depends

from constants.periodictable import ATOMIC_NUMBER_TO_SYMBOL
from model.pubchem import PubChemPUGResponse
from services.requests import request_get_obj
from model.mol_vis import MoleculeGeometry
from services.sparql import SparqlClient, get_ontospecies_endpoint


class XYZManager:
    def __init__(self, ontospecies_endpoint: str):
        self.sparql_client = SparqlClient(ontospecies_endpoint)

    def get(self, iris: list[str]):
        return self.get_from_pubchem(iris)

    def get_from_pubchem(self, iris: list[str]):
        if not iris:
            return [None for _ in iris]

        unique_iris = list(set(iris))

        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT *
WHERE {{
    VALUES ?Species {{ {iris} }} 
    ?Species os:hasCID/os:value ?CID .
}}""".format(
            iris=" ".join(f"<{iri}>" for iri in unique_iris)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        iri2cid = {binding["Species"]: int(binding["CID"]) for binding in bindings}
        if not iri2cid:
            return [None for _ in iris]

        unique_cids = list(set(iri2cid.values()))

        url = (
            "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/{cids}/JSON".format(
                cids=",".join(str(x) for x in unique_cids)
            )
        )
        res = request_get_obj(
            url=url,
            params={"record_type": "3d"},
            response_type=PubChemPUGResponse,
        )

        cid2atoms = dict()
        for compound in res.PC_Compounds:
            try:
                conformer = compound.coords[0].conformers[0]
                symbols = [ATOMIC_NUMBER_TO_SYMBOL[x] for x in compound.atoms.element]
                atoms = [
                    {"symbol": symbol, "x": x, "y": y, "z": z}
                    for symbol, x, y, z in zip(
                        symbols, conformer.x, conformer.y, conformer.z
                    )
                ]
                cid2atoms[compound.id.id.cid] = atoms
            except:
                pass

        cid2xyz = {
            cid: MoleculeGeometry.model_validate(
                {"atoms": atoms, "comment": "Generated from PubChem"}
            ).to_xyz_str()
            for cid, atoms in cid2atoms.items()
        }
        return [cid2xyz.get(iri2cid.get(iri)) for iri in iris]

    def get_from_ontospecies(self, iris: list[str]):
        unique_iris = list(set(iris))

        query = """PREFIX gc: <http://purl.org/gc/> 
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT *
WHERE {{
    VALUES ?Species {{ {iris} }}
    ?Species gc:hasAtom [
        gc:isElement/os:hasElementSymbol/os:value ?symbol ;
        os:hasXCoordinate/os:value ?x ;
        os:hasYCoordinate/os:value ?y ;
        os:hasZCoordinate/os:value ?z
    ] .
}}""".format(
            iris=" ".join(f"<{iri}>" for iri in unique_iris)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        iri2binding = {binding["Species"]: binding for binding in bindings}
        iri2geom = {
            iri: MoleculeGeometry.model_validate(
                {"atoms": binding, "comment": "Generated from SPARQL query"}
            )
            for iri, binding in iri2binding.items()
        }
        return [iri2geom.get(iri) for iri in iris]


@cache
def get_xyz_manager(
    endpoint: Annotated[SparqlClient, Depends(get_ontospecies_endpoint)]
):
    return XYZManager(ontospecies_endpoint=endpoint)
