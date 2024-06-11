from functools import lru_cache
from typing import Annotated

from fastapi import Depends
from pydantic import TypeAdapter

from model.mol_vis import MoleculeGeometry
from services.kg import KgClient, get_ontospecies_bgClient


class XYZManager:
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client
        self.geom_adapter = TypeAdapter(MoleculeGeometry)

    @lru_cache(maxsize=128)
    def get(self, iri: str):
        query = """PREFIX gc: <http://purl.org/gc/> 
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?symbol ?x ?y ?z
WHERE {{
    VALUES ?Species {{ <{IRI}> }}
    ?Species gc:hasAtom [
        gc:isElement/os:hasElementSymbol/os:value ?symbol ;
        os:hasXCoordinate/os:value ?x ;
        os:hasYCoordinate/os:value ?y ;
        os:hasZCoordinate/os:value ?z
    ] .
}}""".format(
            IRI=iri
        )

        _, bindings = self.bg_client.querySelectThenFlatten(query)
        geom_data = self.geom_adapter.validate_python({"atoms": bindings})
        return geom_data.to_xyz_str()


def get_xyz_manager(bg_client: Annotated[KgClient, Depends(get_ontospecies_bgClient)]):
    return XYZManager(bg_client=bg_client)
