from functools import cache, lru_cache
from typing import Annotated

from fastapi import Depends
from pydantic import TypeAdapter

from model.mol_vis import MoleculeGeometry
from services.sparql import SparqlClient, get_ontospecies_endpoint


class XYZManager:
    def __init__(self, ontospecies_endpoint: str):
        self.sparql_client = SparqlClient(ontospecies_endpoint)
        self.geom_adapter = TypeAdapter(MoleculeGeometry)

    @lru_cache(maxsize=128)
    def get(self, iri: str):
        query = f"""PREFIX gc: <http://purl.org/gc/> 
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?symbol ?x ?y ?z
WHERE {{
    VALUES ?Species {{ <{iri}> }}
    ?Species gc:hasAtom [
        gc:isElement/os:hasElementSymbol/os:value ?symbol ;
        os:hasXCoordinate/os:value ?x ;
        os:hasYCoordinate/os:value ?y ;
        os:hasZCoordinate/os:value ?z
    ] .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        if not bindings:
            return None

        geom_data = self.geom_adapter.validate_python(
            {"atoms": bindings, "comment": "Generated from SPARQL query"}
        )
        return geom_data.to_xyz_str()


@cache
def get_xyz_manager(endpoint: Annotated[SparqlClient, Depends(get_ontospecies_endpoint)]):
    return XYZManager(ontospecies_endpoint=endpoint)
