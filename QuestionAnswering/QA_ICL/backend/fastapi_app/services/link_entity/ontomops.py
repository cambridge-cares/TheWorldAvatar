from functools import cache
import logging
from typing import Annotated

from fastapi import Depends
from constants.namespace import ONTOMOPS
from model.entity_linking.ontomops import (
    AMLinkingArgs,
    CBULinkingArgs,
    GBULinkingArgs,
    MOPLinkingArgs,
)
from services.sparql import SparqlClient, get_ontomops_endpoint
from .base import LinkerManager


logger = logging.getLogger(__name__)


class OntomopsLinkerManager(LinkerManager):
    def __init__(self, ontomops_endpoint: str):
        self.sparql_client = SparqlClient(ontomops_endpoint)

    @property
    def cls2linker(self):
        return {
            "mops:MetalOrganicPolyhedron": self.linkMOP,
            "mops:ChemicalBuildingUnit": self.linkCBU,
            "mops:GenericBuildingUnit": self.linkGBU,
            "mops:AssemblyModel": self.linkAM,
        }

    def linkMOP(self, text: str | None, **kwargs):
        try:
            args = MOPLinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(
            f"Linking metal organic polyhedra with args: {args.model_dump_json()}"
        )

        query = f"""PREFIX mops: <{ONTOMOPS}>
        
SELECT DISTINCT *
WHERE {{
    ?MOP mops:hasMOPFormula "{args.formula}"
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["MOP"] for binding in bindings]
        return iris

    def linkCBU(self, text: str | None, **kwargs):
        try:
            args = CBULinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(
            f"Linking chemical building unit with args: {args.model_dump_json()}"
        )

        query = f"""PREFIX mops: <{ONTOMOPS}>
        
SELECT DISTINCT *
WHERE {{
    ?CBU mops:hasCBUFormula "{args.formula}"
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["CBU"] for binding in bindings]
        return iris

    def linkGBU(self, text: str | None, **kwargs):
        logger.info(f"Linking generic building unit with args: {kwargs}")
        try:
            args = GBULinkingArgs.model_validate(kwargs)
        except Exception as e:
            logger.error(
                f"Invalid linking args for generic building unit with error: {e}"
            )
            lst: list[str] = []
            return lst

        query = f"""PREFIX mops: <{ONTOMOPS}>
        
SELECT DISTINCT *
WHERE {{
    ?GBU mops:hasModularity {args.modularity} ; mops:hasPlanarity "{args.planarity}" .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["GBU"] for binding in bindings]
        return iris

    def linkAM(self, text: str | None, **kwargs):
        try:
            args = AMLinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(f"Linking assembly model with args: {args.model_dump_json()}")

        clauses_GBU = [
            *(
                triple
                for i, gbu_args in enumerate(args.GBU)
                for triple in [
                    f"?AM mops:hasGenericBuildingUnit ?GBU{i} ; mops:hasGenericBuildingUnitNumber ?GBUNum{i} .",
                    f'?GBU{i} mops:hasModularity {gbu_args.modularity} ; mops:hasPlanarity "{gbu_args.planarity}" .',
                    f"?GBUNum{i} mops:hasUnitNumberValue {gbu_args.num} .",
                    f"?GBUNum{i} mops:isNumberOf ?GBU{i} .",
                ]
            ),
            (
                """FILTER NOT EXISTS {{
        {}
    }}""".format(
                    "\n        ".join(
                        [
                            f"?AM mops:hasGenericBuildingUnit ?GBUExclude .",
                            "FILTER ( ?GBUExclude NOT IN ( {} ) )".format(
                                ", ".join(f"?GBU{i}" for i in range(len(args.GBU)))
                            ),
                        ]
                    )
                )
                if args.GBU
                else None
            ),
        ]
        clause_sym_pt_grp = (
            f'?AM mops:hasSymmetryPointGroup "{args.symmetry_point_group}" .'
            if args.symmetry_point_group
            else None
        )
        clauses = [x for x in [*clauses_GBU, clause_sym_pt_grp] if x]

        query = """PREFIX mops: <{mops}>

SELECT DISTINCT ?AM
WHERE {{
    {clauses}
}}""".format(
            mops=ONTOMOPS, clauses="\n    ".join(clauses)
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["AM"] for binding in bindings]
        return iris


@cache
def get_ontomops_linkerManager(
    ontomops_endpoint: Annotated[str, Depends(get_ontomops_endpoint)]
):
    return OntomopsLinkerManager(ontomops_endpoint=ontomops_endpoint)
