from functools import cache
from importlib.resources import files
import logging
import os
from typing import Annotated

from fastapi import Depends
from requests import HTTPError
import requests

from config import AppSettings, OntomopsFileserverSettings, get_app_settings
from constants.namespace import GC, ONTOMOPS, ONTOSPECIES
from constants.periodictable import ATOMIC_NUMBER_TO_SYMBOL
from model.pubchem import PubChemPUGResponse
from services.requests import request_get_obj
from model.mol_vis import XYZ
from services.sparql import (
    SparqlClient,
    get_ontomops_endpoint,
    get_ontospecies_endpoint,
)
from utils.itertools_recipes import batched


logger = logging.getLogger(__name__)


class XYZManager:
    PUBCHEM_PUG_BATCHSIZE = 128

    def __init__(
        self,
        ontospecies_endpoint: str,
        ontomops_endpoint: str,
        ontomops_fileserver_username: str,
        ontomops_fileserver_password: str,
    ):
        self.ontospecies_client = SparqlClient(ontospecies_endpoint)
        self.ontomops_client = SparqlClient(ontomops_endpoint)
        self.ontomops_fileserver_username = ontomops_fileserver_username
        self.ontomops_fileserver_password = ontomops_fileserver_password

        self.cid_xyz_cache_path = files("data").joinpath(".xyz_cache/cid")
        os.makedirs(self.cid_xyz_cache_path, exist_ok=True)
        self.cids_with_xyz = set(
            [
                int(f.name[: -len(".xyz")])
                for f in self.cid_xyz_cache_path.iterdir()
                if f.name.endswith(".xyz") and not f.name.endswith(".notfound.xyz")
            ]
        )
        self.cids_no_xyz = set(
            [
                int(f.name[: -len(".notfound.xyz")])
                for f in self.cid_xyz_cache_path.iterdir()
                if f.name.endswith(".notfound.xyz")
            ]
        )

    def get(self, iris: list[str]):
        return self.get_from_pubchem(iris)

    def get_from_pubchem(self, iris: list[str]):
        none_lst: list[str | None] = [None for _ in iris]
        if not iris:
            return none_lst

        query = f"""PREFIX os: <{ONTOSPECIES}>

SELECT DISTINCT *
WHERE {{
    VALUES ?Species {{ {" ".join(f"<{iri}>" for iri in set(iris))} }} 
    ?Species os:hasCID/os:value ?CID .
}}"""
        _, bindings = self.ontospecies_client.querySelectThenFlatten(query)

        iri2cid = {binding["Species"]: int(binding["CID"]) for binding in bindings}
        if not iri2cid:
            return none_lst

        unique_cids = set(iri2cid.values()) - self.cids_no_xyz

        cache_miss: list[int] = list()
        cid2xyz: dict[int, str] = dict()
        for cid in unique_cids:
            if cid not in self.cids_with_xyz:
                cache_miss.append(cid)
                continue
            try:
                xyz = self.cid_xyz_cache_path.joinpath(f"{cid}.xyz").read_text()
            except:
                cache_miss.append(cid)
                continue
            cid2xyz[cid] = xyz

        for batch in batched(cache_miss, n=self.PUBCHEM_PUG_BATCHSIZE):
            url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/{cids}/JSON".format(
                cids=",".join(str(x) for x in batch)
            )
            try:
                res = request_get_obj(
                    url=url,
                    params={"record_type": "3d"},
                    response_type=PubChemPUGResponse,
                )
            except HTTPError as e:
                if e.response.status_code == 404:
                    logger.info(
                        "Recieved a NOT FOUND response from PubChem PUG API: " + str(e)
                    )
                    logger.info(
                        f"Geometry data for CIDs {batch} will be marked as not found."
                    )
                    for cid in batch:
                        with open(
                            str(
                                self.cid_xyz_cache_path.joinpath(f"{cid}.notfound.xyz")
                            ),
                            "w",
                        ):
                            pass
                        self.cids_no_xyz.add(cid)
                    continue
                else:
                    raise e
            except Exception as e:
                logger.error(
                    "An unexpected error is encountered while calling PubChem PUG API: "
                    + str(e)
                )
                logger.info(f"Geometry data for CID {batch} will be ignored")
                continue

            cid2atoms: dict[int, list[dict]] = dict()
            for compound in res.PC_Compounds:
                try:
                    conformer = compound.coords[0].conformers[0]
                    symbols = [
                        ATOMIC_NUMBER_TO_SYMBOL[x] for x in compound.atoms.element
                    ]
                    atoms = [
                        {"symbol": symbol, "x": x, "y": y, "z": z}
                        for symbol, x, y, z in zip(
                            symbols, conformer.x, conformer.y, conformer.z
                        )
                    ]
                    cid2atoms[compound.id.id.cid] = atoms
                except:
                    pass

            batch_cid2xyz = {
                cid: XYZ.model_validate(
                    {"atoms": atoms, "comment": "Generated from PubChem"}
                ).to_xyz_str()
                for cid, atoms in cid2atoms.items()
            }

            for cid, xyz in batch_cid2xyz.items():
                with open(
                    str(self.cid_xyz_cache_path.joinpath(f"{cid}.xyz")), "w"
                ) as f:
                    f.write(xyz)
                self.cids_with_xyz.add(cid)

            cid2xyz.update(batch_cid2xyz)

        return [cid2xyz.get(iri2cid.get(iri)) for iri in iris]

    def get_from_ontospecies(self, iris: list[str]):
        query = f"""PREFIX gc: <{GC}>
PREFIX os: <{ONTOSPECIES}>

SELECT DISTINCT *
WHERE {{
    VALUES ?Species {{ {" ".join(f"<{iri}>" for iri in set(iris))} }}
    ?Species gc:hasAtom [
        gc:isElement/os:hasElementSymbol/os:value ?symbol ;
        os:hasXCoordinate/os:value ?x ;
        os:hasYCoordinate/os:value ?y ;
        os:hasZCoordinate/os:value ?z
    ] .
}}"""
        _, bindings = self.ontospecies_client.querySelectThenFlatten(query)

        iri2binding = {binding["Species"]: binding for binding in bindings}
        iri2geom = {
            iri: XYZ.model_validate(
                {"atoms": binding, "comment": "Generated from SPARQL query"}
            )
            for iri, binding in iri2binding.items()
        }
        return [iri2geom.get(iri) for iri in iris]

    def _get_from_url(self, url: str):
        response = requests.get(
            url,
            auth=(self.ontomops_fileserver_username, self.ontomops_fileserver_password),
        )
        try:
            response.raise_for_status()
        except HTTPError:
            return None
        return response.text

    def get_from_ontomops(self, iris: list[str]):
        query = f"""PREFIX os: <{ONTOSPECIES}>
PREFIX mops: <{ONTOMOPS}>

SELECT DISTINCT * WHERE {{
    VALUES ?IRI {{ {" ".join(f"<{iri}>" for iri in iris)} }}
    ?IRI os:hasGeometry/mops:hasGeometryFile ?URL .
}}"""
        _, bindings = self.ontomops_client.querySelectThenFlatten(query)
        iri2url = {binding["IRI"]: binding["URL"] for binding in bindings}
        iri2xyz = {iri: self._get_from_url(url) for iri, url in iri2url.items()}
        return [iri2xyz.get(iri) for iri in iris]


def get_ontomops_fileserverSettings(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.ontomops_fileserver


@cache
def get_xyz_manager(
    ontospecies_endpoint: Annotated[str, Depends(get_ontospecies_endpoint)],
    ontomops_endpoint: Annotated[str, Depends(get_ontomops_endpoint)],
    ontomops_fileserver_settings: Annotated[
        OntomopsFileserverSettings, Depends(get_ontomops_fileserverSettings)
    ],
):
    return XYZManager(
        ontospecies_endpoint=ontospecies_endpoint,
        ontomops_endpoint=ontomops_endpoint,
        ontomops_fileserver_username=ontomops_fileserver_settings.username,
        ontomops_fileserver_password=ontomops_fileserver_settings.password,
    )
