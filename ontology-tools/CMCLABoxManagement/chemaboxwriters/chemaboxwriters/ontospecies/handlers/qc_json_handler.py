import chemutils.obabelutils.obconverter as obconverter
import chemutils.obabelutils.obutils as obutils
import compchemparser.helpers.utils as ccparse_utils
import chemaboxwriters.common.utilsfunc as utilsfunc
from compchemparser.parsers.ccgaussian_parser import (
    ATOM_MASSES,
    FORMAL_CHARGE,
    ATOM_TYPES,
    EMP_FORMULA,
    GEOM,
    SPIN_MULT,
)
from compchemparser.helpers.elements_data import get_molwt_from_atom_types
import pubchempy as pcp
from collections import Counter
import json
import re
import time
import chemaboxwriters.common.params as params
from chemaboxwriters.common.handler import Handler
from chemaboxwriters.ontospecies.abox_stages import OS_ABOX_STAGES
from typing import List, Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)

cas_re = re.compile(r"(\d{2,7}-\d\d-\d)")

MOLWT = "MolecularWeight"
INCHI = "InChi"
SMILES = "Smiles"
GEOM_STRING = "GeometryString"
BOND_STRING = "BondString"
PUBCHEM_ALT_LABEL = "PubchemAlternativeLabel"
CAS_NUMBER = "CAS"
PUBCHEM_CID = "PubchemCID"
ATOM_LIST = "AtomsList"
ATOM_COUNTS = "AtomsCounts"
ENTH_FORM = "StandardEnthalpyOfFormation"
ENTH_UNIT = "StandardEnthalpyOfFormationUnit"
ENTH_PHASE = "StandardEnthalpyOfFormationPhase"
ENTH_REFTEMP = "ReferenceTemperature"
ENTH_REFTEMP_UNIT = "ReferenceTemperatureUnit"
ENTH_PROV = "StandardEnthalpyofFormationProvenance"


HANDLER_PREFIXES = {
    "spec_pref": {"required": True},
}

HANDLER_PARAMETERS = {
    "random_id": {"required": False},
    "enth_of_form": {"required": False},
    "enth_of_form_unit": {"required": False},
    "enth_of_form_phase": {"required": False},
    "enth_of_form_ref_temp": {"required": False},
    "enth_of_form_ref_temp_unit": {"required": False},
    "enth_of_form_provenance": {"required": False},
}

PUBCHEM_QUERY_ATTEMPTS = 3


class QC_JSON_TO_OS_JSON_Handler(Handler):
    """Handler converting qc_json files to os_json.
    Inputs: List of qc_json file paths
    Outputs: List of os_json file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="QC_JSON_TO_OS_JSON",
            in_stage=OS_ABOX_STAGES.qc_json,  # type: ignore
            out_stage=OS_ABOX_STAGES.os_json,  # type: ignore
            prefixes=HANDLER_PREFIXES,
            handler_params=HANDLER_PARAMETERS,
        )

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: str,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage,
                out_dir=out_dir,
            )
            self._os_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
            )
            outputs.append(out_file_path)
        return outputs

    def _os_jsonwriter(self, file_path: str, output_file_path: str) -> None:

        spec_pref = self.get_prefix_value(name="spec_pref")

        random_id = self.get_parameter_value(name="random_id")
        enth_of_form = self.get_parameter_value(name="enth_of_form")
        enth_of_form_unit = self.get_parameter_value(name="enth_of_form_unit")
        enth_of_form_phase = self.get_parameter_value(name="enth_of_form_phase")
        enth_of_form_ref_temp = self.get_parameter_value(name="enth_of_form_ref_temp")
        enth_of_form_ref_temp_unit = self.get_parameter_value(
            name="enth_of_form_ref_temp_unit"
        )
        enth_of_form_provenance = self.get_parameter_value(
            name="enth_of_form_provenance"
        )

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        data_out = {}
        xyz = ccparse_utils.get_xyz_from_parsed_json(data)
        inchi = obconverter.obConvert(xyz, "xyz", "inchi")
        smiles = obconverter.obConvert(xyz, "xyz", "smi")
        data_out[INCHI] = inchi
        data_out[SMILES] = smiles
        data_out[EMP_FORMULA] = data[EMP_FORMULA]
        data_out[ATOM_TYPES] = data[ATOM_TYPES]
        data_out[GEOM] = data[GEOM]
        data_out[SPIN_MULT] = data[SPIN_MULT]

        if enth_of_form is not None:
            data_out[ENTH_FORM] = enth_of_form
        if enth_of_form_unit is not None:
            data_out[ENTH_UNIT] = enth_of_form_unit
        if enth_of_form_phase is not None:
            data_out[ENTH_PHASE] = enth_of_form_phase
        if enth_of_form_ref_temp is not None:
            data_out[ENTH_REFTEMP] = enth_of_form_ref_temp
        if enth_of_form_ref_temp_unit is not None:
            data_out[ENTH_REFTEMP_UNIT] = enth_of_form_ref_temp_unit
        if enth_of_form_provenance is not None:
            data_out[ENTH_PROV] = enth_of_form_provenance

        if ATOM_MASSES not in data.keys():
            data_out[MOLWT] = get_molwt_from_atom_types(data_out[ATOM_TYPES])
        else:
            data_out[MOLWT] = sum(data[ATOM_MASSES])

        if FORMAL_CHARGE not in data.keys():
            # use openbable to find the charge?
            data_out[FORMAL_CHARGE] = 0
        else:
            data_out[FORMAL_CHARGE] = data[FORMAL_CHARGE]

        data_out[GEOM_STRING] = self._geom_info(data)
        bonds_info = obutils.obGetMolBonds(xyz)
        bonds_info_line = [
            f"{bond['beginAtom']['atomId']} {bond['endAtom']['atomId']} {bond['order']}"
            for bond in bonds_info
        ]
        data_out[BOND_STRING] = " ".join(bonds_info_line)

        pubchem_data = _get_pubchem_data(
            inchi=data_out[INCHI], max_attempts=PUBCHEM_QUERY_ATTEMPTS
        )

        data_out[PUBCHEM_ALT_LABEL] = pubchem_data.get("alt_labels")
        data_out[CAS_NUMBER] = pubchem_data.get("casid")
        data_out[PUBCHEM_CID] = pubchem_data.get("cid")

        atom_list, atom_counts = self._atom_constructor(data_out[ATOM_TYPES])
        data_out[ATOM_LIST] = atom_list
        data_out[ATOM_COUNTS] = atom_counts

        if not random_id:
            random_id = utilsfunc.get_random_id()

        data_out[params.ENTRY_UUID] = random_id
        data_out[params.ENTRY_IRI] = f"{spec_pref}Species_{random_id}"

        utilsfunc.write_dict_to_file(dict_data=data_out, dest_path=output_file_path)

    @staticmethod
    def _geom_info(data):
        atoms = data["Atom types"]
        coords = data["Geometry"]
        geom_out = []
        for k in range(len(atoms)):
            atom = atoms[k]
            x = coords[k][0]
            y = coords[k][1]
            z = coords[k][2]
            geom_out.append(f"{atom} {x} {y} {z}")
        geom_string = " ".join(geom_out)
        return geom_string

    @staticmethod
    def _atom_constructor(atom_list):
        pruned_atoms = list(dict.fromkeys(atom_list))
        c = Counter(atom_list)
        atom_counts = [c[x] for x in pruned_atoms]
        return pruned_atoms, atom_counts


def _get_pubchem_data(inchi: str, max_attempts: int = 3) -> Dict:
    attempts_loc = 0
    pubchem_data = {}
    while attempts_loc < max_attempts:
        try:
            pubchem_data = _query_pubchem(inchi=inchi)
            break
        except pcp.BadRequestError:
            logger.warning("PubChem Bad request error.")
            attempts_loc = max_attempts
        except Exception:
            logger.warning("Issue with the PubChem request. Waiting 10s and retrying.")
            time.sleep(10)
            attempts_loc += 1

    return pubchem_data


def _get_substructure_cas(synonyms) -> List[str]:
    cas_rns: List[str] = []
    for syn in synonyms:
        match = re.match(cas_re, syn)
        if match:
            cas_rns.append(match.group(1))
    return cas_rns


def _query_pubchem(inchi: str) -> Dict:
    pubchem_data: Dict[str, Optional[Any]] = {
        "cid": None,
        "casid": None,
        "alt_labels": None,
    }

    pubchem_compound = pcp.get_compounds(inchi, "inchi")
    # We will wait for 1 second to try and avoid hitting PubChem's limit.

    time.sleep(1)
    if pubchem_compound:
        pubchem_data["cid"] = pubchem_compound[0].cid
        if pubchem_compound[0].synonyms:
            pubchem_data["alt_labels"] = pubchem_compound[0].synonyms[0]
            casid = _get_substructure_cas(pubchem_compound[0].synonyms)
            if casid:
                pubchem_data["casid"] = casid[0]
    return pubchem_data
