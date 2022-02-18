from os import stat
import chemutils.obabelutils.obconverter as obconverter
import chemutils.obabelutils.obutils as obutils
import compchemparser.helpers.utils as ccparse_utils
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common import PREFIXES
from compchemparser.parsers.ccgaussian_parser import (
    ATOM_MASSES,
    FORMAL_CHARGE,
    ATOM_TYPES,
    ATOM_MASSES,
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
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common.handler import IHandler
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional


cas_re = re.compile("\d{2,7}-\d\d-\d")

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

spec_pref = PREFIXES["spec_pref"]


@dataclass
class QC_JSON_TO_OS_JSON_Handler(IHandler):
    """Handler converting qc json files to os json.
    Inputs: List of qc json file paths
    Outputs: List of os json file paths
    """

    name: str = field(default="QC_JSON_TO_OS_JSON")
    in_stages: List[Enum] = field(default_factory=lambda: [globals.aboxStages.QC_JSON])
    out_stage: Enum = field(default=globals.aboxStages.OS_JSON)

    def handle_input(
        self, inputs: List[str], out_dir: str, **handler_kwargs
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self.out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._os_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **handler_kwargs
            )
            outputs.append(out_file_path)
        return outputs

    def _os_jsonwriter(
        self,
        file_path: str,
        output_file_path: str,
        random_id: str = "",
        spec_pref: str = PREFIXES["spec_pref"],
        hf: Optional[str] = None,
        hf_unit: Optional[str] = None,
        hf_phase: Optional[str] = None,
        hfTref: Optional[str] = None,
        hfTref_unit: Optional[str] = None,
        hf_prov: Optional[str] = None,
        *args,
        **kwargs
    ) -> None:

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

        if hf:
            data_out[ENTH_FORM] = hf
        if hf_unit:
            data_out[ENTH_UNIT] = hf_unit
        if hf_phase:
            data_out[ENTH_PHASE] = hf_phase
        if hfTref:
            data_out[ENTH_REFTEMP] = hfTref
        if hfTref_unit:
            data_out[ENTH_REFTEMP_UNIT] = hfTref_unit
        if hf_prov:
            data_out[ENTH_PROV] = hf_prov

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
            str(bond["beginAtom"]["atomId"])
            + " "
            + str(bond["endAtom"]["atomId"])
            + " "
            + str(bond["order"])
            for bond in bonds_info
        ]
        data_out[BOND_STRING] = " ".join(bonds_info_line)
        # add atoms positions!

        alt_labels = None
        casid = None
        cid = None

        try:
            pubchem_compound = pcp.get_compounds(data_out[INCHI], "inchi")
            time.sleep(
                5
            )  # We will wait for 5 seconds to try and avoid hitting PubChem's limit.
            if pubchem_compound:
                cid = pubchem_compound[0].cid
                if pubchem_compound[0].synonyms:
                    alt_labels = pubchem_compound[0].synonyms[0]
                    casid = self._get_substructure_cas(pubchem_compound[0].synonyms)
                    if casid:
                        casid = casid[0]
        except pcp.BadRequestError:
            print("Issue with PubChem request")

        data_out[PUBCHEM_ALT_LABEL] = alt_labels
        data_out[CAS_NUMBER] = casid
        data_out[PUBCHEM_CID] = cid

        atom_list, atom_counts = self._atom_constructor(data_out[ATOM_TYPES])
        data_out[ATOM_LIST] = atom_list
        data_out[ATOM_COUNTS] = atom_counts

        if not random_id:
            random_id = utilsfunc.get_random_id()

        data_out[globals.ENTRY_UUID] = random_id
        data_out[globals.ENTRY_IRI] = spec_pref + "Species_" + random_id

        utilsfunc.write_dict_to_file(dict_data=data_out, dest_path=output_file_path)

    @staticmethod
    def _get_substructure_cas(synonyms):
        cas_rns = []
        for syn in synonyms:
            match = re.match("(\d{2,7}-\d\d-\d)", syn)
            if match:
                cas_rns.append(match.group(1))
        return cas_rns

    @staticmethod
    def _geom_info(data):
        atoms = data["Atom types"]
        coords = data["Geometry"]
        geom_out = []
        for k in range(len(atoms)):
            geom_out.append(
                atoms[k]
                + " "
                + str(coords[k][0])
                + " "
                + str(coords[k][1])
                + " "
                + str(coords[k][2])
            )
        geom_string = " ".join(geom_out)
        return geom_string

    @staticmethod
    def _atom_constructor(atom_list):
        pruned_atoms = list(dict.fromkeys(atom_list))
        c = Counter(atom_list)
        atom_counts = [c[x] for x in pruned_atoms]
        return pruned_atoms, atom_counts
