import chemutils.obabelutils.obconverter as obconverter
import chemutils.obabelutils.obutils as obutils
import compchemparser.helpers.utils as ccparse_utils
import chemaboxwriters.common.utilsfunc as utilsfunc
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
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
from enum import Enum
from typing import List, Optional, Dict


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


class QC_JSON_TO_OS_JSON_Handler(Handler):
    """Handler converting qc_json files to os_json.
    Inputs: List of qc_json file paths
    Outputs: List of os_json file paths
    """

    def __init__(
        self,
        endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
    ) -> None:
        super().__init__(
            name="QC_JSON_TO_OS_JSON",
            in_stage=globals.aboxStages.QC_JSON,
            out_stage=globals.aboxStages.OS_JSON,
            endpoints_proxy=endpoints_proxy,
            required_endpoints_config={abconf.WRITERS_PREFIXES_KEY: ["spec_pref"]},
            supported_handler_kwargs=[
                "random_id",
                "enth_of_form",
                "enth_of_form_unit",
                "enth_of_form_phase",
                "enth_of_form_ref_temp",
                "enth_of_form_ref_temp_unit",
                "enth_of_form_provenance",
            ],
        )

    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._os_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **self._handler_kwargs
            )
            outputs.append(out_file_path)
        return outputs

    def _os_jsonwriter(
        self,
        file_path: str,
        output_file_path: str,
        random_id: str = "",
        enth_of_form: Optional[str] = None,
        enth_of_form_unit: Optional[str] = None,
        enth_of_form_phase: Optional[str] = None,
        enth_of_form_ref_temp: Optional[str] = None,
        enth_of_form_ref_temp_unit: Optional[str] = None,
        enth_of_form_provenance: Optional[str] = None,
        *args,
        **kwargs
    ) -> None:

        spec_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["spec_pref"]

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

        if enth_of_form:
            data_out[ENTH_FORM] = enth_of_form
        if enth_of_form_unit:
            data_out[ENTH_UNIT] = enth_of_form_unit
        if enth_of_form_phase:
            data_out[ENTH_PHASE] = enth_of_form_phase
        if enth_of_form_ref_temp:
            data_out[ENTH_REFTEMP] = enth_of_form_ref_temp
        if enth_of_form_ref_temp_unit:
            data_out[ENTH_REFTEMP_UNIT] = enth_of_form_ref_temp_unit
        if enth_of_form_provenance:
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
