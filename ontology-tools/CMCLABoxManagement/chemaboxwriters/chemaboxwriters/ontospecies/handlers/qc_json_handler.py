import chemaboxwriters.ontospecies.handlers.os_json_keys as os_keys
import chemaboxwriters.common.utilsfunc as utilsfunc
import pubchempy as pcp
import json
import re
import time
from chemaboxwriters.common.handler import Handler
from chemaboxwriters.ontospecies.abox_stages import OS_ABOX_STAGES
from chemaboxwriters.ontospecies import OS_SCHEMA
from typing import List, Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)

cas_re = re.compile(r"(\d{2,7}-\d\d-\d)")


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
            handler_params=HANDLER_PARAMETERS,
        )

    def handle_input(
        self, inputs: List[str], out_dir: str, *args, **kwargs
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

        random_id = self.get_parameter_value(
            name="random_id", default=utilsfunc.get_random_id()
        )
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
            qc_data_in = json.load(file_handle)

        # create an empty os json out dict
        data_out = dict.fromkeys(os_keys.OS_JSON_KEYS, None)

        # populate the selected os json entries with the qc json data
        # apply any defined post processing steps
        # --------------------------------------------------
        for os_key, qc_os_map in os_keys.OS_JSON_TO_QC_JSON_KEYS_MAP.items():
            qc_values = [qc_data_in.get(key) for key in qc_os_map["cckeys"]]
            data_out[os_key] = qc_os_map["postproc_func"](qc_values)

        # add any enthalpy data
        data_out[os_keys.STANDARD_ENTH_FORM] = enth_of_form
        data_out[os_keys.STANDARD_ENTH_FORM_UNIT] = enth_of_form_unit
        data_out[os_keys.STANDARD_ENTH_FORM_PHASE] = enth_of_form_phase
        data_out[os_keys.STANDARD_ENTH_FORM_REF_TEMP] = enth_of_form_ref_temp
        data_out[os_keys.STANDARD_ENTH_FORM_REF_TEMP_UNIT] = enth_of_form_ref_temp_unit
        data_out[os_keys.STANDARD_ENTH_FORM_PROVENANCE] = enth_of_form_provenance

        # add any pubchem data
        if data_out[os_keys.INCHI] is not None:
            pubchem_data = _get_pubchem_data(
                inchi=data_out[os_keys.INCHI], max_attempts=PUBCHEM_QUERY_ATTEMPTS
            )
            data_out[os_keys.PUBCHEM_ALT_LABEL] = pubchem_data.get("alt_labels")
            data_out[os_keys.CAS_NUMBER] = pubchem_data.get("casid")
            data_out[os_keys.PUBCHEM_CID] = pubchem_data.get("cid")

        # write entry id and iri
        data_out[os_keys.ENTRY_ID] = random_id
        main_inst_pref = utilsfunc.read_main_pref_from_schema(
            schema_file=OS_SCHEMA, main_pref_name="main_inst_pref"
        )
        data_out[os_keys.ENTRY_IRI] = f"{main_inst_pref}{random_id}"

        utilsfunc.write_dict_to_file(dict_data=data_out, dest_path=output_file_path)


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
