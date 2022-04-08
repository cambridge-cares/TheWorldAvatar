import chemaboxwriters.common.params as params
from chemaboxwriters.common.abox_stages import ABOX_STAGES_COMMON
import chemutils.rdkitutils.rdkitmolutils as rdkitutils
import chemutils.rdkitutils.rdkitconverters as rdkitconverters
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import (
    convert_csv_string_into_rdf,
)
import os
import glob
from typing import List, Optional, Dict, Literal, Tuple
import uuid
import logging
import json
import re
import pathlib
import csv
import numpy as np

FORMULA_CLEAN_RE = re.compile("(?<=[a-zA-Z])(1)(?=[a-zA-Z]+?|$)")


class Abox_csv_writer:
    def __init__(self, file_path: str, configs: Optional[Dict] = None) -> None:
        self.file_path = file_path
        self._num_cols = 6
        if configs is None:
            configs = {}
        self.instance_field = configs.get("instance", "Instance")
        self.data_property_field = configs.get("data_property", "Data Property")
        self.object_property_field = configs.get("object_property", "Instance")
        self.ontology_field = configs.get("ontology", "Ontology")
        self.default_prefix = configs.get("prefix", "")
        self._current_inst = None
        self._prefixes = {}
        self._current_name = None
        self._write_history = []

    def register_prefix(self, name: str, value: str) -> None:
        self._prefixes[name] = value

    def write_header(self) -> "Abox_csv_writer":
        self._write_row(
            "Source",
            "Type",
            "Target",
            "Relation",
            "Value",
            "Data Type",
        )
        return self

    def write_imports(
        self,
        name: str,
        importing: str,
        rel: Optional[str] = None,
        store_name: bool = True,
    ) -> "Abox_csv_writer":
        if rel is None:
            rel = "http://www.w3.org/2002/07/owl#imports"

        name, rel, importing = self._apply_prefixes(name, rel, importing)
        self._write_row(name, self.ontology_field, importing, rel)

        if store_name:
            self._current_name = name
        return self

    def write_inst(
        self, iri: str, type: str, rel: str = "", store_inst: bool = True
    ) -> "Abox_csv_writer":

        iri, rel, type = self._apply_prefixes(iri, rel, type)

        self._write_row(iri, self.instance_field, type, rel)
        if store_inst:
            self._current_inst = iri
        return self

    def write_data_prop(
        self,
        iri: str,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float"] = "String",
        store_inst: bool = True,
    ) -> "Abox_csv_writer":

        iri, rel, value = self._apply_prefixes(iri, rel, value)
        self._write_row(rel, self.data_property_field, iri, "", value, str(data_type))

        if store_inst:
            self._current_inst = iri
        return self

    def write_obj_prop(
        self,
        src_iri: str,
        rel: str,
        trg_iri: str,
        store_inst: Literal["src", "trg", ""] = "",
    ) -> "Abox_csv_writer":

        src_iri, rel, trg_iri = self._apply_prefixes(src_iri, rel, trg_iri)

        self._write_row(src_iri, self.object_property_field, trg_iri, rel)
        if store_inst == "src":
            self._current_inst = src_iri
        elif store_inst == "trg":
            self._current_inst = trg_iri
        return self

    def add_obj_prop(
        self, rel: str, iri: str, store_inst: bool = False, reverse: bool = False
    ) -> "Abox_csv_writer":
        if self._current_inst is None:
            raise app_exceptions.MissingInstance(
                "No instance created. Cannont add object property."
            )

        src_iri, trg_iri = iri, self._current_inst
        if reverse:
            src_iri, trg_iri = self._current_inst, iri

        store_inst_lit = "src" if store_inst else ""

        self.write_obj_prop(
            src_iri=src_iri, rel=rel, trg_iri=trg_iri, store_inst=store_inst_lit
        )
        return self

    def add_data_prop(
        self,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float"] = "String",
    ) -> "Abox_csv_writer":

        if self._current_inst is None:
            raise app_exceptions.MissingInstance(
                "No instance created. Can not add data property."
            )
        self.write_data_prop(
            iri=self._current_inst, rel=rel, value=value, data_type=data_type
        )
        return self

    def add_imports(
        self, importing: str, rel: Optional[str] = None
    ) -> "Abox_csv_writer":

        if self._current_name is None:
            raise app_exceptions.MissingOntologyName(
                "No ontology name defined. Can not add import statement."
            )

        self.write_imports(name=self._current_name, importing=importing, rel=rel)
        return self

    def _write_row(self, *args: str) -> None:
        content = [args[i] if i < len(args) else "" for i in range(self._num_cols)]
        # if content not in self._write_history:
        self.csvwriter.writerow(content)
        #    self._write_history.append(content)

    def _apply_prefixes(self, *args: str) -> List[str]:
        items = []
        for item in args:
            prefix_name = item.split(":")[0]
            prefix_value = self._prefixes.get(prefix_name)
            if prefix_value is not None:
                item = item.replace(f"{prefix_name}:", prefix_value)
            items.append(item)
        return items

    def __enter__(self):
        self._file_obj = open(self.file_path, "w", newline="").__enter__()
        self.csvwriter = csv.writer(
            self._file_obj, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback) -> bool:
        self._file_obj.__exit__(exc_type, exc_value, exc_traceback)
        return True


def config_logging(
    log_file_dir: Optional[str] = None,
    log_file_name: Optional[str] = None,
    no_file_logging: bool = False,
) -> None:

    if log_file_name is None:
        log_file_name = "aboxwriter_pipeline.log"

    if log_file_dir is None:
        log_file_dir = os.getcwd()
    log_file = os.path.join(log_file_dir, log_file_name)

    logHandlers = []
    logHandlers.append(logging.StreamHandler())
    if not no_file_logging:
        logHandlers.append(logging.FileHandler(filename=log_file, mode="w"))

    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s [%(threadName)s] [%(levelname)s] %(message)s",
        handlers=logHandlers,
    )


def get_stage_files(
    file_or_dir: str, in_stage: str, file_ext: Optional[str] = None
) -> List[str]:

    file_ext_str: str = ""
    if file_ext is None:
        if in_stage == ABOX_STAGES_COMMON.qc_log:
            file_ext_str = params.CC_LOG_EXT
        else:
            file_ext_str = in_stage.lower()
    else:
        file_ext_str = file_ext

    files = get_files_by_extensions(file_or_dir=file_or_dir, file_ext_str=file_ext_str)
    return files


def get_files_by_extensions(file_or_dir: str, file_ext_str: str) -> List[str]:
    files = []
    file_ext = file_ext_str.split(",")
    if os.path.isfile(file_or_dir):
        files = [file_or_dir]
    elif os.path.isdir(file_or_dir):
        for ext in file_ext:
            files += glob.glob(os.path.join(file_or_dir, f"*.{ext}"))
    else:
        raise app_exceptions.IncorrectFileOrDirPath(
            (f"Error: File or dir {file_or_dir} does not exist.")
        )
    return files


def readFile(file_path: str) -> str:
    with open(file_path, "r") as file_handle:
        file_content = file_handle.read()
    return file_content


def fileExists(path: str) -> bool:
    return os.path.isfile(os.path.abspath(path))


def csv2rdf_wrapper(file_path: str) -> List[str]:
    csv_string = readFile(file_path)
    return [convert_csv_string_into_rdf(csv_string)]


def getRefName(filepath: str, jobIndex: int, numJobs: int, extension: str) -> str:

    if numJobs > 1:
        refName = filepath + "_" + str(jobIndex + 1) + extension
    else:
        refName = filepath + extension
    return refName


def get_random_id():
    return str(
        uuid.uuid4()
    )  # Get a randomly generated identifier for creation of the ABox.


def write_dict_to_file(
    dict_data: Dict, dest_path: str, indent: int = 4, *args, **kwargs
) -> None:
    with open(dest_path, "w") as jsonFile:
        json.dump(dict_data, jsonFile, indent=4, *args, **kwargs)


def get_out_file_path(
    input_file_path: str,
    file_extension: str,
    out_dir: Optional[str] = None,
    replace_last_ext: bool = True,
) -> str:

    p = pathlib.Path(input_file_path)
    if replace_last_ext:
        filename = (
            re.sub(r".([^.]*)$", f".{file_extension}", p.name)
            if "." in p.name
            else f"{p.name}.{file_extension}"
        )
    else:
        filename = f"{p.name}.{file_extension}"

    if out_dir is None:
        return str((p.parents[0]).joinpath(filename))
    else:
        return str((pathlib.Path(out_dir)).joinpath(filename))


def generate_molecule_png(inchi: str, out_path: str, **kwargs) -> None:
    rdkit_mol = rdkitconverters.inchiToRdkitMol(inchi=inchi)
    rdkitutils.rdkitMolToFile(rdkitMol=rdkit_mol, out_path=out_path, **kwargs)


def split_qc_json_geom_to_xyz_coords(geom_list: List) -> Tuple:
    geom_arr = np.array(geom_list)
    x_coord = geom_arr[:, 0].tolist()
    y_coord = geom_arr[:, 1].tolist()
    z_coord = geom_arr[:, 2].tolist()

    return x_coord, y_coord, z_coord


def get_atom_indices_from_qc_json(atom_list: List):
    atom_counters = {}
    atom_ind = []
    for atom_type in atom_list:
        if atom_type not in atom_counters:
            atom_counters[atom_type] = 1
        atom_ind.append(atom_counters[atom_type])
        atom_counters[atom_type] += 1
    return atom_ind


def clean_qc_json_emp_formula(emp_formula: str):
    return FORMULA_CLEAN_RE.sub("", emp_formula)
