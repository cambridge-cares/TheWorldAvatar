import chemaboxwriters.common.globals as globals
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import (
    convert_csv_string_into_rdf,
)
import os
import glob
from enum import Enum
from typing import List, Optional, Dict, Literal
import uuid
import logging
import json
import re
import pathlib
import csv

formula_clean_re = re.compile("(?<=[a-zA-Z])(1)(?=[a-zA-Z]+?|$)")


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

    def __enter__(self):
        self._file_obj = open(self.file_path, "w", newline="").__enter__()
        self.csvwriter = csv.writer(
            self._file_obj, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback) -> bool:
        self._file_obj.__exit__(exc_type, exc_value, exc_traceback)
        return True

    def write_header(self) -> None:
        self._write_row(
            "Source",
            "Type",
            "Target",
            "Relation",
            "Value",
            "Data Type",
        )

    def _write_row(self, *args: str) -> None:
        content = [args[i] if i < len(args) else "" for i in range(self._num_cols)]
        self.csvwriter.writerow(content)

    def write_imports(
        self,
        name: str,
        importing: str,
        rel: Optional[str] = None,
    ) -> None:
        if rel is None:
            rel = "http://www.w3.org/2002/07/owl#imports"
        self._write_row(name, self.ontology_field, importing, rel)

    def write_inst(self, iri: str, type: str, relation: str = "") -> None:
        self._write_row(iri, self.instance_field, type, relation)

    def write_data_prop(
        self,
        iri: str,
        rel: str,
        value: str,
        data_type: Literal["String", "Integer", "Float", ""] = "",
    ) -> None:
        self._write_row(rel, self.data_property_field, iri, "", value, str(data_type))

    def write_obj_prop(self, src_iri: str, rel: str, trg_iri: str) -> None:
        self._write_row(src_iri, self.object_property_field, trg_iri, rel)


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
    file_or_dir: str, in_stage: Enum, file_ext: Optional[str] = None
) -> List[str]:

    file_ext_str: str = ""
    if file_ext is None:
        if in_stage == globals.aboxStages.QC_LOG:
            file_ext_str = globals.CC_LOG_EXT
        else:
            file_ext_str = in_stage.name.lower()
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
        raise app_exceptions.IncorrectFileOrDirPath
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


def stage_name_to_enum(stage_name: str) -> Enum:
    try:
        inStage = globals.aboxStages[stage_name.upper()]
    except KeyError:
        raise app_exceptions.UnsupportedStage
    return inStage


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
