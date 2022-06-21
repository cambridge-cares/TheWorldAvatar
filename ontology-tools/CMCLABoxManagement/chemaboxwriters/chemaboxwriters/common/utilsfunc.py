from chemaboxwriters.common.abox_stages import ABOX_STAGES_COMMON
import chemutils.rdkitutils.rdkitmolutils as rdkitutils
import chemutils.rdkitutils.rdkitconverters as rdkitconverters
import chemutils.obabelutils.obutils as obutils
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
import os
import glob
from typing import List, Optional, Dict, Tuple
import uuid
import logging
import json
import re
import pathlib
import yaml
import numpy as np

__doc__ = """
This module contains a collection of functions that are used throughout the code.
"""

FORMULA_CLEAN_RE = re.compile("(?<=[a-zA-Z])(1)(?=[a-zA-Z]+?|$)")
# default cc log extensions
CC_LOG_EXT = "qc_log,log,out,g03,g09,g16"


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
            file_ext_str = CC_LOG_EXT
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


def read_main_pref_from_schema(schema_file: str, main_pref_name: str) -> str:
    with open(schema_file, "r") as stream:
        schema_dict = yaml.safe_load(stream)

    prefixes = schema_dict.get("prefixes", {})

    return prefixes.get(main_pref_name, "")


def construct_xyz_file_string_from_atoms_and_coords(
    atoms: List[str], coords: List[List[str]]
) -> str:
    num_ats = len(atoms)
    xyz_coords = f"{num_ats}\n\n"
    for atom, xyz in zip(atoms, coords):
        xyz_coords = f"{xyz_coords}{atom} {xyz[0]} {xyz[1]} {xyz[2]}\n"
    xyz_coords = xyz_coords.rstrip()
    return xyz_coords


def construct_xyz_string_from_atoms_and_coords(
    atoms: List[str], coords: List[List[str]]
) -> str:
    xyz_coords = ""
    for atom, xyz in zip(atoms, coords):
        xyz_coords = f"{xyz_coords}{atom} {xyz[0]} {xyz[1]} {xyz[2]}\n"
    xyz_coords = xyz_coords.rstrip()
    return xyz_coords


def construct_bond_string(atoms: List[str], coords: List[List[str]]) -> str:
    xyz_string = construct_xyz_file_string_from_atoms_and_coords(atoms, coords)
    bonds_info = obutils.obGetMolBonds(xyz_string)
    bonds_info_line = [
        f"{bond['beginAtom']['atomId']} {bond['endAtom']['atomId']} {bond['order']}"
        for bond in bonds_info
    ]
    return " ".join(bonds_info_line)
