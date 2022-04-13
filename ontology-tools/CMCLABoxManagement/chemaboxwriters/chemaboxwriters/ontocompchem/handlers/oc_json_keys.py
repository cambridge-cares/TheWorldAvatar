import compchemparser.parsers.ccgaussian_parser as ccparser
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.utilsfunc as utilsfunc
import numpy as np

__doc__ = """
This module defines all the oc json file keys and the mapping 
on how to populate oc json entries with the qc json entries.
"""

# OC_JSON_KEYS
# -------------------------
# atoms, molecules etc
EMPIRICAL_FORMULA = "Empirical_Formula"
ATOM_TOTAL_COUNTS = "Atom_Total_Counts"
ATOM_LIST = "Atom_List"
ATOM_MASSES = "Atom_Masses"
ELEMENTS = "Elements"
# method, lvl of theory, spin, charge
LEVEL_OF_THEORY = "Level_of_Theory"
BASIS_SET = "Basis_Set"
SPIN_MULTIPLICITY = "Spin_Multiplicity"
TOTAL_CHARGE = "Total_Charge"
# geometry
GEOMETRY_TYPE = "Geometry_Type"
X_ATOMS_COORDINATES = "X_Atoms_Coordinates"
Y_ATOMS_COORDINATES = "Y_Atoms_Coordinates"
Z_ATOMS_COORDINATES = "Z_Atoms_Coordinates"
# rot consts and freq
ROTATIONAL_CONSTANTS_STRING = "Rotational_Constants_String"
ROTATIONAL_CONSTANTS_NUMBER = "Rotational_Constants_Number"
ROTATIONAL_SYMMETRY_NUMBER = "Rotational_Symmetry_Number"
FREQUENCIES_STRING = "Frequencies_String"
FREQUENCIES_NUMBER = "Frequencies_Number"
# energies
ELECTRONIC_ENERGY = "Electronic_Energy"
ELECTRONIC_AND_ZPE_ENERGY = "Electronic_And_ZPE_Energy"
ZPE_ENERGY = "ZPE_Energy"
HOMO_ENERGY = "Homo_Energy"
HOMO_MIN_1_ENERGY = "Homo-1_Energy"
HOMO_MIN_2_ENERGY = "Homo-2_Energy"
LUMO_ENERGY = "Lumo_Energy"
LUMO_PLUS_1_ENERGY = "Lumo+1_Energy"
LUMO_PLUS_2_ENERGY = "Lumo+2_Energy"
# metadata
PROGRAM_NAME = "Program_Name"
PROGRAM_VERSION = "Program_Version"
RUN_DATE = "Run_Date"
LOG_FILE_NAME = "Log_File_Name"
# other
SPECIES_IRI = "Species_IRI"
ENTRY_IRI = "Entry_IRI"
PNG_SOURCE_LOCATION = "PNG_Source_Location"
CCLOG_SOURCE_LOCATION = "CCLOG_Source_Location"
ENTRY_ID = "Entry_ID"


OC_JSON_KEYS = [
    EMPIRICAL_FORMULA,
    ATOM_TOTAL_COUNTS,
    ATOM_LIST,
    ATOM_MASSES,
    ELEMENTS,
    LEVEL_OF_THEORY,
    BASIS_SET,
    SPIN_MULTIPLICITY,
    TOTAL_CHARGE,
    GEOMETRY_TYPE,
    X_ATOMS_COORDINATES,
    Y_ATOMS_COORDINATES,
    Z_ATOMS_COORDINATES,
    ROTATIONAL_CONSTANTS_STRING,
    ROTATIONAL_CONSTANTS_NUMBER,
    ROTATIONAL_SYMMETRY_NUMBER,
    FREQUENCIES_STRING,
    FREQUENCIES_NUMBER,
    ELECTRONIC_ENERGY,
    ELECTRONIC_AND_ZPE_ENERGY,
    ZPE_ENERGY,
    HOMO_ENERGY,
    HOMO_MIN_1_ENERGY,
    HOMO_MIN_2_ENERGY,
    LUMO_ENERGY,
    LUMO_PLUS_1_ENERGY,
    LUMO_PLUS_2_ENERGY,
    PROGRAM_NAME,
    PROGRAM_VERSION,
    RUN_DATE,
    LOG_FILE_NAME,
    SPECIES_IRI,
    ENTRY_IRI,
    PNG_SOURCE_LOCATION,
    CCLOG_SOURCE_LOCATION,
]


# this defines on how to assign the oc json data using the qc json data
# note that the following entries are skipped as they are not required
# for the oc abox:
#
# ccparser.ATOM_MASSES_UNIT
# ccparser.GEOM_UNIT
# ccparser.FORMAL_CHARGE_UNIT
# ccparser.ROT_CONST_UNIT
# ccparser.FREQ_UNIT
#
# In the future it would be usefule to introduce a map object that would
# do some intermediate results caching as atm there is some repetition
OC_JSON_TO_QC_JSON_KEYS_MAP = {
    EMPIRICAL_FORMULA: {
        "cckeys": [ccparser.EMP_FORMULA],
        "postproc_func": lambda x: utilsfunc.clean_qc_json_emp_formula(x[0])
        if x[0] is not None
        else None,
    },
    ATOM_LIST: {
        "cckeys": [ccparser.ATOM_TYPES],
        "postproc_func": lambda x: x[0],
    },
    ATOM_TOTAL_COUNTS: {
        "cckeys": [ccparser.ATOM_COUNTS],
        "postproc_func": lambda x: list(x[0].values()) if x[0] is not None else None,
    },
    ATOM_MASSES: {
        "cckeys": [ccparser.ATOM_MASSES],
        "postproc_func": lambda x: x[0],
    },
    ELEMENTS: {
        "cckeys": [ccparser.ATOM_COUNTS],
        "postproc_func": lambda x: list(x[0].keys()) if x[0] is not None else x[0],
    },
    # method, lvl of theory, spin, charge
    LEVEL_OF_THEORY: {
        "cckeys": [ccparser.METHOD],
        "postproc_func": lambda x: x[0],
    },
    BASIS_SET: {"cckeys": [ccparser.BASIS_SET], "postproc_func": lambda x: x[0]},
    SPIN_MULTIPLICITY: {
        "cckeys": [ccparser.SPIN_MULT],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else x[0],
    },
    TOTAL_CHARGE: {
        "cckeys": [ccparser.FORMAL_CHARGE],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else x[0],
    },
    # geometry
    GEOMETRY_TYPE: {
        "cckeys": [ccparser.GEOM_TYPE],
        "postproc_func": lambda x: x[0],
    },
    X_ATOMS_COORDINATES: {
        "cckeys": [ccparser.GEOM],
        "postproc_func": lambda x: np.array(x[0])[:, 0].tolist()
        if x[0] is not None
        else None,
    },
    Y_ATOMS_COORDINATES: {
        "cckeys": [ccparser.GEOM],
        "postproc_func": lambda x: np.array(x[0])[:, 1].tolist()
        if x[0] is not None
        else None,
    },
    Z_ATOMS_COORDINATES: {
        "cckeys": [ccparser.GEOM],
        "postproc_func": lambda x: np.array(x[0])[:, 2].tolist()
        if x[0] is not None
        else None,
    },
    ROTATIONAL_CONSTANTS_STRING: {
        "cckeys": [ccparser.ROT_CONST],
        "postproc_func": lambda x: " ".join(str(rc) for rc in x[0])
        if x[0] is not None
        else None,
    },
    ROTATIONAL_CONSTANTS_NUMBER: {
        "cckeys": [ccparser.ROT_CONST_NR],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else None,
    },
    ROTATIONAL_SYMMETRY_NUMBER: {
        "cckeys": [ccparser.ROT_SYM_NR],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else None,
    },
    FREQUENCIES_STRING: {
        "cckeys": [ccparser.FREQ],
        "postproc_func": lambda x: " ".join(str(rc) for rc in x[0])
        if x[0] is not None
        else None,
    },
    FREQUENCIES_NUMBER: {
        "cckeys": [ccparser.FREQ_NR],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else None,
    },
    # energies
    ELECTRONIC_ENERGY: {
        "cckeys": [ccparser.ELECTRONIC_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    ELECTRONIC_AND_ZPE_ENERGY: {
        "cckeys": [ccparser.ELECTRONIC_ZPE_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    ZPE_ENERGY: {
        "cckeys": [
            ccparser.ZPE_ENERGY,
            ccparser.ELECTRONIC_ENERGY,
            ccparser.ELECTRONIC_ZPE_ENERGY,
        ],
        "postproc_func": lambda x: x[0]
        if x[0] is not None
        else x[2] - x[1]
        if x[1] is not None and x[2] is not None
        else None,
    },
    HOMO_ENERGY: {
        "cckeys": [ccparser.HOMO_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    HOMO_MIN_1_ENERGY: {
        "cckeys": [ccparser.HOMO_MIN_1_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    HOMO_MIN_2_ENERGY: {
        "cckeys": [ccparser.HOMO_MIN_2_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    LUMO_ENERGY: {
        "cckeys": [ccparser.LUMO_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    LUMO_PLUS_1_ENERGY: {
        "cckeys": [ccparser.LUMO_PLUS_1_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    LUMO_PLUS_2_ENERGY: {
        "cckeys": [ccparser.LUMO_PLUS_2_ENERGY],
        "postproc_func": lambda x: x[0],
    },
    # metadata
    PROGRAM_NAME: {
        "cckeys": [ccparser.PROGRAM_NAME],
        "postproc_func": lambda x: x[0],
    },
    PROGRAM_VERSION: {
        "cckeys": [ccparser.PROGRAM_VERSION],
        "postproc_func": lambda x: x[0],
    },
    RUN_DATE: {"cckeys": [ccparser.RUN_DATE], "postproc_func": lambda x: x[0]},
    LOG_FILE_NAME: {
        "cckeys": [ccparser.LOG_FILE_NAME],
        "postproc_func": lambda x: x[0],
    },
    CCLOG_SOURCE_LOCATION: {
        "cckeys": [handlers.CCLOG_SOURCE_LOCATION],
        "postproc_func": lambda x: x[0],
    },
}
