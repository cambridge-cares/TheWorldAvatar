import compchemparser.parsers.ccgaussian_parser as ccparser
import compchemparser.helpers.elements_data as el_data
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemutils.obabelutils.obconverter as obconverter
import numpy as np

__doc__ = """
This module defines all the os json file keys and the mapping 
on how to populate os json entries with the qc json entries.
"""

# OS_JSON_KEYS
# -------------------------
# atoms, molecules etc
EMPIRICAL_FORMULA = "Empirical_Formula"
ATOM_TOTAL_COUNTS = "Atom_Total_Counts"
PER_ATOM_TYPE_INDICES = "Per_Atom_Type_Indices"
ATOM_LIST = "Atom_List"
ELEMENTS = "Elements"
MOLECULAR_WEIGHT = "Molecular_Weight"
# spin, charge
SPIN_MULTIPLICITY = "Spin_Multiplicity"
TOTAL_CHARGE = "Total_Charge"
# geometry
GEOMETRY_STRING = "Geometry_String"
X_ATOMS_COORDINATES = "X_Atoms_Coordinates"
Y_ATOMS_COORDINATES = "Y_Atoms_Coordinates"
Z_ATOMS_COORDINATES = "Z_Atoms_Coordinates"
BOND_STRING = "Bond_String"
# identifiers
INCHI = "Inchi"
SMILES = "Smiles"
PUBCHEM_ALT_LABEL = "Pubchem_Alternative_Label"
CAS_NUMBER = "CAS_Number"
PUBCHEM_CID = "Pubchem_CID"
# enthalpy
STANDARD_ENTH_FORM = "Standard_Enthalpy_Formation"
STANDARD_ENTH_FORM_UNIT = "Standard_Enthalpy_Formation_Unit"
STANDARD_ENTH_FORM_PHASE = "Standard_Enthalpy_Formation_Phase"
STANDARD_ENTH_FORM_REF_TEMP = "Standard_Enthalpy_Formation_Ref_Temperature"
STANDARD_ENTH_FORM_REF_TEMP_UNIT = "Standard_Enthalpy_Formation_Ref_Temperature_Unit"
STANDARD_ENTH_FORM_PROVENANCE = "Standard_Enthalpy_Formation_Provenance"
# other
ENTRY_IRI = "Entry_IRI"
ENTRY_ID = "Entry_ID"


OS_JSON_KEYS = [
    # atoms, molecules etc
    EMPIRICAL_FORMULA,
    ATOM_TOTAL_COUNTS,
    PER_ATOM_TYPE_INDICES,
    ATOM_LIST,
    ELEMENTS,
    MOLECULAR_WEIGHT,
    # spin, charge
    SPIN_MULTIPLICITY,
    TOTAL_CHARGE,
    # geometry
    GEOMETRY_STRING,
    X_ATOMS_COORDINATES,
    Y_ATOMS_COORDINATES,
    Z_ATOMS_COORDINATES,
    BOND_STRING,
    # identifiers
    INCHI,
    SMILES,
    PUBCHEM_ALT_LABEL,
    CAS_NUMBER,
    PUBCHEM_CID,
    # enthalpy
    STANDARD_ENTH_FORM,
    STANDARD_ENTH_FORM_UNIT,
    STANDARD_ENTH_FORM_PHASE,
    STANDARD_ENTH_FORM_REF_TEMP,
    STANDARD_ENTH_FORM_REF_TEMP_UNIT,
    STANDARD_ENTH_FORM_PROVENANCE,
    #
    ENTRY_IRI,
]


# this defines on how assign the os json data using the qc json data
#
OS_JSON_TO_QC_JSON_KEYS_MAP = {
    EMPIRICAL_FORMULA: {
        "cckeys": [ccparser.EMP_FORMULA],
        "postproc_func": lambda x: utilsfunc.clean_qc_json_emp_formula(x[0])
        if x[0] is not None
        else None,
    },
    ATOM_TOTAL_COUNTS: {
        "cckeys": [ccparser.ATOM_COUNTS],
        "postproc_func": lambda x: list(x[0].values()) if x[0] is not None else None,
    },
    ATOM_LIST: {
        "cckeys": [ccparser.ATOM_TYPES],
        "postproc_func": lambda x: x[0],
    },
    PER_ATOM_TYPE_INDICES: {
        "cckeys": [ccparser.ATOM_TYPES],
        "postproc_func": lambda x: utilsfunc.get_atom_indices_from_qc_json(
            atom_list=x[0]
        )
        if x[0] is not None
        else None,
    },
    ELEMENTS: {
        "cckeys": [ccparser.ATOM_TYPES],
        # One could use set here rather than dict.fromkeys,
        # but set does not preserve the atoms order
        "postproc_func": lambda x: list(dict.fromkeys(x[0]))
        if x[0] is not None
        else x[0],
    },
    MOLECULAR_WEIGHT: {
        "cckeys": [ccparser.ATOM_MASSES, ccparser.ATOM_TYPES],
        "postproc_func": lambda x: sum(x[0])
        if x[0] is not None
        else el_data.get_molwt_from_atom_types(x[1])
        if x[1] is not None
        else None,
    },
    # spin, charge
    SPIN_MULTIPLICITY: {
        "cckeys": [ccparser.SPIN_MULT],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else x[0],
    },
    TOTAL_CHARGE: {
        "cckeys": [ccparser.FORMAL_CHARGE],
        "postproc_func": lambda x: int(x[0]) if x[0] is not None else 0,
    },
    # geometry_string, x,y,z coords
    GEOMETRY_STRING: {
        "cckeys": [ccparser.ATOM_TYPES, ccparser.GEOM],
        "postproc_func": lambda x: " ".join(
            utilsfunc.construct_xyz_string_from_atoms_and_coords(
                atoms=x[0], coords=x[1]
            ).split("\n")
        )
        if x[0] is not None and x[1] is not None
        else None,
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
    BOND_STRING: {
        "cckeys": [ccparser.ATOM_TYPES, ccparser.GEOM],
        "postproc_func": lambda x: utilsfunc.construct_bond_string(
            atoms=x[0],
            coords=x[1],
        )
        if x[0] is not None and x[1] is not None
        else None,
    },
    INCHI: {
        "cckeys": [ccparser.ATOM_TYPES, ccparser.GEOM],
        "postproc_func": lambda x: obconverter.obConvert(
            utilsfunc.construct_xyz_file_string_from_atoms_and_coords(x[0], x[1]),
            "xyz",
            "inchi",
        )
        if x[0] is not None and x[1] is not None
        else None,
    },
    SMILES: {
        "cckeys": [ccparser.ATOM_TYPES, ccparser.GEOM],
        "postproc_func": lambda x: obconverter.obConvert(
            utilsfunc.construct_xyz_file_string_from_atoms_and_coords(x[0], x[1]),
            "xyz",
            "smi",
        )
        if x[0] is not None and x[1] is not None
        else None,
    },
}
