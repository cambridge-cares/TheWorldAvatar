import chemaboxwriters.ontomops.handlers.ominp_json_keys as ominp_keys
from typing import List, Optional

__doc__ = """
This module defines all the om json file keys and the mapping 
on how to populate om json entries with the ominp json entries.
"""

# OM_JSON_KEYS
# -------------------------
MOPS_MOLECULAR_WEIGHT = "Mops_Molecular_Weight"
MOPS_POLYHEDRAL_SHAPE = "Mops_Polyhedral_Shape"
MOPS_POLYHEDRAL_SHAPE_SYMBOL = "Mops_Polyhedral_Shape_Symbol"
MOPS_SYMMETRY_POINT_GROUP = "Mops_Symmetry_Point_Group"
MOPS_CHARGE = "Mops_Charge"
MOPS_FORMULA = "Mops_Formula"
MOPS_LABEL = "Mops_Label"
MOPS_CAVITY_VOLUME = "Mops_Cavity_Volume"
MOPS_CCDC_NUMBER = "Mops_CCDC_Number"
MOPS_REFERENCE_DOI = "Mops_Reference_DOI"
MOPS_XYZ_GEOMETRY_FILE_URL = "Mops_XYZ_Geometry_File_URL"
SPECIES_IRIS = "Species_IRIs"
CBU_FORMULAS = "CBU_Formulas"
CBU_BINDING_SITES = "CBU_Binding_Sites"
CBU_BINDING_SITE_LABELS = "CBU_Binding_Site_Labels"
CBU_BINDING_SITE_COORD_NUMBERS = "CBU_Binding_Site_Coord_Numbers"
CBU_CORE_LABELS = "CBU_Core_Labels"
CBU_CORE_SUBSTITUENT_LABELS = "CBU_Core_Substituent_Labels"
CBU_SPACER_LABELS = "CBU_Spacer_Labels"
CBU_SPACER_SUBSTITUENT_LABELS = "CBU_Spacer_Substituent_Labels"
CBU_BINDING_DIRECTIONS = "CBU_Binding_Directions"
GBU_PLANARITIES = "GBU_Planarities"
GBU_MODULARITIES = "GBU_Modularities"
GBU_NUMBERS = "GBU_Numbers"
ASSEMBLY_MODEL_ID = "Assembly_Model_ID"
ENTRY_IRI = "Entry_IRI"
ENTRY_ID = "Entry_ID"


OM_JSON_KEYS = [
    MOPS_MOLECULAR_WEIGHT,
    MOPS_POLYHEDRAL_SHAPE,
    MOPS_POLYHEDRAL_SHAPE_SYMBOL,
    MOPS_SYMMETRY_POINT_GROUP,
    MOPS_CHARGE,
    MOPS_FORMULA,
    MOPS_LABEL,
    MOPS_CAVITY_VOLUME,
    MOPS_CCDC_NUMBER,
    MOPS_REFERENCE_DOI,
    MOPS_XYZ_GEOMETRY_FILE_URL,
    SPECIES_IRIS,
    CBU_FORMULAS,
    CBU_BINDING_SITES,
    CBU_BINDING_SITE_LABELS,
    CBU_BINDING_SITE_COORD_NUMBERS,
    CBU_CORE_LABELS,
    CBU_CORE_SUBSTITUENT_LABELS,
    CBU_SPACER_LABELS,
    CBU_SPACER_SUBSTITUENT_LABELS,
    CBU_BINDING_DIRECTIONS,
    GBU_PLANARITIES,
    GBU_MODULARITIES,
    GBU_NUMBERS,
    ASSEMBLY_MODEL_ID,
    ENTRY_IRI,
    ENTRY_ID,
]


def _merge_nested_values(data: Optional[List], child_key: str) -> Optional[List[str]]:
    if data is None:
        return
    merged_values = [sub_dict.get(child_key) for sub_dict in data]
    if None in merged_values:
        return
    return merged_values


# this defines on how assign the om json data using the ominp json data
#
OM_JSON_TO_OMINP_JSON_KEYS_MAP = {
    MOPS_MOLECULAR_WEIGHT: {
        "cckeys": [ominp_keys.MOPS_MOLECULAR_WEIGHT],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_POLYHEDRAL_SHAPE: {
        "cckeys": [ominp_keys.MOPS_POLYHEDRAL_SHAPE],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_POLYHEDRAL_SHAPE_SYMBOL: {
        "cckeys": [ominp_keys.MOPS_POLYHEDRAL_SHAPE_SYMBOL],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_SYMMETRY_POINT_GROUP: {
        "cckeys": [ominp_keys.MOPS_SYMMETRY_POINT_GROUP],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_CHARGE: {
        "cckeys": [ominp_keys.MOPS_CHARGE],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_FORMULA: {
        "cckeys": [ominp_keys.MOPS_FORMULA],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_LABEL: {
        "cckeys": [ominp_keys.MOPS_LABEL],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_CAVITY_VOLUME: {
        "cckeys": [ominp_keys.MOPS_CAVITY_VOLUME],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_CCDC_NUMBER: {
        "cckeys": [ominp_keys.MOPS_CCDC_NUMBER],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    MOPS_REFERENCE_DOI: {
        "cckeys": [ominp_keys.MOPS_REFERENCE_DOI],
        "postproc_func": lambda x: x[0] if x[0] is not None else None,
    },
    SPECIES_IRIS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(x[0], ominp_keys.SPECIES_IRI),
    },
    CBU_FORMULAS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(x[0], ominp_keys.CBU_FORMULA),
    },
    CBU_BINDING_SITES: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_BINDING_SITE
        ),
    },
    CBU_BINDING_SITE_LABELS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_BINDING_SITE_LABEL
        ),
    },
    CBU_BINDING_SITE_COORD_NUMBERS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_BINDING_SITE_COORD_NUMBER
        ),
    },
    CBU_CORE_LABELS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_CORE_LABEL
        ),
    },
    CBU_CORE_SUBSTITUENT_LABELS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_CORE_SUBSTITUENT_LABEL
        ),
    },
    CBU_SPACER_LABELS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_SPACER_LABEL
        ),
    },
    CBU_SPACER_SUBSTITUENT_LABELS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_SPACER_SUBSTITUENT_LABEL
        ),
    },
    CBU_BINDING_DIRECTIONS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.CBU_BINDING_DIRECTION
        ),
    },
    GBU_PLANARITIES: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(x[0], ominp_keys.GBU_PLANARITY),
    },
    GBU_MODULARITIES: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(
            x[0], ominp_keys.GBU_MODULARITY
        ),
    },
    GBU_NUMBERS: {
        "cckeys": [ominp_keys.MOPS_CHEMICAL_BUILDING_UNITS],
        "postproc_func": lambda x: _merge_nested_values(x[0], ominp_keys.GBU_NUMBER),
    },
}
