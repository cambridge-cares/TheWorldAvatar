__doc__ = """
This module defines all the ops json file keys.
"""

# on species
SPECIES_IRI = "Species_IRI"
SCAN_ATOMS_IRIS = "Scan_Atoms_Iris"
# atom indices w.r.t cc joob
SCAN_ATOMS_CCJOB_POSITIONS = "Scan_Atoms_Ccjob_Positions"
# scan coord stuff
SCAN_COORDINATE_TYPE = "Scan_Coordinate_Type"
SCAN_COORDINATE_VALUES = "Scan_Coordinate_Values"
SCAN_COORDINATE_UNIT = "Scan_Coordinate_Unit"
# cc job full iris
SCAN_POINT_JOBS_IRIS = "Scan_Point_Jobs_Iris"
# other
ENTRY_IRI = "Entry_IRI"
ENTRY_ID = "Entry_ID"


OPS_JSON_KEYS = [
    SPECIES_IRI,
    SCAN_ATOMS_IRIS,
    SCAN_ATOMS_CCJOB_POSITIONS,
    SCAN_COORDINATE_TYPE,
    SCAN_COORDINATE_VALUES,
    SCAN_COORDINATE_UNIT,
    SCAN_POINT_JOBS_IRIS,
    ENTRY_IRI,
    ENTRY_ID,
]
