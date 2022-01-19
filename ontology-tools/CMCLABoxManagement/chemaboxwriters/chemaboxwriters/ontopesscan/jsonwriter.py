from chemutils.mathutils.linalg import getXYZPointsDistance, getPlaneAngle, getDihedralAngle
from chemaboxwriters.common.utilsfunc import get_random_id
import json
import numpy as np
import chemaboxwriters.common.globals as globals
from compchemparser.parsers.ccgaussian_parser import GEOM

SCAN_COORDINATE_ATOMS_IRIS='ScanCoordinateAtomsIRIs'
SCAN_COORDINATE_TYPE='ScanCoordinateType'
SCAN_COORDINATE_UNIT='ScanCoordinateUnit'
SCAN_COORDINATE_VALUE='ScanCoordinateValue'
SCAN_POINTS_JOBS='ScanPointsJobs'
SCAN_ATOM_IDS= 'ScanAtomIDs'

def ops_jsonwriter(file_paths, os_iris, os_atoms_iris, oc_atoms_pos, random_id=""):
    data_out ={}
    data_out[globals.SPECIES_IRI] = os_iris.split(',')
    data_out[SCAN_COORDINATE_ATOMS_IRIS] = os_atoms_iris.split(',')
    data_out[SCAN_ATOM_IDS] = " ".join(oc_atoms_pos.split(',')[:])
    oc_atoms_pos = [int(at_pos)-1 for at_pos in oc_atoms_pos.split(',')]

    ndegrees = len(os_atoms_iris.split(','))
    if ndegrees == 2:
        data_out[SCAN_COORDINATE_TYPE] = 'DistanceCoordinate'
        data_out[SCAN_COORDINATE_UNIT] = 'Angstrom'
    else:
        data_out[SCAN_COORDINATE_UNIT] = 'Degree'
        if ndegrees == 3:
            data_out[SCAN_COORDINATE_TYPE] = 'AngleCoordinate'
        else:
            data_out[SCAN_COORDINATE_TYPE] = 'DihedralAngleCoordinate'

    if not random_id: random_id = get_random_id()
    data_out[globals.ENTRY_UUID]= random_id
    data_out[globals.ENTRY_IRI]='PotentialEnergySurfaceScan_'+random_id

    scanCoordinateValue = []
    ontoCompChemJobs = []

    for file_path in file_paths:

        with open(file_path, 'r') as file_handle:
            data_item = json.load(file_handle)

        ontoCompChemJobs.append(data_item[globals.ENTRY_IRI])
        xyz = np.array(data_item[GEOM])

        if ndegrees==2:
            scanAtomsPos = xyz[oc_atoms_pos]
            scanCoordinateValue.append(getXYZPointsDistance(scanAtomsPos[0],scanAtomsPos[1]))
        elif ndegrees==3:
            scanAtomsPos =  xyz[oc_atoms_pos]
            scanCoordinateValue.append(getPlaneAngle(scanAtomsPos[0],scanAtomsPos[1],scanAtomsPos[2]))

        elif ndegrees==4:
            scanAtomsPos =  xyz[oc_atoms_pos]
            scanCoordinateValue.append(getDihedralAngle(scanAtomsPos[0],scanAtomsPos[1],scanAtomsPos[2],scanAtomsPos[3]))

    scanCoordinateValue, ontoCompChemJobs = zip(*sorted(zip(scanCoordinateValue, ontoCompChemJobs)))
    scanCoordinateValue = list(scanCoordinateValue)
    ontoCompChemJobs = list(ontoCompChemJobs)

    data_out[SCAN_COORDINATE_VALUE]=scanCoordinateValue
    data_out[SCAN_POINTS_JOBS]=ontoCompChemJobs

    return [json.dumps(data_out)]