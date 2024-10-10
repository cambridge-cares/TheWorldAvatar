# This file instantiates the json for AM and CBU geometries
from ontomops import *

def instantiate_cbu_from_json(cbu_json, gbu):
    cbu_binding_points = {}
    cbu_atoms = {}
    cbu_center_x = 0.0
    cbu_center_y = 0.0
    cbu_center_z = 0.0
    cbu_center = None

    for k, v in cbu_json.items():
        if v['atom'] == 'X':
            pt = BindingSite(
                hasOuterCoordinationNumber=2, # NOTE: This is a placeholder TODO pull it from KG
                hasBindingPoint=BindingPoint(hasX=v['coordinate_x'], hasY=v['coordinate_y'], hasZ=v['coordinate_z'])
            )
            cbu_binding_points[k] = pt
        elif v['atom'] == 'Center':
            cbu_center = Point(x=v['coordinate_x'], y=v['coordinate_y'], z=v['coordinate_z'])
        else:
            pt = Point(x=v['coordinate_x'], y=v['coordinate_y'], z=v['coordinate_z'], label=v['atom'])
            cbu_center_x += v['coordinate_x']
            cbu_center_y += v['coordinate_y']
            cbu_center_z += v['coordinate_z']
            cbu_atoms[k] = pt

    if cbu_center is None:
        cbu_center_x = cbu_center_x / len(cbu_atoms)
        cbu_center_y = cbu_center_y / len(cbu_atoms)
        cbu_center_z = cbu_center_z / len(cbu_atoms)
        cbu_center = Point(x=cbu_center_x, y=cbu_center_y, z=cbu_center_z)

    cbu = ChemicalBuildingUnit(
        hasBindingDirection=DirectBinding(), # TODO this should be pulled from KG
        hasBindingSite=[cbu_binding_points[k] for k in cbu_binding_points],
        isFunctioningAs=gbu, # TODO this should be pulled from KG
        hasCharge=set(), # TODO this should be pulled from KG
        hasMolecularWeight=set(), # TODO this should be pulled from KG
        hasGeometry=ontospecies.Geometry(
            hasGeometryFile=set(), # TODO this should be pulled from KG
            hasPoints=[cbu_atoms[k] for k in cbu_atoms],
        ), # TODO this should be pulled from KG
        hasCBUFormula='', # TODO this should be pulled from KG
        hasCBUCoordinateCenter=CBUCoordinateCenter(hasX=cbu_center.x, hasY=cbu_center.y, hasZ=cbu_center.z)
    )
    return cbu

# am_json_4_planar_2_bent = am_json['(4-planar)x6(2-bent)x12_Oh']
# am_json_4_planar_2_bent = am_json['(4-planar)x12(2-bent)x24_Oh']
def instantiate_mop(
    am_json, cbu1_json, cbu2_json,
    gbu_type_1: GenericBuildingUnitType, gbu_type_2: GenericBuildingUnitType,
    gbu_number_1: int, gbu_number_2: int,
    am_symmetry: str
):
    coordinate_point_dict = {}
    connecting_point_dict = {}
    # {
    #     "Key": "Dummy_1",
    #     "Label": "Dummy",
    #     "X": -0.78135968,
    #     "Y": -3.22398966,
    #     "Z": -2.1214766750000003,
    #     "Positions": [
    #         "Position_12",
    #         "Position_1"
    #     ]
    # },
    for i in range(len(am_json)):
        if am_json[i]['Label'] == 'Dummy':
            dummy = GBUConnectingPoint(hasX=am_json[i]['X'], hasY=am_json[i]['Y'], hasZ=am_json[i]['Z'])
            connecting_point_dict[am_json[i]['Key']] = dummy

    # {
    #     "Key": "Position_36",
    #     "Label": "2-bent",
    #     "X": -3.76774533,
    #     "Y": 1.56271936,
    #     "Z": 1.56271936,
    #     "Neighbors": [
    #         {
    #             "Key": "Position_10",
    #             "Label": "4-planar",
    #             "Distance": 2.20762579732423
    #         },
    #         {
    #             "Key": "Position_17",
    #             "Label": "4-planar",
    #             "Distance": 2.20762579732423
    #         }
    #     ],
    #     "ClosestDummies": [
    #         "Dummy_30",
    #         "Dummy_48"
    #     ]
    # },
    for i in range(len(am_json)):
        if am_json[i]['Label'] in [gbu_type_1.label, gbu_type_2.label]:
            coord = GBUCoordinateCenter(
                hasX=am_json[i]['X'],
                hasY=am_json[i]['Y'],
                hasZ=am_json[i]['Z'],
                hasGBUConnectingPoint=[connecting_point_dict[d] for d in am_json[i]['ClosestDummies']]
            )
            if am_json[i]['Label'] in coordinate_point_dict:
                coordinate_point_dict[am_json[i]['Label']].append(coord)
            else:
                coordinate_point_dict[am_json[i]['Label']] = [coord]
    # instantiate the GBUs
    gbu1 = GenericBuildingUnit(hasGBUType=gbu_type_1, hasGBUCoordinateCenter=coordinate_point_dict[gbu_type_1.label])
    gbu2 = GenericBuildingUnit(hasGBUType=gbu_type_2, hasGBUCoordinateCenter=coordinate_point_dict[gbu_type_2.label])
    # instantiate the CBUs
    cbu1 = instantiate_cbu_from_json(cbu1_json, gbu1)
    cbu2 = instantiate_cbu_from_json(cbu2_json, gbu2)
    
    am = AssemblyModel(
        hasGenericBuildingUnit=[gbu1, gbu2],
        hasGenericBuildingUnitNumber=[
            GenericBuildingUnitNumber(isNumberOf=gbu1, hasUnitNumberValue=gbu_number_1),
            GenericBuildingUnitNumber(isNumberOf=gbu2, hasUnitNumberValue=gbu_number_2)],
        hasPolyhedralShape=set(), # TODO this should be pulled from KG
        hasSymmetryPointGroup=am_symmetry, # TODO this should be pulled from KG
        hasGBUCoordinateCenter=[c for g in list(coordinate_point_dict.values()) for c in g],
        hasGBUConnectingPoint=list(connecting_point_dict.values())
    )
    mop = MetalOrganicPolyhedron(
        hasAssemblyModel=am,
        hasCavity=set(), # TODO this should be pulled from KG
        hasChemicalBuildingUnit=[cbu1, cbu2],
        hasProvenance=set(), # TODO this should be pulled from KG
        hasCharge=set(), # TODO this should be pulled from KG
        hasMolecularWeight=set(), # TODO this should be pulled from KG
        hasGeometry=set(), # TODO this should be pulled from KG
        hasCCDCNumber=set(), # TODO this should be pulled from KG
        hasMOPFormula='' # TODO this should be pulled from KG
    )
    return mop, cbu1, cbu2, gbu1, gbu2
