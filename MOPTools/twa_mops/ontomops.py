from __future__ import annotations
from typing import Optional, Dict, List
from scipy.optimize import fsolve
import plotly.express as px
import pandas as pd
import numpy as np
import math
import json

from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty, KnowledgeGraph
import ontospecies
import om

from geo import Point, Vector, Line, Plane, RotationMatrix

HALF_BOND_LENGTH = 1.4 / 2
BINDING_FRAGMENT_METAL = 'Metal'
BINDING_FRAGMENT_CO2 = 'CO2'
BINDING_FRAGMENT_O = 'O'
BINDING_FRAGMENT_N2 = 'N2'
GBU_TYPE_2_BENT = '2-bent'
GBU_TYPE_2_LINEAR = '2-linear'
GBU_TYPE_3_PLANAR = '3-planar'
GBU_TYPE_3_PYRAMIDAL = '3-pyramidal'
GBU_TYPE_4_PLANAR = '4-planar'
GBU_TYPE_4_PYRAMIDAL = '4-pyramidal'
GBU_TYPE_5_PYRAMIDAL = '5-pyramidal'


class OntoMOPs(BaseOntology):
    base_url = 'https://www.theworldavatar.com/kg'
    namespace = 'ontomops'
    owl_versionInfo = '1.1-ogm'
    rdfs_comment = 'An ontology developed for representing Metal-Organic Polyhedra (MOPs). This is object graph mapper (OGM) version.'


# object properties
HasAssemblyModel = ObjectProperty.create_from_base('HasAssemblyModel', OntoMOPs)
HasBindingDirection = ObjectProperty.create_from_base('HasBindingDirection', OntoMOPs)
HasBindingSite = ObjectProperty.create_from_base('HasBindingSite', OntoMOPs)
HasCavity = ObjectProperty.create_from_base('HasCavity', OntoMOPs)
HasChemicalBuildingUnit = ObjectProperty.create_from_base('HasChemicalBuildingUnit', OntoMOPs)
HasGenericBuildingUnit = ObjectProperty.create_from_base('HasGenericBuildingUnit', OntoMOPs)
HasGenericBuildingUnitNumber = ObjectProperty.create_from_base('HasGenericBuildingUnitNumber', OntoMOPs)
HasMOPCavityVolume = ObjectProperty.create_from_base('HasMOPCavityVolume', OntoMOPs)
HasPolyhedralShape = ObjectProperty.create_from_base('HasPolyhedralShape', OntoMOPs)
HasProvenance = ObjectProperty.create_from_base('HasProvenance', OntoMOPs)
IsFunctioningAs = ObjectProperty.create_from_base('IsFunctioningAs', OntoMOPs)
IsNumberOf = ObjectProperty.create_from_base('IsNumberOf', OntoMOPs)
# additions for assembler
HasGBUConnectingPoint = ObjectProperty.create_from_base('HasGBUConnectingPoint', OntoMOPs)
HasGBUCoordinateCenter = ObjectProperty.create_from_base('HasGBUCoordinateCenter', OntoMOPs)
HasCBUAssemblyCenter = ObjectProperty.create_from_base('HasCBUAssemblyCenter', OntoMOPs)
HasBindingPoint = ObjectProperty.create_from_base('HasBindingPoint', OntoMOPs)
HasGBUType = ObjectProperty.create_from_base('HasGBUType', OntoMOPs)


# data properties
HasCBUFormula = DatatypeProperty.create_from_base('HasCBUFormula', OntoMOPs)
HasCCDCNumber = DatatypeProperty.create_from_base('HasCCDCNumber', OntoMOPs)
HasMOPFormula = DatatypeProperty.create_from_base('HasMOPFormula', OntoMOPs)
HasModularity = DatatypeProperty.create_from_base('HasModularity', OntoMOPs)
HasOuterCoordinationNumber = DatatypeProperty.create_from_base('HasOuterCoordinationNumber', OntoMOPs)
HasPlanarity = DatatypeProperty.create_from_base('HasPlanarity', OntoMOPs)
HasReferenceDOI = DatatypeProperty.create_from_base('HasReferenceDOI', OntoMOPs)
HasSymbol = DatatypeProperty.create_from_base('HasSymbol', OntoMOPs)
HasSymmetryPointGroup = DatatypeProperty.create_from_base('HasSymmetryPointGroup', OntoMOPs)
HasUnitNumberValue = DatatypeProperty.create_from_base('HasUnitNumberValue', OntoMOPs)
# additions for assembler
HasBindingFragment = DatatypeProperty.create_from_base('HasBindingFragment', OntoMOPs)
HasX = DatatypeProperty.create_from_base('HasX', OntoMOPs)
HasY = DatatypeProperty.create_from_base('HasY', OntoMOPs)
HasZ = DatatypeProperty.create_from_base('HasZ', OntoMOPs)
RefersTo = ObjectProperty.create_from_base('RefersTo', OntoMOPs)
Connects = ObjectProperty.create_from_base('Connects', OntoMOPs)


# classes
class MolecularCage(BaseClass):
    rdfs_isDefinedBy = OntoMOPs

class CoordinationCage(MolecularCage):
    pass

class GenericBuildingUnitType(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasModularity: HasModularity[int]
    hasPlanarity: HasPlanarity[str]

    @property
    def label(self):
        return f"{list(self.hasModularity)[0]}-{list(self.hasPlanarity)[0]}"

class CoordinatePoint(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasX: HasX[float]
    hasY: HasY[float]
    hasZ: HasZ[float]

    @property
    def coordinates(self):
        return Point(x=list(self.hasX)[0], y=list(self.hasY)[0], z=list(self.hasZ)[0])

class GenericBuildingUnit(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasGBUType: HasGBUType[GenericBuildingUnitType]
    hasGBUCoordinateCenter: HasGBUCoordinateCenter[GBUCoordinateCenter]

    @property
    def gbu_type(self):
        return list(self.hasGBUType)[0].label

    @property
    def is_4_planar(self):
        return list(self.hasGBUType)[0].label == '4-planar'

    @property
    def modularity(self):
        return list(list(self.hasGBUType)[0].hasModularity)[0]

class GenericBuildingUnitNumber(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    isNumberOf: IsNumberOf[GenericBuildingUnit]
    hasUnitNumberValue: HasUnitNumberValue[int]

class AssemblyModel(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasGenericBuildingUnit: HasGenericBuildingUnit[GenericBuildingUnit]
    hasGenericBuildingUnitNumber: HasGenericBuildingUnitNumber[GenericBuildingUnitNumber]
    hasPolyhedralShape: HasPolyhedralShape[PolyhedralShape]
    hasSymmetryPointGroup: HasSymmetryPointGroup[str]
    hasGBUCoordinateCenter: HasGBUCoordinateCenter[GBUCoordinateCenter]
    hasGBUConnectingPoint: HasGBUConnectingPoint[GBUConnectingPoint]

    @staticmethod
    def process_geometry_json(am_json, gbu_type_1_label, gbu_type_2_label):
        """
        Example JSON for AM (the `am_json` will be the list of dictionaries within the key of the AM label in the JSON file):
        {
            "(5-pyramidal)x12(2-linear)x30_Ih": [
                {
                    "Key": "Position_1",
                    "Label": "5-pyramidal",
                    "X": 2.6368,
                    "Y": 2.7551,
                    "Z": 1.2068,
                    "Neighbors": [
                        {
                            "Key": "Position_13",
                            "Label": "2-linear",
                            "Distance": 2.1029
                        },
                        {
                            "Key": "Position_15",
                            "Label": "2-linear",
                            "Distance": 2.1029
                        },
                        ...
                    ],
                    "ClosestDummies": ["Dummy_1", "Dummy_2", "Dummy_3", ...]
                },
                ...
                {
                    "Key": "Dummy_1",
                    "Label": "Dummy",
                    "X": 2.8490,
                    "Y": 1.7266,
                    "Z": 1.2590,
                    "Positions": ["Position_13", "Position_1"]
                },
                ...
                {
                    "Key": "Center",
                    "Label": "Center",
                    "X": 0.0,
                    "Y": 0.0,
                    "Z": 0.0
                }
            ]
        }
        """
        coordinate_point_dict = {}
        connecting_point_dict = {}
        # handle dummy blocks `{"Label": "Dummy", ...}`
        for i in range(len(am_json)):
            if am_json[i]['Label'] == 'Dummy':
                dummy = GBUConnectingPoint(hasX=am_json[i]['X'], hasY=am_json[i]['Y'], hasZ=am_json[i]['Z'])
                connecting_point_dict[am_json[i]['Key']] = dummy

        # handle position blocks `{"Label": "5-pyramidal", ...}`
        # TODO the part that getting the gbu_type_1 and gbu_type_2 can be optimised
        for i in range(len(am_json)):
            if am_json[i]['Label'] in [gbu_type_1_label, gbu_type_2_label]:
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

        return coordinate_point_dict, connecting_point_dict

    def add_coordinates_from_json(self, am_json):
        gbus = list(self.hasGenericBuildingUnit)
        gbu1 = gbus[0]
        gbu2 = gbus[1]
        coordinate_point_dict, connecting_point_dict = self.__class__.process_geometry_json(am_json, gbu1.gbu_type, gbu2.gbu_type)
        gbu1.hasGBUCoordinateCenter = coordinate_point_dict[gbu1.gbu_type]
        gbu2.hasGBUCoordinateCenter = coordinate_point_dict[gbu2.gbu_type]
        self.hasGBUCoordinateCenter = [c for g in list(coordinate_point_dict.values()) for c in g]
        self.hasGBUConnectingPoint = list(connecting_point_dict.values())

    @classmethod
    def from_geometry_json(
        cls,
        am_json,
        gbu_type_1: GenericBuildingUnitType,
        gbu_type_2: GenericBuildingUnitType,
        gbu_number_1: int,
        gbu_number_2: int,
        am_symmetry: str,
        polyhedral_shape: str = None,
    ):

        # process the AM geometry JSON
        coordinate_point_dict, connecting_point_dict = cls.process_geometry_json(am_json, gbu_type_1.label, gbu_type_2.label)

        # instantiate the GBUs
        gbu1 = GenericBuildingUnit(hasGBUType=gbu_type_1, hasGBUCoordinateCenter=coordinate_point_dict[gbu_type_1.label])
        gbu2 = GenericBuildingUnit(hasGBUType=gbu_type_2, hasGBUCoordinateCenter=coordinate_point_dict[gbu_type_2.label])

        return cls(
            hasGenericBuildingUnit=[gbu1, gbu2],
            hasGenericBuildingUnitNumber=[
                GenericBuildingUnitNumber(isNumberOf=gbu1, hasUnitNumberValue=gbu_number_1),
                GenericBuildingUnitNumber(isNumberOf=gbu2, hasUnitNumberValue=gbu_number_2)],
            hasPolyhedralShape=polyhedral_shape if polyhedral_shape is not None else set(),
            hasSymmetryPointGroup=am_symmetry,
            hasGBUCoordinateCenter=[c for g in list(coordinate_point_dict.values()) for c in g],
            hasGBUConnectingPoint=list(connecting_point_dict.values())
        )

    def gbu_of_type(self, gbu_type):
        return [gbu for gbu in self.hasGenericBuildingUnit if gbu_type == gbu.gbu_type]

    @property
    def pairs_of_connected_gbus(self) -> Dict:
        pairs = {}
        for cc in self.hasGBUCoordinateCenter:
            cc: GBUCoordinateCenter
            cps = list(cc.hasGBUConnectingPoint)
            for cp in cps:
                if cp.instance_iri not in pairs:
                    pairs[cp.instance_iri] = [cc]
                else:
                    pairs[cp.instance_iri].append(cc)
        return pairs

class Provenance(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasReferenceDOI: HasReferenceDOI[str]

class Volume(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasValue: om.HasValue[om.Measure]

class Cavity(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasMOPCavityVolume: HasMOPCavityVolume[Volume]

class PolyhedralShape(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasSymbol: HasSymbol[str]

class BindingPoint(CoordinatePoint):
    pass

class BindingSite(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasOuterCoordinationNumber: HasOuterCoordinationNumber[int]
    hasBindingPoint: HasBindingPoint[BindingPoint]
    hasBindingFragment: HasBindingFragment[str]
    temporarily_blocked: Optional[bool] = False

    @property
    def binding_coordinates(self) -> Point:
        return list(self.hasBindingPoint)[0].coordinates

class MetalSite(BindingSite):
    pass

class OrganicSite(BindingSite):
    pass

class BindingDirection(BaseClass):
    rdfs_isDefinedBy = OntoMOPs

class DirectBinding(BindingDirection):
    pass

class SidewayBinding(BindingDirection):
    pass

class GBUConnectingPoint(CoordinatePoint):
    pass

class GBUCoordinateCenter(CoordinatePoint):
    hasGBUConnectingPoint: HasGBUConnectingPoint[GBUConnectingPoint]

    @property
    def vector_from_am_center(self) -> Vector:
        # NOTE TODO this assumes that the center of the AM is the (0, 0, 0) point
        return Vector.from_two_points(start=Point(x=0, y=0, z=0), end=self.coordinates)

    @property
    def distance_to_am_center(self):
        return self.coordinates.get_distance_to(Point(x=0, y=0, z=0))

    @property
    def vector_to_connecting_point_plane(self):
        # TODO add safeguards for str IRIs
        connecting_points = [p.coordinates for p in self.hasGBUConnectingPoint]
        if len(connecting_points) < 3:
            line = Line.from_two_points(start=connecting_points[0], end=connecting_points[1])
            if line.is_point_on_line(self.coordinates):
                # if the center point is on the line then we take the normal vector of the line
                # the normal vector has to be on the plane formed by the connecting points and the AM center point
                # i.e. Point(0, 0, 0)
                # TODO check if it is safe to make this assumption for all CBUs
                _v = line.normal_vector_from_point_to_line(Point(x=0, y=0, z=0))
                # flip it to point in the direction of center
                v = Vector.from_array(-_v.as_array)
            else:
                v = line.normal_vector_from_point_to_line(self.coordinates)
        else:
            plane = Plane.fit_from_points([bs for bs in connecting_points])
            v = plane.normal_vector_from_point_to_plane(self.coordinates)
        return v

    @property
    def vector_to_farthest_connecting_point(self):
        # find the plane perpendicular to the vector to the average connecting point
        plane = Plane.from_point_and_normal(self.coordinates, self.vector_to_connecting_point_plane)
        # project all connecting points onto the plane
        projected_points = [plane.project_point(p.coordinates) for p in self.hasGBUConnectingPoint]
        # find the farthest connecting point and construct a vector from center to it
        farthest_projected_point = self.coordinates.farthest_point(projected_points)
        vector = Vector.from_two_points(start=self.coordinates, end=farthest_projected_point)
        return vector

    @property
    def vector_to_shortest_side(self):
        # find the plane perpendicular to the vector to the average connecting point
        plane = Plane.from_point_and_normal(self.coordinates, self.vector_to_connecting_point_plane)
        # project all connecting points onto the plane
        projected_points = [plane.project_point(p.coordinates) for p in self.hasGBUConnectingPoint]
        # find the closest pair of connecting points and construct a vector connecting the center to the line connecting them
        closest_pair = Point.closest_pair(projected_points)
        line = Line.from_two_points(start=closest_pair[0], end=closest_pair[1])
        v = line.normal_vector_from_point_to_line(self.coordinates)
        return v

class CBUAssemblyCenter(CoordinatePoint):
    pass

class ChemicalBuildingUnit(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasBindingDirection: HasBindingDirection[BindingDirection]
    hasBindingSite: HasBindingSite[BindingSite]
    isFunctioningAs: IsFunctioningAs[GenericBuildingUnit]
    hasCharge: ontospecies.HasCharge[ontospecies.Charge]
    hasMolecularWeight: ontospecies.HasMolecularWeight[ontospecies.MolecularWeight]
    hasGeometry: ontospecies.HasGeometry[ontospecies.Geometry]
    hasCBUFormula: HasCBUFormula[str]
    hasCBUAssemblyCenter: HasCBUAssemblyCenter[CBUAssemblyCenter]

    @property
    def charge(self):
        return list(list(list(self.hasCharge)[0].hasValue)[0].hasNumericalValue)[0]

    @property
    def molecular_weight(self):
        return list(list(list(self.hasMolecularWeight)[0].hasValue)[0].hasNumericalValue)[0]

    @property
    def cbu_formula(self):
        return f"[{list(self.hasCBUFormula)[0].rstrip(']').lstrip('[')}]"

    @property
    def assembly_center(self):
        return list(self.hasCBUAssemblyCenter)[0].coordinates

    @property
    def highest_modularity_gbu(self):
        return max([gbu for gbu in list(self.isFunctioningAs)], key=lambda x: x.modularity)

    @property
    def is_metal_cbu(self):
        return all([isinstance(bs, MetalSite) for bs in list(self.hasBindingSite)])

    @property
    def active_binding_sites(self):
        return [bs for bs in list(self.hasBindingSite) if not bs.temporarily_blocked]

    def allocate_active_binding_sites(self, num: int):
        for bs in list(self.hasBindingSite)[num:]:
            bs.temporarily_blocked = True

    def release_blocked_binding_sites(self):
        for bs in self.hasBindingSite:
            bs.temporarily_blocked = False

    def load_geometry_from_fileserver(self, sparql_client):
        return list(self.hasGeometry)[0].load_xyz_from_geometry_file(sparql_client)

    def add_binding_site_and_assembly_center_from_json(
        self, cbu_json_fpath, ocn, binding_fragment: str, gbu_type: str, metal_site: bool = False
    ):
        with open(cbu_json_fpath, "r") as file:
            cbu_json = json.load(file)
        binding_sides, assemb_center, atom_points = self.__class__.process_geometry_json(
            cbu_json, ocn, binding_fragment, gbu_type, metal_site)
        self.hasBindingSite = binding_sides
        self.hasCBUAssemblyCenter = assemb_center

    @staticmethod
    def process_geometry_json(cbu_json, ocn: int, binding_fragment: str, gbu_type: str, metal_site: bool = False):
        """
        Example of CBU json consisting of real atoms, dummy atoms as binding sites, and an optional "CENTER" point.
        Note that below coordinates are just for demonstration purpose, they do not represent any actual molecules.
        {
            "atom_uuid": {"atom": "C", "coordinate_x": 0.0, "coordinate_y": 0.0, "coordinate_z": 0.0},
            ...
            "dummy_uuid": {"atom": "X", "coordinate_x": -0.1, "coordinate_y": 2.1, "coordinate_z": 5.3},
            "CENTER": {"atom": "CENTER", "coordinate_x": 0.0, "coordinate_y": 0.0, "coordinate_z": 0.0}
        }
        """
        cbu_binding_points = {}
        lst_binding_sits = []
        cbu_atoms = {}
        atom_points = []
        cbu_atoms_acc_x = 0.0
        cbu_atoms_acc_y = 0.0
        cbu_atoms_acc_z = 0.0
        cbu_assemb_center = None

        # iterate through the json file and process the coordinates
        _bs_clz = MetalSite if metal_site else OrganicSite
        for k, v in cbu_json.items():
            if v['atom'] == 'X':
                pt = _bs_clz(
                    hasOuterCoordinationNumber=ocn,
                    hasBindingPoint=BindingPoint(hasX=v['coordinate_x'], hasY=v['coordinate_y'], hasZ=v['coordinate_z']),
                    hasBindingFragment=binding_fragment,
                )
                cbu_binding_points[k] = pt
                lst_binding_sits.append(pt)
            elif str(v['atom']).lower() == 'center':
                cbu_assemb_center = Point(x=v['coordinate_x'], y=v['coordinate_y'], z=v['coordinate_z'])
            else:
                pt = Point(x=v['coordinate_x'], y=v['coordinate_y'], z=v['coordinate_z'], label=v['atom'])
                cbu_atoms_acc_x += v['coordinate_x']
                cbu_atoms_acc_y += v['coordinate_y']
                cbu_atoms_acc_z += v['coordinate_z']
                cbu_atoms[k] = pt
                atom_points.append(pt)

        # compute the CBU assembly center, which is the geometry (coordinates) center point of the CBU structure (average of all atoms)
        # projected on the normal vector of the plane formed by the binding sites (dummy atoms) that pass through the circumcenter point of all binding sites
        if cbu_assemb_center is None:
            lst_binding_points = [cbu_binding_points[k].binding_coordinates for k in cbu_binding_points]
            if len(lst_binding_points) > 2:
                cbu_binding_sites_plane = Plane.fit_from_points(lst_binding_points)
                cbu_binding_sites_circumcenter = Point.fit_circle_2d(lst_binding_points)[0]
                if 'planar' in gbu_type.lower():
                    cbu_assemb_center = cbu_binding_sites_circumcenter
                else:
                    cbu_geo_center = Point.centroid(atom_points)
                    line = Line(point=cbu_binding_sites_circumcenter, direction=cbu_binding_sites_plane.normal)
                    cbu_assemb_center = line.project_point(cbu_geo_center)
            else:
                bindingsite_mid_point = Point.mid_point(*lst_binding_points)
                if 'linear' in gbu_type.lower():
                    # if the CBU is expected to function as 2-linear, take the average of the two binding sites
                    cbu_assemb_center = bindingsite_mid_point
                else:
                    # if the CBU is expected to function as 2-bent, then compute the intersection of the two vectors
                    # to be used as the third point to fit a plane with the two binding sites
                    if binding_fragment == 'CO2':
                        # first find the closest two O atoms and the C atoms for each binding sites
                        v_dct = {}
                        lst_pts_for_plane = []
                        for k in cbu_binding_points:
                            bp: Point = cbu_binding_points[k].binding_coordinates
                            ranked_atoms = bp.rank_distanct_to_points(atom_points)
                            closest_two_O = [a for a in ranked_atoms if a.label == 'O'][:2]
                            closest_C = [a for a in ranked_atoms if a.label == 'C'][0]
                            # find the average of the O atoms and use it to form line with the closest C atom
                            avg_O = Point.mid_point(*closest_two_O)
                            v_dct[k] = {'avg_O': avg_O, 'closest_C': closest_C, 'line': Line.from_two_points(start=avg_O, end=closest_C)}
                            lst_pts_for_plane.extend([closest_C, avg_O])
                        # find the plane from these points
                        plane = Plane.fit_from_points(lst_pts_for_plane)
                        # find the intersection of these lines when they are projected on the plane
                        proj_lines = [v['line'] for v in v_dct.values()]
                        intersection = plane.find_intersection_of_lines_projected(*proj_lines)
                        # use the intersection and two binding site to form the plane
                        plane_of_bs = Plane.from_three_points(pt1=intersection, pt2=lst_binding_points[0], pt3=lst_binding_points[1])
                        perpendicular_bisector = plane_of_bs.find_perpendicular_bisector_on_plane(*lst_binding_points)
                        cbu_assemb_center = perpendicular_bisector.project_point(intersection)
                    else:
                        raise NotImplementedError(f'CBUs functioning as a {gbu_type} with binding fragment {binding_fragment} is not yet supported.')
        assemb_center = CBUAssemblyCenter(hasX=cbu_assemb_center.x, hasY=cbu_assemb_center.y, hasZ=cbu_assemb_center.z)

        return lst_binding_sits, assemb_center, atom_points

    @classmethod
    def from_geometry_json(cls, cbu_formula, cbu_json, charge, ocn, binding_fragment, gbu_type, gbu: str = None, direct_binding: bool = True, metal_site: bool = False):
        """
        Example of CBU json consisting of real atoms, dummy atoms as binding sites, and an optional "CENTER" point.
        Note that below coordinates are just for demonstration purpose, they do not represent any actual molecules.
        {
            "atom_uuid": {"atom": "C", "coordinate_x": 0.0, "coordinate_y": 0.0, "coordinate_z": 0.0},
            ...
            "dummy_uuid": {"atom": "X", "coordinate_x": -0.1, "coordinate_y": 2.1, "coordinate_z": 5.3},
            "CENTER": {"atom": "CENTER", "coordinate_x": 0.0, "coordinate_y": 0.0, "coordinate_z": 0.0}
        }
        """

        if not direct_binding:
            raise NotImplementedError("Non-direct binding, e.g. side binding, is not yet supported.")

        binding_sides, assemb_center, atom_points = cls.process_geometry_json(cbu_json, ocn, binding_fragment, gbu_type, metal_site)
        # prepare the geometry of the CBU
        cbu_iri = cls.init_instance_iri()
        cbu_xyz_file = f"{cbu_iri.split('/')[-1]}.xyz"
        cbu_geo = ontospecies.Geometry.from_points(atom_points, cbu_xyz_file)

        # instantiate actual CBU
        return cls(
            instance_iri=cbu_iri,
            # TODO hasBindingDirection should be modified once side-binding is implemented
            hasBindingDirection='https://www.theworldavatar.com/kg/ontomops/DirectBinding_f3716525-0a8d-430f-ae24-0a043ec0c93a',
            hasBindingSite=binding_sides,
            isFunctioningAs=gbu if gbu is not None else set(),
            hasCharge=ontospecies.Charge(hasValue=om.Measure(hasNumericalValue=charge, hasUnit=om.elementaryCharge)),
            hasMolecularWeight=ontospecies.MolecularWeight.from_xyz_file(cbu_xyz_file),
            hasGeometry=cbu_geo,
            hasCBUFormula=cbu_formula,
            hasCBUAssemblyCenter=assemb_center
        )

    @property
    def vector_to_binding_site_plane(self):
        # get the center of the CBU
        center = list(self.hasCBUAssemblyCenter)[0].coordinates
        # get the line or plane of the binding sites
        binding_sites: List[BindingSite] = self.active_binding_sites
        if len(binding_sites) < 3:
            line = Line.from_two_points(start=binding_sites[0].binding_coordinates, end=binding_sites[1].binding_coordinates)
            if line.is_point_on_line(center):
                # if the center point is on the line then we approximate the plane fitted by the closest 2 atoms of CBU
                # to each binding site and we take the normal vector of the plane
                lst_atoms = []
                for bs in binding_sites:
                    lst_atoms.extend(bs.binding_coordinates.rank_distanct_to_points(list(self.hasGeometry)[0].hasPoints)[:2])
                plane = Plane.fit_from_points(lst_atoms)
                v = plane.normal_vector_from_point_to_plane(center)
            else:
                v = line.normal_vector_from_point_to_line(center)
        else:
            plane = Plane.fit_from_points([bs.binding_coordinates for bs in binding_sites])
            v = plane.normal_vector_from_point_to_plane(center)
        return v

    @property
    def vector_to_farthest_binding_site(self):
        # NOTE this needs to be after the rotation
        # find the plane perpendicular to the vector to the average connecting point
        center = list(self.hasCBUAssemblyCenter)[0].coordinates
        plane = Plane.from_point_and_normal(center, self.vector_to_binding_site_plane)
        # project all connecting points onto the plane
        projected_points = [plane.project_point(bs.binding_coordinates) for bs in self.active_binding_sites]
        # find the farthest binding site and construct a vector connecting the center to it
        farthest_projected_point = center.farthest_point(projected_points)
        vector = Vector.from_two_points(start=center, end=farthest_projected_point)
        return vector

    @property
    def vector_to_shortest_side(self):
        # NOTE this needs to be after the rotation
        # find the plane perpendicular to the vector to the average connecting point
        center = list(self.hasCBUAssemblyCenter)[0].coordinates
        plane = Plane.from_point_and_normal(center, self.vector_to_binding_site_plane)
        # project all connecting points onto the plane
        projected_points = [plane.project_point(bs.binding_coordinates) for bs in self.active_binding_sites]
        # find the closest pair of binding sites and construct a vector connecting the center to the line connecting them
        closest_pair = Point.closest_pair(projected_points)
        line = Line.from_two_points(start=closest_pair[0], end=closest_pair[1])
        v = line.normal_vector_from_point_to_line(center)
        return v

    def vector_of_most_possible_binding_site(self, gbu_plane: Plane, rotation_matrices: List[RotationMatrix] = [RotationMatrix.identity()]):
        # find the binding site that is most likely to bind with the other GBU
        # this is the one that has the smallest angle to the plane of the two GBU coordinate centers
        center: Point = list(self.hasCBUAssemblyCenter)[0].coordinates
        # rotate the binding sites with the rotation matrix
        most_possible_binding_site_angle = 90 # in degrees
        rotated_binding_vector = None
        length_center_to_binding_site = None
        for bs in self.active_binding_sites:
            bs: BindingSite
            v = Vector.from_two_points(start=center, end=bs.binding_coordinates)
            # rotate the vector to align with the GBU coordinate center
            for r in rotation_matrices:
                v = Vector.from_array(r.apply(v.as_array))
            angle = abs(90 - v.get_deg_angle_to(gbu_plane.normal))
            if angle < most_possible_binding_site_angle:
                rotated_binding_vector = v
                most_possible_binding_site_angle = angle
                length_center_to_binding_site = center.get_distance_to(bs.binding_coordinates)
        return rotated_binding_vector, most_possible_binding_site_angle, length_center_to_binding_site

    def visualise(self, sparql_client = None):
        rows = []
        if list(self.hasGeometry)[0].hasPoints is None:
            if sparql_client is None:
                raise ValueError('SPARQL client is required to visualise/load the geometry')
            self.load_geometry_from_fileserver(sparql_client)
        # atoms
        for pt in list(self.hasGeometry)[0].hasPoints:
            rows.append([pt.label, pt.x, pt.y, pt.z])
        # binding sites
        for pt in list(self.hasBindingSite):
            rows.append(['BindingSite', pt.binding_coordinates.x, pt.binding_coordinates.y, pt.binding_coordinates.z])
        # assembly center
        rows.append(['AssemblyCenter', self.assembly_center.x, self.assembly_center.y, self.assembly_center.z])
        df = pd.DataFrame(rows, columns=['Atom', 'X', 'Y', 'Z',])
        fig = px.scatter_3d(df, x='X', y='Y', z='Z', color='Atom', title=f'CBU: {list(self.hasCBUFormula)[0]}')
        fig.update_traces(marker=dict(size=2))
        fig.update_layout(autosize=False, width=1200, height=400)
        fig.show()
        return fig


class MetalOrganicPolyhedron(CoordinationCage):
    hasAssemblyModel: HasAssemblyModel[AssemblyModel]
    hasCavity: Optional[HasCavity[Cavity]] = None
    hasChemicalBuildingUnit: HasChemicalBuildingUnit[ChemicalBuildingUnit]
    hasProvenance: HasProvenance[Provenance]
    hasCharge: ontospecies.HasCharge[ontospecies.Charge]
    hasMolecularWeight: ontospecies.HasMolecularWeight[ontospecies.MolecularWeight]
    hasGeometry: Optional[ontospecies.HasGeometry[ontospecies.Geometry]] = None
    hasCCDCNumber: Optional[HasCCDCNumber[str]] = None
    hasMOPFormula: HasMOPFormula[str]

    @classmethod
    def from_assemble(
        cls,
        am: AssemblyModel,
        lst_cbu: List[ChemicalBuildingUnit],
        prov: Provenance = set(),
        ccdc: str = set(),
        sparql_client = None,
        upload_geometry: bool = False,
    ):
        # prepare the variables
        mop_charge = 0
        mop_mw = 0
        mop_formula = ''

        # locate the GBUs to build the AM
        gbus = list(am.hasGenericBuildingUnit)
        # place the CBUs according to the GBUs
        cbu_rotation_matrix = {}
        for cbu in lst_cbu:
            cbu: ChemicalBuildingUnit
            if list(cbu.hasGeometry)[0].hasPoints is None:
                if sparql_client is None:
                    raise ValueError('SPARQL client is required to visualise/load the geometry')
                list(cbu.hasGeometry)[0].load_xyz_from_geometry_file(sparql_client)
            gbu = cbu.isFunctioningAs.intersection(gbus)
            if len(gbu) == 0:
                raise ValueError(f'No GBU found for CBU {cbu.instance_iri} in AM {am.instance_iri}')
            elif len(gbu) > 1:
                raise ValueError(f'Multiple GBUs found for CBU {cbu.instance_iri} in AM {am.instance_iri}: {gbu}')
            else:
                gbu = gbu.pop()
            if type(gbu) is not GenericBuildingUnit:
                gbu: GenericBuildingUnit = KnowledgeGraph.get_object_from_lookup(gbu)
            # NOTE here we need to block the binding sites based on the GBU
            cbu.allocate_active_binding_sites(gbu.modularity)
            # TODO optimise below
            # rotate the CBU to match the GBU
            # rotate the vector from center to binding site plane of CBU to the vector from center to connecting point plane of GBU
            rotation_matrix_1 = {
                gbu_center.instance_iri: cbu.vector_to_binding_site_plane.get_rotation_matrix_to_parallel(
                    gbu_center.vector_to_connecting_point_plane, flip_if_180=True) for gbu_center in gbu.hasGBUCoordinateCenter
            }
            # rotate the normal vector of the line connecting the farthest pair of binding sites of CBU to the same vector of GBU
            # NOTE that here we are getting the rotation matrix for the vector that is already rotated
            rotation_matrix_2 = {}
            for gbu_center in gbu.hasGBUCoordinateCenter:
                gbu_center: GBUCoordinateCenter
                if gbu.is_4_planar:
                    second_vector_for_alignment_cbu = cbu.vector_to_shortest_side
                    second_vector_for_alignment_gbu = gbu_center.vector_to_shortest_side
                else:
                    second_vector_for_alignment_cbu = cbu.vector_to_farthest_binding_site
                    second_vector_for_alignment_gbu = gbu_center.vector_to_farthest_connecting_point
                rotated = rotation_matrix_1[gbu_center.instance_iri].apply(second_vector_for_alignment_cbu.as_array)
                rotated_cbu_center_to_binding_site_plane = Vector.from_array(rotation_matrix_1[gbu_center.instance_iri].apply(cbu.vector_to_binding_site_plane.as_array))
                rotation_matrix_2[gbu_center.instance_iri] = Vector.from_array(rotated).get_rotation_matrix_to_parallel(
                    second_vector_for_alignment_gbu, flip_if_180=True, base_axis_if_180=rotated_cbu_center_to_binding_site_plane)
            # put the two rotation matrix together
            cbu_rotation_matrix[cbu.instance_iri] = {
                gbu_center.instance_iri: [
                    rotation_matrix_1[gbu_center.instance_iri], rotation_matrix_2[gbu_center.instance_iri]
                ] for gbu_center in gbu.hasGBUCoordinateCenter
            }
            # calculate the charge and molecular weight of the MOP
            mop_charge += cbu.charge * len(gbu.hasGBUCoordinateCenter)
            mop_mw += cbu.molecular_weight * len(gbu.hasGBUCoordinateCenter)
            # prepare the mop_formula
            mop_formula += f'{cbu.cbu_formula}{len(gbu.hasGBUCoordinateCenter)}'

        # find any connecting point and its two ends of GBUs
        pair_gbu_center = list(am.pairs_of_connected_gbus.values())[0]
        gc1: GBUCoordinateCenter = pair_gbu_center[0]
        gc2: GBUCoordinateCenter = pair_gbu_center[1]
        v_gbu1: Vector = gc1.vector_from_am_center
        v_gbu2: Vector = gc2.vector_from_am_center
        # calculate the degree between the two GBUs vectors and the plane these two vectors form
        theta_rad = v_gbu1.get_rad_angle_to(v_gbu2)
        plane = Plane.from_two_vectors(v_gbu1, v_gbu2)
        # get the projection of the rotated CBU vectors (from coordinate center to binding site) onto the plane
        for cbu_iri, rm_to_gbu in cbu_rotation_matrix.items():
            # TODO optimise the below
            cbu = KnowledgeGraph.get_object_from_lookup(cbu_iri)
            if gc1.instance_iri in rm_to_gbu:
                # get the rotation matrix for the CBU, for the first GBU
                rm = rm_to_gbu[gc1.instance_iri]
                rotated_binding_vector_cbu1, vector_plane_angle_cbu1, side_length_cbu1 = cbu.vector_of_most_possible_binding_site(plane, rm)
                # NOTE here we are adjusting the side length to account for the half bond length
                # although some of the binding site in the initial data already has additional length
                # i.e. the dummy atom already away from the actually binding atoms
                # we still use the half bond length to adjust the side length
                # as it's generally easier to optimise the geometry if the molecules are too far compared to overlapping (see SI of 10.1021/jp507643v)
                # NOTE we only add half bond length to the organic site as the metal site would be added already
                adjusted_side_length_cbu1 = side_length_cbu1 + HALF_BOND_LENGTH if not cbu.is_metal_cbu else side_length_cbu1
                projected_adjusted_side_length_cbu1 = adjusted_side_length_cbu1 * np.cos(np.deg2rad(vector_plane_angle_cbu1))
                projected_binding_vector_cbu1 = plane.get_projected_vector(rotated_binding_vector_cbu1)
                _gbu_projected_cbu_angle_cbu1 = projected_binding_vector_cbu1.get_rad_angle_to(v_gbu1)
                # NOTE the projected angle is the outer angle so we need to subtract it from pi
                gbu_projected_cbu_angle_cbu1 = np.pi - _gbu_projected_cbu_angle_cbu1
                l_vertical_cbu1 = projected_adjusted_side_length_cbu1 * np.sin(gbu_projected_cbu_angle_cbu1)
                distance_to_am_center_gbu1 = gc1.distance_to_am_center
            elif gc2.instance_iri in rm_to_gbu:
                # get the rotation matrix for the CBU, for the second GBU
                rm = rm_to_gbu[gc2.instance_iri]
                rotated_binding_vector_cbu2, vector_plane_angle_cbu2, side_length_cbu2 = cbu.vector_of_most_possible_binding_site(plane, rm)
                # NOTE here we are adjusting the side length to account for the half bond length
                # although some of the binding site in the initial data already has additional length
                # i.e. the dummy atom already away from the actually binding atoms
                # we still use the half bond length to adjust the side length
                # as it's generally easier to optimise the geometry if the molecules are too far compared to overlapping (see SI of 10.1021/jp507643v)
                # NOTE we only add half bond length to the organic site as the metal site would be added already
                adjusted_side_length_cbu2 = side_length_cbu2 + HALF_BOND_LENGTH if not cbu.is_metal_cbu else side_length_cbu2
                projected_adjusted_side_length_cbu2 = adjusted_side_length_cbu2 * np.cos(np.deg2rad(vector_plane_angle_cbu2))
                projected_binding_vector_cbu2 = plane.get_projected_vector(rotated_binding_vector_cbu2)
                _gbu_projected_cbu_angle_cbu2 = projected_binding_vector_cbu2.get_rad_angle_to(v_gbu2)
                # NOTE the projected angle is the outer angle so we need to subtract it from pi
                gbu_projected_cbu_angle_cbu2 = np.pi - _gbu_projected_cbu_angle_cbu2
                l_vertical_cbu2 = projected_adjusted_side_length_cbu2 * np.sin(gbu_projected_cbu_angle_cbu2)
                distance_to_am_center_gbu2 = gc2.distance_to_am_center
            else:
                raise ValueError(f'No rotation matrix found for CBU {cbu.instance_iri} in AM {am.instance_iri}')
        # equation to solve: sin(omega)/l_vertical_cbu1 = sin(theta-omega)/l_vertical_cbu2
        initial_guess = theta_rad / 2
        omega = fsolve(lambda x: np.sin(x)/l_vertical_cbu1 - np.sin(theta_rad - x)/l_vertical_cbu2, initial_guess)
        shared_side = l_vertical_cbu1 / np.sin(omega)
        scaled_cbu1 = math.sqrt(projected_adjusted_side_length_cbu1**2 + shared_side**2 - 2*projected_adjusted_side_length_cbu1*shared_side*np.cos(np.pi - gbu_projected_cbu_angle_cbu1 - omega))
        scaled_cbu2 = math.sqrt(projected_adjusted_side_length_cbu2**2 + shared_side**2 - 2*projected_adjusted_side_length_cbu2*shared_side*np.cos(np.pi - gbu_projected_cbu_angle_cbu2 - (theta_rad - omega)))
        # calculate the scaling factor for the CBU
        scaling_factor_cbu1 = scaled_cbu1 / distance_to_am_center_gbu1
        scaling_factor_cbu2 = scaled_cbu2 / distance_to_am_center_gbu2

        # apply all rotation to the cbu
        # rotate the cbu to be parallel to the gbu
        cbu_translated = {}
        cbu_translation_vector = {}
        for cbu_iri, rm_to_gbu in cbu_rotation_matrix.items():
            # TODO optimise the below
            cbu = KnowledgeGraph.get_object_from_lookup(cbu_iri)
            if list(cbu.hasGeometry)[0].hasPoints is None:
                if sparql_client is None:
                    raise ValueError('SPARQL client is required to load the geometry')
                cbu.load_geometry_from_fileserver(sparql_client)
            dct_rotated = {
                gc: [Point.from_array(rm_to_gbu[gc][1].apply(rm_to_gbu[gc][0].apply(pt.as_array)), label=pt.label) for pt in list(cbu.hasGeometry)[0].hasPoints] for gc in rm_to_gbu
            }
            # find the translation vector of the cbu center to the gbu center
            # note that the gbu center need to be the scaled version of the gbu center
            dct_translation_vector = {
                gc: Point.from_array(
                        rm_to_gbu[gc][1].apply(rm_to_gbu[gc][0].apply(list(cbu.hasCBUAssemblyCenter)[0].coordinates.as_array))
                    ).get_translation_vector_to(
                        Point.scale(
                            KnowledgeGraph.get_object_from_lookup(gc).coordinates,
                            scaling_factor_cbu1 if gc1.instance_iri in rm_to_gbu else scaling_factor_cbu2
                        )
                    ) for gc in rm_to_gbu
            }
            # translate the cbu to the gbu
            dct_translated = {
                gc: [Point.translate(pt, dct_translation_vector[gc]) for pt in dct_rotated[gc]] for gc in rm_to_gbu
            }
            cbu_translated[cbu_iri] = dct_translated
            cbu_translation_vector[cbu_iri] = dct_translation_vector

        lst_points = []
        for cbu, gcc_pts in cbu_translated.items():
            for gcc, pts in gcc_pts.items():
                lst_points.extend(pts)
        # prepare the geometry file and upload
        mop_iri = cls.init_instance_iri()
        local_file_path = f"./data/xyz_mops_new/{am.rdfs_label}_{list(am.hasSymmetryPointGroup)[0]}___{mop_formula}___{mop_iri.split('/')[-1] if not bool(ccdc) else ccdc}.xyz"
        mop_geo = ontospecies.Geometry.from_points(lst_points, local_file_path)
        if upload_geometry:
            # upload the geometry to the KG
            if sparql_client is None:
                raise ValueError('SPARQL client is required to upload the geometry')
            else:
                remote_file_path, timestamp_upload = sparql_client.upload_file(local_file_path)
                mop_geo.hasGeometryFile = {remote_file_path}

        # release the blocked binding sites
        for cbu in lst_cbu:
            cbu.release_blocked_binding_sites()

        return cls(
            instance_iri=mop_iri,
            hasAssemblyModel=am,
            hasChemicalBuildingUnit=lst_cbu,
            hasProvenance=prov,
            hasCharge=ontospecies.Charge(hasValue=om.Measure(hasNumericalValue=mop_charge, hasUnit=om.elementaryCharge)),
            hasMolecularWeight=ontospecies.MolecularWeight(hasValue=om.Measure(hasNumericalValue=mop_mw, hasUnit=om.gramPerMole)),
            hasMOPFormula=mop_formula,
            hasCCDCNumber=ccdc,
            hasGeometry=mop_geo,
        )

    def visualise(self, sparql_client = None):
        rows = []
        if list(self.hasGeometry)[0].hasPoints is None:
            if sparql_client is None:
                raise ValueError('SPARQL client is required to visualise/load the geometry')
            list(self.hasGeometry)[0].load_xyz_from_geometry_file(sparql_client)
        for pt in list(self.hasGeometry)[0].hasPoints:
            rows.append([pt.label, pt.x, pt.y, pt.z])
        df = pd.DataFrame(rows, columns=['Atom', 'X', 'Y', 'Z',])
        fig = px.scatter_3d(df, x='X', y='Y', z='Z', color='Atom', title=f'MOP: {list(self.hasMOPFormula)[0]}\n AM: {list(self.hasAssemblyModel)[0].instance_iri}')
        fig.update_traces(marker=dict(size=2))
        fig.update_layout(autosize=False, width=1200, height=400)
        fig.show()
        return fig
