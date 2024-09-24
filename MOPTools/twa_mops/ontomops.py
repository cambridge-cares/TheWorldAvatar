from __future__ import annotations
from typing import Optional, Dict, List
from scipy.optimize import fsolve
import numpy as np
import math

from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty, KnowledgeGraph
import ontospecies
import om

from geo import Point, Vector, Line, Plane, RotationMatrix


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
HasCBUCoordinateCenter = ObjectProperty.create_from_base('HasCBUCoordinateCenter', OntoMOPs)
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
    # below are additions for assembler
    hasGBUCoordinateCenter: HasGBUCoordinateCenter[GBUCoordinateCenter]
    hasGBUConnectingPoint: HasGBUConnectingPoint[GBUConnectingPoint]

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

    @property
    def binding_coordinates(self):
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
        # NOTE TODO not sure if this will work for AMs that are not Oh symmetry
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

class CBUCoordinateCenter(CoordinatePoint):
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
    hasCBUCoordinateCenter: HasCBUCoordinateCenter[CBUCoordinateCenter]

    @property
    def vector_to_binding_site_plane(self):
        # get the center of the CBU
        center = list(self.hasCBUCoordinateCenter)[0].coordinates
        # get the line or plane of the binding sites
        binding_sites = list(self.hasBindingSite)
        if len(binding_sites) < 3:
            line = Line.from_two_points(start=binding_sites[0].binding_coordinates, end=binding_sites[1].binding_coordinates)
            if line.is_point_on_line(center):
                # if the center point is on the line then we approximate the plane fitted by the atoms of CBU
                # and we take the normal vector of the plane
                # TODO check if it is safe to make this assumption for all CBUs
                plane = Plane.fit_from_points([pt for pt in list(self.hasGeometry)[0].hasPoints])
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
        center = list(self.hasCBUCoordinateCenter)[0].coordinates
        plane = Plane.from_point_and_normal(center, self.vector_to_binding_site_plane)
        # project all connecting points onto the plane
        projected_points = [plane.project_point(bs.binding_coordinates) for bs in self.hasBindingSite]
        # find the farthest binding site and construct a vector connecting the center to it
        farthest_projected_point = center.farthest_point(projected_points)
        vector = Vector.from_two_points(start=center, end=farthest_projected_point)
        return vector

    def vector_of_most_possible_binding_site(self, gbu_plane: Plane, rotation_matrices: List[RotationMatrix] = [RotationMatrix.identity()]):
        # find the binding site that is most likely to bind with the other GBU
        # this is the one that has the smallest angle to the plane of the two GBU coordinate centers
        center: Point = list(self.hasCBUCoordinateCenter)[0].coordinates
        # rotate the binding sites with the rotation matrix
        most_possible_binding_site_angle = 90 # in degrees
        rotated_binding_vector = None
        length_center_to_binding_site = None
        for bs in self.hasBindingSite:
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

    def assemble(self, half_bond_length):
        # get the assembly model
        am: AssemblyModel = list(self.hasAssemblyModel)[0]
        # locate the GBUs to build the AM
        gbus = list(am.hasGenericBuildingUnit)
        # place the CBUs according to the GBUs
        cbu_rotation_matrix = {}
        for cbu in self.hasChemicalBuildingUnit:
            cbu: ChemicalBuildingUnit
            gbu = cbu.isFunctioningAs.intersection(gbus)
            if len(gbu) == 0:
                raise ValueError(f'No GBU found for CBU {cbu.instance_iri} in AM {am.instance_iri}')
            elif len(gbu) > 1:
                raise ValueError(f'Multiple GBUs found for CBU {cbu.instance_iri} in AM {am.instance_iri}: {gbu}')
            else:
                gbu: GenericBuildingUnit = gbu.pop()
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
                rotated = rotation_matrix_1[gbu_center.instance_iri].apply(cbu.vector_to_farthest_binding_site.as_array)
                rotated_cbu_center_to_binding_site_plane = Vector.from_array(rotation_matrix_1[gbu_center.instance_iri].apply(cbu.vector_to_binding_site_plane.as_array))
                rotation_matrix_2[gbu_center.instance_iri] = Vector.from_array(rotated).get_rotation_matrix_to_parallel(
                    gbu_center.vector_to_farthest_connecting_point, flip_if_180=True, base_axis_if_180=rotated_cbu_center_to_binding_site_plane)
            # put the two rotation matrix together
            cbu_rotation_matrix[cbu.instance_iri] = {
                gbu_center.instance_iri: [
                    rotation_matrix_1[gbu_center.instance_iri], rotation_matrix_2[gbu_center.instance_iri]
                ] for gbu_center in gbu.hasGBUCoordinateCenter
            }
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
                # but we only add half of the half bond length because the binding site in the initial data already has additional length
                # i.e. it's a dummy atom already away from the actually binding atoms
                adjusted_side_length_cbu1 = side_length_cbu1 + half_bond_length / 2
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
                # but we only add half of the half bond length because the binding site in the initial data already has additional length
                # i.e. it's a dummy atom already away from the actually binding atoms
                adjusted_side_length_cbu2 = side_length_cbu2 + half_bond_length / 2
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
        scaled_cbu1 = math.sqrt(l_vertical_cbu1**2 + shared_side**2 - 2*l_vertical_cbu1*shared_side*np.cos(np.pi - gbu_projected_cbu_angle_cbu1 - omega))
        scaled_cbu2 = math.sqrt(l_vertical_cbu2**2 + shared_side**2 - 2*l_vertical_cbu2*shared_side*np.cos(np.pi - gbu_projected_cbu_angle_cbu2 - (theta_rad - omega)))
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
            dct_rotated = {
                gc: [Point.from_array(rm_to_gbu[gc][1].apply(rm_to_gbu[gc][0].apply(pt.as_array)), label=pt.label) for pt in list(cbu.hasGeometry)[0].hasPoints] for gc in rm_to_gbu
            }
            # find the translation vector of the cbu center to the gbu center
            # note that the gbu center need to be the scaled version of the gbu center
            dct_translation_vector = {
                gc: Point.get_translation_vector_to(
                    Point.from_array(rm_to_gbu[gc][1].apply(rm_to_gbu[gc][0].apply(list(cbu.hasCBUCoordinateCenter)[0].coordinates.as_array))),
                    Point.scale(KnowledgeGraph.get_object_from_lookup(gc).coordinates, scaling_factor_cbu1 if gc1.instance_iri in rm_to_gbu else scaling_factor_cbu2)) for gc in rm_to_gbu
            }
            # translate the cbu to the gbu
            dct_translated = {
                gc: [Point.translate(pt, dct_translation_vector[gc]) for pt in dct_rotated[gc]] for gc in rm_to_gbu
            }
            cbu_translated[cbu_iri] = dct_translated
            cbu_translation_vector[cbu_iri] = dct_translation_vector
        return cbu_translated
