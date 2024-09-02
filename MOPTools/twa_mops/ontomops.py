from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
import ontospecies
import om
from typing import Optional


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
HasGeometry = ObjectProperty.create_from_base('HasGeometry', OntoMOPs)


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


# classes
class MolecularCage(BaseClass):
    rdfs_isDefinedBy = OntoMOPs

class CoordinationCage(MolecularCage):
    pass

class GenericBuildingUnit(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasModularity: HasModularity[int]
    hasPlanarity: HasPlanarity[str]

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

class BindingSite(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasOuterCoordinationNumber: HasOuterCoordinationNumber[int]

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

class ChemicalBuildingUnit(BaseClass):
    rdfs_isDefinedBy = OntoMOPs
    hasBindingDirection: HasBindingDirection[BindingDirection]
    hasBindingSite: HasBindingSite[BindingSite]
    isFunctioningAs: IsFunctioningAs[GenericBuildingUnit]
    hasCharge: ontospecies.HasCharge[ontospecies.Charge]
    hasMolecularWeight: ontospecies.HasMolecularWeight[ontospecies.MolecularWeight]
    hasGeometry: HasGeometry[ontospecies.Geometry]
    hasCBUFormula: HasCBUFormula[str]

class MetalOrganicPolyhedron(CoordinationCage):
    hasAssemblyModel: HasAssemblyModel[AssemblyModel]
    hasCavity: Optional[HasCavity[Cavity]] = None
    hasChemicalBuildingUnit: HasChemicalBuildingUnit[ChemicalBuildingUnit]
    hasProvenance: HasProvenance[Provenance]
    hasCharge: ontospecies.HasCharge[ontospecies.Charge]
    hasMolecularWeight: ontospecies.HasMolecularWeight[ontospecies.MolecularWeight]
    hasGeometry: Optional[HasGeometry[ontospecies.Geometry]] = None
    hasCCDCNumber: Optional[HasCCDCNumber[str]] = None
    hasMOPFormula: HasMOPFormula[str]
