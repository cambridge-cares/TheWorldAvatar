# ontocof_ontology.py

from __future__ import annotations
from typing import ClassVar, Set
from typing import Optional
from twa.data_model.base_ontology import (
    BaseOntology,
    BaseClass,
    ObjectProperty,
    DatatypeProperty,
)

# Define the ontology
class OntoCOF(BaseOntology):
    base_url: ClassVar[str] = "https://www.theworldavatar.com/kg/ontocof/"
    #namespace: ClassVar[str] = "ontocof"
    owl_versionInfo: ClassVar[str] = "0.0.1"
    rdfs_comment: ClassVar[set[str]] = {"Ontology for COF framework constructs"}

# Define properties related to FrameworkConstruct
HasAssemblyModel = ObjectProperty.create_from_base("HasAssemblyModel", OntoCOF)
HasLinkage = ObjectProperty.create_from_base("HasLinkage", OntoCOF)
HasPrecursor = ObjectProperty.create_from_base("HasPrecursor", OntoCOF)
HasCrystalOccurrence = ObjectProperty.create_from_base("HasCrystalOccurrence", OntoCOF)
HasNumberOfPrecursors = DatatypeProperty.create_from_base("HasNumberOfPrecursors", OntoCOF)

# Define properties related to AssemblyModel
HasGenericBuildingUnit = ObjectProperty.create_from_base("HasGenericBuildingUnit", OntoCOF)
HasLinkageGenericBuildingUnit = ObjectProperty.create_from_base("HasLinkageGenericBuildingUnit", OntoCOF)
HasGBUNumber = ObjectProperty.create_from_base("HasGBUNumber", OntoCOF)
HasLGBUNumber = ObjectProperty.create_from_base("HasLGBUNumber", OntoCOF)
HasAssemblyModelLabel = DatatypeProperty.create_from_base("HasAssemblyModelLabel", OntoCOF)
HasAssemblyModelShortFormula = DatatypeProperty.create_from_base("HasAssemblyModelShortFormula", OntoCOF)
HasAssemblyModelFormula = DatatypeProperty.create_from_base("HasAssemblyModelFormula", OntoCOF)
HasRCSRReference = DatatypeProperty.create_from_base("HasRCSRReference", OntoCOF)
HasFrameworkName = DatatypeProperty.create_from_base("HasFrameworkName", OntoCOF)
HasPeriodicity = DatatypeProperty.create_from_base("HasPeriodicity", OntoCOF)
HasFrameworkNotation = DatatypeProperty.create_from_base("HasFrameworkNotation", OntoCOF)

# Define properties related to GBUNumber and LGBUNumber
IsNumberOfGBU = ObjectProperty.create_from_base("IsNumberOfGBU", OntoCOF)
IsNumberOfLGBU = ObjectProperty.create_from_base("IsNumberOfLGBU", OntoCOF)
HasValue = DatatypeProperty.create_from_base("HasValue", OntoCOF)

# Define properties related to the Linkage 
InvolvesBindingSite = ObjectProperty.create_from_base("InvolvesBindingSite", OntoCOF)
InvolvesCondensateSpecies = ObjectProperty.create_from_base("InvolvesCondensateSpecies", OntoCOF)
HasStochiometricCoefficient = ObjectProperty.create_from_base("HasStochiometricCoefficient", OntoCOF)
FunctionsAsLGBU = ObjectProperty.create_from_base("FunctionsAsLGBU", OntoCOF)
IsValueOfBindingSite = ObjectProperty.create_from_base("IsValueOfBindingSite", OntoCOF)
IsValueOfCondensateSpecies = ObjectProperty.create_from_base("IsValueOfCondensateSpecies", OntoCOF)
HasReactionName = DatatypeProperty.create_from_base("HasReactionName", OntoCOF)
HasReactionNote = DatatypeProperty.create_from_base("HasReactionNote", OntoCOF)
HasLinkageLabel = DatatypeProperty.create_from_base("HasLinkageLabel", OntoCOF)
HasLinkageFormula = DatatypeProperty.create_from_base("HasLinkageFormula", OntoCOF)
HasFormalCharge = DatatypeProperty.create_from_base("HasFormalCharge", OntoCOF)
HasLinkageName = DatatypeProperty.create_from_base("HasLinkageName", OntoCOF)
HasChemicalFormula = DatatypeProperty.create_from_base("HasChemicalFormula", OntoCOF)
HasChemicalName = DatatypeProperty.create_from_base("HasChemicalName", OntoCOF)

# Define additional properties
HasGBULabel = DatatypeProperty.create_from_base("HasGBULabel", OntoCOF)
HasLGBULabel = DatatypeProperty.create_from_base("HasLGBULabel", OntoCOF)
HasModularity = DatatypeProperty.create_from_base("HasModularity", OntoCOF)
HasPlanarity = DatatypeProperty.create_from_base("HasPlanarity", OntoCOF)

FunctionsAsGBU = ObjectProperty.create_from_base("FunctionsAsGBU", OntoCOF)
HasBindingSite = ObjectProperty.create_from_base("HasBindingSite", OntoCOF)
HasPrecursorLabel = DatatypeProperty.create_from_base("HasPrecursorLabel", OntoCOF)
HasCoreMolFile = DatatypeProperty.create_from_base("HasCoreMolFile", OntoCOF)
HasCoreInpFile = DatatypeProperty.create_from_base("HasCoreInpFile", OntoCOF)

HasBindingSiteLabel = DatatypeProperty.create_from_base("HasBindingSiteLabel", OntoCOF)
HasBindingSiteName = DatatypeProperty.create_from_base("HasBindingSiteName", OntoCOF)
HasBindingSiteIndex = DatatypeProperty.create_from_base("HasBindingSiteIndex", OntoCOF)
HasDentation = DatatypeProperty.create_from_base("HasDentation", OntoCOF)
HasDummyLabel = DatatypeProperty.create_from_base("HasDummyLabel", OntoCOF)
HasBindingSiteComplementaryDummy = DatatypeProperty.create_from_base("HasBindingSiteComplementaryDummy", OntoCOF)

HasDOI = DatatypeProperty.create_from_base("HasDOI", OntoCOF)
HasCrystalName = DatatypeProperty.create_from_base("HasCrystalName", OntoCOF)
HasInterlayerStacking = DatatypeProperty.create_from_base("HasInterlayerStacking", OntoCOF)

class BindingSite(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasBindingSiteLabel: HasBindingSiteLabel[str]
    hasBindingSiteName: Optional[HasBindingSiteName[str]] = set()
    hasBindingSiteIndex: Optional[HasBindingSiteIndex[str]] = set()
    hasDentation: Optional[HasDentation[str]] = set()
    hasDummyLabel: Optional[HasDummyLabel[str]] = set()
    hasBindingSiteComplementaryDummy: Optional[HasBindingSiteComplementaryDummy[str]] = set()

class CondensateSpecies(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasChemicalFormula: Optional[HasChemicalFormula[str]] = set()
    hasChemicalName: Optional[HasChemicalName[str]] = set()

class StochiometricCoefficient(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasValue: Optional[HasValue[int]] = set()
    isValueOfBindingSite: Optional[IsValueOfBindingSite[BindingSite]] = set()
    isValueOfCondensateSpecies: Optional[IsValueOfCondensateSpecies[CondensateSpecies]] = set()

class GenericBuildingUnit(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasGBULabel: HasGBULabel[str]
    hasModularity: HasModularity[int]
    hasPlanarity: HasPlanarity[str]

class LinkageGenericBuildingUnit(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasLGBULabel: HasLGBULabel[str]
    hasModularity: HasModularity[int]
    hasPlanarity: HasPlanarity[str]

class GBUNumber(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    isNumberOfGBU: IsNumberOfGBU[GenericBuildingUnit]
    hasValue: HasValue[int]

class LGBUNumber(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    isNumberOfLGBU: IsNumberOfLGBU[LinkageGenericBuildingUnit]
    hasValue: HasValue[int]

class AssemblyModel(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasGenericBuildingUnit: HasGenericBuildingUnit[GenericBuildingUnit]
    hasLinkageGenericBuildingUnit: HasLinkageGenericBuildingUnit[LinkageGenericBuildingUnit]
    hasGBUNumber: HasGBUNumber[GBUNumber]
    hasLGBUNumber: HasLGBUNumber[LGBUNumber]
    hasAssemblyModelLabel: HasAssemblyModelLabel[str]
    hasAssemblyModelShortFormula: HasAssemblyModelShortFormula[str]
    hasAssemblyModelFormula: HasAssemblyModelFormula[str]
    hasRCSRReference: Optional[HasRCSRReference[str]] = set()
    hasFrameworkName: HasFrameworkName[str]
    hasPeriodicity: HasPeriodicity[str]
    hasFrameworkNotation: HasFrameworkNotation[str]

class CrystalStructure(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasDOI: Optional [HasDOI[str]] = set()
    hasCrystalName: Optional [HasCrystalName[str]] = set()
    hasInterlayerStacking: Optional [HasInterlayerStacking[str]] = set()

class Precursor(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasPrecursorLabel: HasPrecursorLabel[str]
    functionsAsGBU: FunctionsAsGBU[GenericBuildingUnit]
    hasCoreMolFile: HasCoreMolFile[str]
    hasCoreInpFile: HasCoreInpFile[str]
    hasBindingSite: HasBindingSite[BindingSite]

class Linkage(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    involvesBindingSite: InvolvesBindingSite[BindingSite]
    involvesCondensateSpecies: InvolvesCondensateSpecies[CondensateSpecies]
    functionsAsLGBU: FunctionsAsLGBU[LinkageGenericBuildingUnit]
    hasReactionName: HasReactionName[str]
    hasReactionNote: HasReactionNote[str]
    hasLinkageLabel: HasLinkageLabel[str]
    hasLinkageFormula: HasLinkageFormula[str]
    hasFormalCharge: HasFormalCharge[int]
    hasLinkageName: HasLinkageName[str]
    hasStochiometricCoefficient: HasStochiometricCoefficient[StochiometricCoefficient]

class FrameworkConstruct(BaseClass):
    rdfs_isDefinedBy = OntoCOF
    hasNumberOfPrecursors: Optional[HasNumberOfPrecursors[int]] = set()
    hasAssemblyModel: HasAssemblyModel[AssemblyModel]
    hasLinkage: HasLinkage[Linkage]
    hasPrecursor: HasPrecursor[Precursor]
    hasCrystalOccurrence: HasCrystalOccurrence[CrystalStructure]
