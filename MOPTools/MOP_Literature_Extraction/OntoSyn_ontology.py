# Import relevant packages
from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
from twa.data_model.iris import TWA_BASE_URL
from typing import ClassVar
from pydantic import Field
from rdflib.namespace import RDF
from typing import Optional



class OntoSyn(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'OntoSyn'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoMOPs(BaseOntology):
    base_url:           ClassVar[str]           = "https://www.theworldavatar.com/kg/"
    namespace:          ClassVar[str]           = 'ontomops/'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoLab(BaseOntology):
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'OntoLab'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OM2(BaseOntology):
    base_url:           ClassVar[str]           = "http://www.ontology-of-units-of-measure.org/resource/"
    namespace:          ClassVar[str]           = 'om-2'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class BIBO(BaseOntology):
    base_url:           ClassVar[str]           = "http://purl.org/ontology/"
    namespace:          ClassVar[str]           = 'bibo'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoCapeMaterial(BaseOntology):
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontocape/material/"
    namespace:          ClassVar[str]           = 'material.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoCapePhaseSystem(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/"
    namespace:          ClassVar[str]           = 'phase_system.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoCapeSystem(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontocape/upper_level/"
    namespace:          ClassVar[str]           = 'system.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoSpecies(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#"
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class RDFS(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.w3.org/2000/01/"
    namespace:          ClassVar[str]           = 'rdf-schema'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class SKOS(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.w3.org/2004/02/skos/"
    namespace:          ClassVar[str]           = 'core#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class DAML(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.daml.org/2003/01/periodictable/PeriodicTable#"
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
class OntoReaction(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "https://www.theworldavatar.com/kg/ontoreaction/"
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
###-----------------------------------------------------------------------------------------------
# Datatype Properties:
class HasX1(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasX2(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasY(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasX1Axis(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasX2Axis(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasYAxis(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasVacuum(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsSealed(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsRepeated(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class AltLabel(DatatypeProperty):
    rdfs_isDefinedBy                    = SKOS
class HasUnit(ObjectProperty):
    rdfs_isDefinedBy                    = OM2
class HasOrder(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasCCDCNumber(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class HasMOPFormula(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class HasCBUFormula(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class MopAltLabel(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class HasNumericalValue(DatatypeProperty):
    rdfs_isDefinedBy                    = OM2
class Value(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasTargetPh(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsStirred(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsStirredHeatChill(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsVacuumFiltration(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsLayered(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsWait(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasRotaryEvaporator(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsLayeredTransfer(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasPurity(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn


###-----------------------------------------------------------------------------------------------
# Object Properties:
class HasDryingTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasDryingPressure(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasEvaporationPressure(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasEvaporationTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasYield(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasSolvent(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasInstrumentType(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasElementalDevice(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class IsBasedOnMolecularFormula(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasElementWeightPercentage(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasElementalAnalysis(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class IsReferingToElement(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasMassFraction(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class Has1H1HNMR(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasFourierTransformSpectrum(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasPeak(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasSpectraGraph(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSpecies
class HasTemperatureRate(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasTargetTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasCrystallizationTargetTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasWashingSolvent(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasAddedChemicalInput(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasEquipment(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasChemicalInput(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class RepresentsOccurenceOf(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
class HasUnitOfMeasure(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem
class HasProperty(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem
class ComprisesDirectly(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem
class HasComposition(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
class IsComposedOfSubsystem(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem
class ThermodynamicBehaviour(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeMaterial
class ReferencesMaterial(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasValue(ObjectProperty):
    rdfs_isDefinedBy                    = OM2
class HasStepDuration(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasVesselEnvironment(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasVessel(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasSynthesisStep(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsRepresentedBy(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasChemicalOutput(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn  
class IsDescribedBy(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class RetrievedFrom(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasChemicalBuildingUnit(ObjectProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class IsUsedAsChemical(ObjectProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class HasVesselType(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasStirringTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsTransferedTo(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasSolventDissolve(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasSeparationSolvent(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsEvaporatedToVolume(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasDryingAgent(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class RemovesSpecies(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn   
class IsSeparationType(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn  
class HasTransferedAmount(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn  
class HasHeatChillDevice(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn  
class IsSuppliedBy(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
###-----------------------------------------------------------------------------------------------
# Classes:
class Label(DatatypeProperty):
    rdfs_isDefinedBy                    = RDFS
class UnitOfMeasure(BaseClass):
    rdfs_isDefinedBy                    = OM2
class Measure(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                        = set()
    hasUnit                             : Optional[HasUnit[UnitOfMeasure]]                          = set()
class CharacteristicPeak(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    hasX1                               : Optional[HasX1[str]]                          = set()
    hasX2                               : Optional[HasX2[str]]                          = set()
    hasY                                : Optional[HasY[str]]                           = set()
class SpectraGraph(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    hasPeak                             : Optional[HasPeak[CharacteristicPeak]]             = set()
    hasX1Axis                           : Optional[HasX1Axis[str]]                          = set()
    hasX2Axis                           : Optional[HasX2Axis[str]]                          = set()
    hasYAxis                            : Optional[HasYAxis[str]]                           = set()
class InstrumentType(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
class Solvent(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
class SpectralInformation(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    hasSpectraGraph                     : Optional[HasSpectraGraph[SpectraGraph]]           = set()
    hasInstrumentType                   : Optional[HasInstrumentType[InstrumentType]]       = set()
    hasSolvent                          : Optional[HasSolvent[Solvent]]                     = set()
class NMRSpectra(SpectralInformation):
    rdfs_isDefinedBy                    = OntoSpecies
class DNMRSpectra(NMRSpectra):
    rdfs_isDefinedBy                    = OntoSpecies
class HNMRSpectra(DNMRSpectra):
    rdfs_isDefinedBy                    = OntoSpecies
class AbsortionSpectrum(SpectralInformation):
    rdfs_isDefinedBy                    = OntoSpecies
class FourierTransformSpectrum(AbsortionSpectrum):
    rdfs_isDefinedBy                    = OntoSpecies
class Element(BaseClass):
    rdfs_isDefinedBy                    = DAML
class MassFraction(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                       = set()
class ElementWeightPercentage(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    isReferingToElement                 : Optional[IsReferingToElement[Element]]            = set()
    hasMassFraction                     : Optional[HasMassFraction[MassFraction]]           = set()
class ElementalAnalysis(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    hasElementWeightPercentage          : Optional[HasElementWeightPercentage[ElementWeightPercentage]]           = set()
class ExperimentalElementalAnalysis(ElementalAnalysis):
    rdfs_isDefinedBy                    = OntoSpecies
    hasElementalDevice                  : Optional[HasElementalDevice[InstrumentType]]         = set()
class MolecularFormula(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    value                               : Optional[Value[str]]                                          = set()
class AnalyticalElementalAnalysis(ElementalAnalysis):
    rdfs_isDefinedBy                    = OntoSpecies
    isBasedOnMolecularFormula           : Optional[IsBasedOnMolecularFormula[MolecularFormula]]         = set()
class Species(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    altLabel                            : Optional[AltLabel[str]]                                           = set()
    has1H1HNMR                          : Optional[Has1H1HNMR[HNMRSpectra]]                                 = set()
    label                               : Optional[Label[str]]                                              = set()
    hasFourierTransformSpectrum         : Optional[HasFourierTransformSpectrum[FourierTransformSpectrum]]   = set()
    hasElementalAnalysis                : Optional[HasElementalAnalysis[ElementalAnalysis]]                 = set()
class ScalarValue(BaseClass):
    rdfs_isDefinedBy                    = OntoCapeSystem
    hasUnitOfMeasure                    : Optional[HasUnitOfMeasure[UnitOfMeasure]]                 = set()
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                        = set()
class PhaseComponentConcentration(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    hasValue                            : Optional[HasValue[ScalarValue]]                           = set()
class PhaseComponent(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    representsOccurenceOf               : Optional[RepresentsOccurenceOf[Species]]                  = set()
    hasProperty                         : Optional[HasProperty[PhaseComponentConcentration]]        = set()
class Composition(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    comprisesDirectly                   : Optional[ComprisesDirectly[PhaseComponentConcentration]]  = set()
class SinglePhase(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    isComposedOfSubsystem               : Optional[IsComposedOfSubsystem[PhaseComponent]]           = set()
    hasComposition                      : Optional[HasComposition[Composition]]                     = set()
class Material(BaseClass):
    rdfs_isDefinedBy                    = OntoCapeMaterial
    thermodynamicBehaviour              : Optional[ThermodynamicBehaviour[SinglePhase]]             = set()
class Supplier(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
class ChemicalInput(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    isSuppliedBy                        : Optional[IsSuppliedBy[Supplier]]                          = set()
    referencesMaterial                  : Optional[ReferencesMaterial[Material]]                    = set()
    hasPurity                           : Optional[HasPurity[str]]                                  = set()
class Doi(DatatypeProperty):
    rdfs_isDefinedBy                    = BIBO
class Document(BaseClass):
    rdfs_isDefinedBy                    = BIBO
    doi                                 : Optional[Doi[str]]    
class Duration(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                               = set()
class VesselEnvironment(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
class LabEquipment(BaseClass):
    rdfs_isDefinedBy                    = OntoLab
class HeatChillDevice(LabEquipment):
    rdfs_isDefinedBy                    = OntoSyn
class VesselType(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
class Vessel(LabEquipment):
    rdfs_isDefinedBy                    = OntoSyn
    hasVesselType                       : Optional[HasVesselType[VesselType]]                       = set()
class ExecutionPoint(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
class SynthesisStep(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasVessel                           : Optional[HasVessel[Vessel]]                               = set()
    hasOrder                            : Optional[HasOrder[int]]                                   = set() 
    hasVesselEnvironment                : Optional[HasVesselEnvironment[VesselEnvironment]]         = set()
    hasStepDuration                     : Optional[HasStepDuration[Duration]]                       = set()
class AmountOfSubstanceFraction(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                               = set()
class ChemicalSynthesis(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn  
    hasSynthesisStep                    : Optional[HasSynthesisStep[SynthesisStep]]                 = set()
    retrievedFrom                       : Optional[RetrievedFrom[Document]]                         = set()
    hasChemicalInput                    : Optional[HasChemicalInput[ChemicalInput]]                 = set()
    hasYield                            : Optional[HasYield[AmountOfSubstanceFraction]]             = set()
    # not ideal but mostly unused:
    hasEquipment                        : Optional[HasEquipment[LabEquipment]]                      = set()
class ChemicalBuildingUnit(BaseClass):
    rdfs_isDefinedBy                    = OntoMOPs
    hasCBUFormula                       : Optional[HasCBUFormula[str]]                              = set()
    # is linked with input chemical on the miro board
    isUsedAsChemical                    : Optional[IsUsedAsChemical[Species]]                       = set()
class MetalOrganicPolyhedron(BaseClass):
    rdfs_isDefinedBy                    = OntoMOPs
    hasCCDCNumber                       : Optional[HasCCDCNumber[str]]                              = set()
    hasMOPFormula                       : Optional[HasMOPFormula[str]]                              = set()
    hasChemicalBuildingUnit             : Optional[HasChemicalBuildingUnit[ChemicalBuildingUnit]]   = set()
    altLabel                            : Optional[AltLabel[str]]                                   = set()

class ChemicalOutput(Species):
    rdfs_isDefinedBy                    = OntoSyn
    isRepresentedBy                     : Optional[IsRepresentedBy[MetalOrganicPolyhedron]]         = set()

class ChemicalTransformation(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    isDescribedBy                       : Optional[IsDescribedBy[ChemicalSynthesis]]                = set()
    hasChemicalOutput                   : Optional[HasChemicalOutput[ChemicalOutput]]               = set()

class Add(SynthesisStep):       
    rdfs_isDefinedBy                    = OntoSyn
    hasAddedChemicalInput               : Optional[HasAddedChemicalInput[ChemicalInput]]            = set()
    hasTargetPh                         : Optional[HasTargetPh[float]]                              = set()
    isStirred                           : Optional[IsStirred[bool]]                                 = set()
    isLayered                           : Optional[IsLayered[bool]]                                 = set()
class Filter(SynthesisStep):        
    rdfs_isDefinedBy                    = OntoSyn
    hasWashingSolvent                   : Optional[HasWashingSolvent[ChemicalInput]]                = set()
    isRepeated                          : Optional[IsRepeated[int]]                                 = set()
    isVacuumFiltration                  : Optional[IsVacuumFiltration[bool]]                        = set()
class Sonicate(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
class TemperatureUnit(BaseClass):
    rdfs_isDefinedBy                    = OM2
class Temperature(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                               = set()
class TemperatureChangeRateUnit(BaseClass):
    rdfs_isDefinedBy                    = OM2
class TemperatureRate(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                               = set()
class Crystallize(SynthesisStep):       
    rdfs_isDefinedBy                    = OntoSyn
    hasCrystallizationTargetTemperature : Optional[HasCrystallizationTargetTemperature[Temperature]]    = set()
class Stir(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    # not the way it is supposed to be:
    hasStirringTemperature              : Optional[HasStirringTemperature[Temperature]]                 = set()
    isWait                              : Optional[IsWait[bool]]                                        = set()
class Pressure(BaseClass):       
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                                   = set()
class Dry(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasDryingPressure                   : Optional[HasDryingPressure[Pressure]]                         = set()
    hasDryingTemperature                : Optional[HasDryingTemperature[Temperature]]                   = set()
    hasDryingAgent                      : Optional[HasDryingAgent[ChemicalInput]]                       = set()
class Volume(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                                   = set()
class Evaporate(SynthesisStep):       
    rdfs_isDefinedBy                    = OntoSyn
    hasEvaporationTemperature           : Optional[HasEvaporationTemperature[Temperature]]              = set()
    hasEvaporationPressure              : Optional[HasEvaporationPressure[Pressure]]                    = set()
    removesSpecies                      : Optional[RemovesSpecies[ChemicalInput]]                       = set()
    isEvaporatedToVolume                : Optional[IsEvaporatedToVolume[Volume]]                        = set()
    hasRotaryEvaporator                 : Optional[HasRotaryEvaporator[bool]]                           = set()
class Dissolve(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasSolventDissolve                  : Optional[HasSolventDissolve[ChemicalInput]]                   = set()
class Transfer(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    isTransferedTo                      : Optional[IsTransferedTo[Vessel]]                              = set()
    isLayeredTransfer                   : Optional[IsLayeredTransfer[bool]]                             = set()
    hasTransferedAmount                 : Optional[HasTransferedAmount[Volume]]                         = set()
class SeparationType(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
class Separate(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasSeparationSolvent                : Optional[HasSeparationSolvent[ChemicalInput]]                 = set() # has RemovedSpecies
    isSeparationType                    : Optional[IsSeparationType[SeparationType]]                    = set()
class HeatChill(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasTargetTemperature                : Optional[HasTargetTemperature[Temperature]]                   = set()
    hasTemperatureRate                  : Optional[HasTemperatureRate[TemperatureRate]]                 = set()
    hasVacuum                           : Optional[HasVacuum[bool]]                                     = set()
    isSealed                            : Optional[IsSealed[bool]]                                      = set()
    isStirredHeatChill                  : Optional[IsStirredHeatChill[bool]]                            = set()
    hasHeatChillDevice                  : Optional[HasHeatChillDevice[HeatChillDevice]]                 = set()

if __name__ == "__main__":
    OntoSyn.export_to_owl('OntoSyn.ttl', format='turtle')
