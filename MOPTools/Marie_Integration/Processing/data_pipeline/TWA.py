# Import relevant packages
from __future__                     import annotations
from twa.data_model.base_ontology   import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty, as_range
from twa.data_model.iris            import TWA_BASE_URL
from typing                         import ClassVar
from pydantic                       import Field


# Your ontology needs to inherit the BaseOntology class
class OntoSyn(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'OntoSyn'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'


# Other Ontologies
class OntoSyn(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'OntoSyn'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

    ###-----------------------------------------------------------------------------------------------
    # Classes:

class ChemicalSynthesis(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasSynthesisStep            : SynthesisStep
    hasFirstStep                : SynthesisStep
    hasEquipment                : LabEquipment
    retrievedFrom               : Document
    hasCharacterisation         : Characterisation
    hasProduct                  : SynthesisProduct

class SynthesisStep(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasContainerVessel          : Vessel

class SynthesisProduct(BaseClass):
    is_defined_by_ontology      = OntoSyn
    isRepresentedBy             : MetalOrganicPolyhedron
    hasSynthesisYield           : SynthesisYield

class SynthesisYield(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasYieldMass                : Mass

class SynthesisReactant(BaseClass):
    is_defined_by_ontology      = OntoSyn

class Add(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasAddedReactant            : SynthesisReactant
    hasAddedSolvent             : Solvent
    isAdded                     : MaterialAmount

class Filter(BaseClass):
    is_defined_by_ontology      = OntoSyn
    isWashedWith                : MaterialAmount
    hasWashingSolvent           : Solvent
    isRepeated                  : int

class Dry(BaseClass):
    is_defined_by_ontology      = OntoSyn

class Stir(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasStirringDuration         : Duration

class Sonication(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasSonicationDuration       : Duration

class HeatChill(BaseClass):
    is_defined_by_ontology      = OntoSyn
    hasHeatChillDuration        : Duration
    hasReactionTemperature      : Temperature
    hasHeatChillRate            : TemperatureRate
    hasHeatChillDevice          : HeatChillDevice
    hasVacuum                   : bool
    isSealed                    : bool

class HeatChillDevice(BaseClass):
    is_defined_by_ontology      = OntoSyn

class LabEquipment(BaseClass):
    is_defined_by_ontology      = OntoSyn

    ###-----------------------------------------------------------------------------------------------
    # Datatype Properties:

class hasNitrogenAthmosphere(DatatypeProperty):
    # Same as ObjectProperty, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology      = OntoSyn
    range                       : as_range(bool)

class hasVacuum(DatatypeProperty):
    # Same as ObjectProperty, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology      = OntoSyn
    range                       : as_range(bool)

class isSealed(DatatypeProperty):
    # Same as ObjectProperty, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology      = OntoSyn
    range                       : as_range(bool)

class isRepeated(DatatypeProperty):
    # Same as ObjectProperty, `is_defined_by_ontology` is a compulsory field
    is_defined_by_ontology      = OntoSyn
    range                       : as_range(bool)


    ###-----------------------------------------------------------------------------------------------
    # Object Properties:

class hasSynthesisStep(ObjectProperty):
    # The user MUST provide the ontology for which the concept `is_defined_by_ontology`
    # Since `is_defined_by_ontology` is already defined as a ClassVar, the user can just assign a value here
    is_defined_by_ontology      = OntoSyn
    # 0 and None for field `range` indicates no cardinality restriction (which is also the default value)
    # Therefore, the below line is equivalent to `range: as_range(AnotherConcept)`
    range: as_range(SynthesisStep)

class hasFirstStep(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(SynthesisStep)

class hasNextStep(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(SynthesisStep)

class hasContainerVessel(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(Vessel)

class hasProduct(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(SynthesisProduct)

class isRepresentedBy(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(MetalOrganicPolyhedron)

class hasSynthesisYield(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(SynthesisYield)

class hasYieldMass(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(Mass)

class yieldLimitingSpecies(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(SynthesisReactant)

class hasAddedReactant(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(SynthesisReactant)

class hasAddedSolvent(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(Solvent)

class isAdded(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(MaterialAmount)

class isWashedWith(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(MaterialAmount)

class hasEquipment(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(LabEquipment)

class hasHeatChillDevice(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(HeatChillDevice)

class hasReactionTemperature(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(Temperature)

class hasHeatChillRate(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(TemperatureRate)

class hasHeatChillDuration(ObjectProperty):
    is_defined_by_ontology      = OntoSyn
    range: as_range(Duration)


