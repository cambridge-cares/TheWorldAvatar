# OntoChemExp (under construction...)

Author: Jiaru Bai (jb2197@cam.ac.uk)

## Data Format Specification

This is an ontology developed to support chemical experiment data. The v1.0 started with supporting several types of combustion experiment, including ignition delay measurement and laminar flame speed measurement. Now we are in the phase of developing towards v2.0 that aims to support a wider range of experiments in the chemical science. 



## Supported Experiment Type (Experiment Template)

Ignition delay measurement

Laminar flame speed measurement

Under construction...



## Example SPARQL Query Script (SPARQL Template)

Ignition delay measurement

Laminar flame speed measurement

Under construction...



## How to use the code

Under construction...



## Useful links

[Preprint 262](https://como.ceb.cam.ac.uk/preprints/262/)



## Update history

### v1.1 to v1.2 (as of 14 April 2021)

- Concepts

  - Removed class `Value`
  - Renamed class `Property` to `DimensionalQuantity`
  - Made class `BibliographyLink` `EQUIVALENT-TO` class `OntoKin:Reference`
  - Added class `Velocity`, `SootYield`, `MassBurningRate`, `Mass`, `SpecificSurfaceArea`, `Material`, `Fraction`, `Voltage`, `FlowRate`, `Time`, `VolumetricFlowRate`, `ResidenceTime`, `LaminarBurningVelocity`, `Distance`, `InitialComposition`, `IgnitionDelay`, `Composition`, `Concentration`, `EquivalenceRatio`, `TemperatureInReferenceState`, `PressureInReferenceState`, `VolumetricFlowRateInReferenceState`, `ReactorLength`, `Diameter` as subclass of `DimensionalQuantity`
  - Added class `http://xmlns.com/foaf/0.1/Agent`, `http://xmlns.com/foaf/0.1/Person`, `http://xmlns.com/foaf/0.1/Organization`, `http://purl.org/ontology/bibo/Journal`
  - Inherited class `OntoKin:PublicationSpecification`, `OntoKin:JournalSpecification`, `OntoKin:ProceedingsSpecification`, `OntoKin:PreprintSpecification` from [`OntoKin`](http://theworldavatar.com/ontology/ontokin/OntoKin.owl) ontology

- Relationships

  - Removed below object properties
    - `<Property hasSpeciesLink SpeciesLink>`
    - `<Component hasUncertainty Uncertainty>` class `Component` should not allowed to connect to `Uncertainty` directly
  - Added below object properties
    - `<Composition hasSpeciesLink SpeciesLink>`
    - `<Concentration hasSpeciesLink SpeciesLink>`
    - `<X refersTo DimensionalQuantity>` to make the direct connection between measured data point `X` with the physical `DimensionalQuantity` it represents
    - `<Amount hasUncertainty Uncertainty>` class `Amount` should be used to make the connection with `Uncertainty` when user wants to indicate the uncertainty in `InitialComposition`
  - Inherited publication-related object property from [`OntoKin`](http://theworldavatar.com/ontology/ontokin/OntoKin.owl) ontology
    - `<OntoKin:JournalSpecification OntoKin:specifies http://purl.org/ontology/bibo/Journal>`
    - `<BibliographyLink OntoKin:hasPublicationSpecification OntoKin:PublicationSpecification>`
    - `<BibliographyLink http://purl.org/dc/terms/contributor http://xmlns.com/foaf/0.1/Agent>`
  - Updated below object properties
    - `<Apparatus hasProperty Property>` to `<Apparatus hasDimensionalQuantity DimensionalQuantity>`
    - `<Commonproperties hasProperty Property>` to `<CommonProperties hasDimensionalQuantity DimensionalQuantity>`
    - `<DataGroup hasProperty Property>` to `<DataGroup hasDimensionalQuantity DimensionalQuantity>`
    - `<Property hasUncertainty Uncertainty>` to `<DimensionalQuantity hasUncertainty Uncertainty>`
    - `<Property hasComponent Component>` to `<InitialComposition hasComponent Component>`
    - `<Property hasDerivedProperty DerivedProperty>` to `<DimensionalQuantity hasDerivedProperty DerivedProperty>`
  - Modified below object properties to data properties
    - `<Property OntoChemExp:hasValue Value>` to `<DimensionalQuantity OntoChemExp:hasValue ^^xsd:string>`
  - Removed below data properties
    - `<Property hasName ^^xsd:string>`
    - `<Property hasID ^^xsd:string>`
    - `<Property hasMethod ^^xsd:string>`
    - `<Value hasVal ^^xsd:string>`

  - Updated below data properties
    - `<Property hasLabel ^^xsd:string>` to `<DimensionalQuantity hasLabel ^^xsd:string>`
    - `<Property hasUnits ^^xsd:string>` to `<DimensionalQuantity hasUnits ^^xsd:string>`
    - `<Property hasDescription ^^xsd:string>` to `<DimensionalQuantity hasDescription ^^xsd:string>`
    - `<Property hasSourceType ^^xsd:string>` to `<DimensionalQuantity hasSourceType ^^xsd:string>`
    - `<Property hasDerivedPropertyExists ^^xsd:string>` to `<DimensionalQuantity hasDerivedPropertyExists ^^xsd:string>`
    - `<owl:Thing hasVal ^^xsd:string>` to `<owl:Thing hasValue ^^xsd:string>`
  - Modified below data properties to object properties
    - `<Provenance createdBy ^^xsd:string>` to `<Provenance createdBy http://xmlns.com/foaf/0.1/Agent>`
    - `<Modification modifiedBy ^^xsd:string>` to `<Modified modifiedBy http://xmlns.com/foaf/0.1/Agent>`

### v1.0 to v1.1 (as of 1 April 2021)

- Concepts
  - Removed class `PreferredKey`
  - Added class `ExpSpecs`, `Provenance`, `Modification`, `IgnitionType`, `TimeShift`
- Relationships
  - Removed below object properties
    - `<Experiment OntoChemExp:hasPreferredKey PreferredKey>`
  - Added below object properties
    - `<Experiment OntoChemExp:hasExpSpec ExpSpecs>`
    - `<Experiment OntoChemExp:hasProvenance Provenance>`
    - `<Provenance OntoChemExp:hasModification Modification>`
    - `<ExpSpecs OntoChemExp:hasIgnitionType IgnitionType>`
    - `<ExpSpecs OntoChemExp:hasTimeShift TimeShift>`
  - Removed below data properties
    - `<Experiment OntoChemExp:hasXmlns ^^xsd:string>`, `<Experiment OntoChemExp:hasXmlnsXsi ^^xsd:string>`, `<Experiment OntoChemExp:hasXsiSchemaLocation ^^xsd:string>`
  - Added below data properties
    - `<ExpSpecs OntoChemExp:hasExpType ^^xsd:string>`
    - `<Provenance OntoChemExp:hasDataSource ^^xsd:string>`, `<Provenance OntoChemExp:hasPatent ^^xsd:string>`, `<Provenance OntoChemExp:createdAt ^^xsd:string>`, `<Provenance OntoChemExp:createdBy ^^xsd:string>`
    - `<Modification OntoChemExp:modifiedAt ^^xsd:string>`, `<Modification OntoChemExp:modifiedBy ^^xsd:string>`, `<Modification OntoChemExp:hasModificationDetails ^^xsd:string>`
    - `<IgnitionType OntoChemExp:hasDatTarget ^^xsd:string>`, `<IgnitionType OntoChemExp:hasDatType ^^xsd:string>`, `<IgnitionType OntoChemExp:hasDatAmount ^^xsd:string>`, `<IgnitionType OntoChemExp:hasUnits ^^xsd:string>`
    - `<TimeShift OntoChemExp:hasDatTarget ^^xsd:string>`, `<TimeShift OntoChemExp:hasDatType ^^xsd:string>`, `<TimeShift OntoChemExp:hasDatAmount ^^xsd:string>`
    - `<BibliographyLink OntoChemExp:hasDOI ^^xsd:string>`
    - `<SpeciesLink OntoChemExp:hasCAS ^^xsd:string>`, `<SpeciesLink OntoChemExp:hasInChI ^^xsd:string>`, `<SpeciesLink OntoChemExp:hasSMILES ^^xsd:string>`
    - `<Property OntoChemExp:hasSourceType ^^xsd:string>`, `<Property OntoChemExp:hasMethod ^^xsd:string>`
  - Replaced data property `<OntoChemExp:SpeciesLink OntoChemExp:hasUniqueSpeciesIRI ^^xsd:string>` with object property `<OntoChemExp:SpeciesLink OntoChemExp:hasUniqueSpecies OntoSpecies:Species>`

![OntoChemExp core concepts as of 1 April 2021](https://lucid.app/publicSegments/view/1c4af710-308e-4d14-ba45-cc440a3c31be/image.png)

### v1.0

![OntoChemExp core concepts v1.0](https://lucid.app/publicSegments/view/5ac5846a-b0ae-4077-abb1-3d1bf1a3a01c/image.png)