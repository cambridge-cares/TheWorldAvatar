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

### v1.0 to v2.0 (as of 1 April 2021)

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

![OntoChemExpCoreConcepts_asof20210401](C:\Users\jb2197\Downloads\OntoChemExpCoreConcepts_asof20210401.png)

### v1.0

![Core concepts](C:\Users\jb2197\Downloads\OntoChemExpCoreConcepts.png)