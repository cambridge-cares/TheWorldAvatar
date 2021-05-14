# OntoChemExp (under construction...)

Author: Jiaru Bai (jb2197@cam.ac.uk)

## Data Format Specification

This is an ontology developed to support chemical experiment data. The v1.0 started with supporting several types of combustion experiment, including ignition delay measurement and laminar flame speed measurement. Now we are in the phase of developing towards v2.0 that aims to support a wider range of experiments in the chemical science. 



## Supported Experiment Type (Experiment Template)

Ignition delay measurement

Laminar flame speed measurement

[Power conversion efficient measurement](ExperimentTemplate/HOPV15_Template.xml)

Under construction...



## Example SPARQL Query Script (SPARQL Template)

Ignition delay measurement

Laminar flame speed measurement

[Power conversion efficient measurement](SPARQLTemplate/HOPV15_SPARQL.txt)

Under construction...



## How to use the code

1. Build codes from source (Java 8)
2. Modify below configuration accordingly in [kb.ontochemxp.management.properties](src/main/resources/kb.ontochemexp.management.properties)
   - Basic setup
     - ontochemexp.kb.url - the base URL of the ABox files
     - ontochemexp.kb.root.directory - the name of the folder where the ABox OWL files will be generated, this should be a relative path to the folder where the experiment XML files located
     - ontochemexp.ontology.file.path - the ontology TBox file folder path on your local machine
     - ontochemexp.ontology.file.name - the name of the TBox file on your local machine
     - ontochemexp.kb.file.head.comment - the head comment will appear in the generated ABox files
   - For the purpose of linking to OntoSpecies
     - ontospecies.uniquespeciesiri.kb.server.url - the server address where triple-store for OntoSpecies ABox files located
     - ontospecies.uniquespeciesiri.kb.repository.id - the namespace of the triple-store that contains OntoSpecies ABox files
     - ontospecies.uniquespeciesiri.kb.abox.iri - the base URL of the OntoSpecies ABox files, should be http://www.theworldavatar.com/kb/ontospecies/ by default
   - For provenance information of the experiment data
     - Under construction... will be updated in v1.3...
   - For controlling if generated ABox files are to be uploaded to triple-store automatically
     - files.generation - "local" for only keeping the generated ABox files locally; "server" for uploading to triple-store
     - upload.triple.store.server.url - the server URL to be uploaded to, example of options are provided in the file
     - upload.triple.store.repository.ontochemexp - the namespace to be uploaded to
3. Run com/cmclinnovations.ontochemexp/view/PrimeConversion.java as Java application
4. Point to the folder where the experiment XML files located when a dialog appears

Under construction...



## Useful links

Automated Calibration of a Poly(oxymethylene) Dimethyl Ether Oxidation Mechanism Using the Knowledge Graph Technology [[paper](https://doi.org/10.1021/acs.jcim.0c01322)] [[preprint](https://como.ceb.cam.ac.uk/preprints/262/)]



## Update history

### v1.1 to v1.2 (as of 29 April 2021)

- Concepts

  - Removed class `Value`
  - Renamed class `Property` to `DimensionalQuantity`
  - Made class `BibliographyLink` `EQUIVALENT-TO` class `OntoKin:Reference`
  - Added class `Velocity`, `SootYield`, `MassBurningRate`, `Mass`, `SpecificSurfaceArea`, `Material`, `Fraction`, `Voltage`, `Temperature`, `Length`, `Pressure`, `Density`, `Volume`, `FlowRate`, `Time`, `VolumetricFlowRate`, `ResidenceTime`, `LaminarBurningVelocity`, `Distance`, `InitialComposition`, `IgnitionDelay`, `Composition`, `Concentration`, `EquivalenceRatio`, `TemperatureInReferenceState`, `PressureInReferenceState`, `VolumetricFlowRateInReferenceState`, `ReactorLength`, `Diameter`, `JunctionArchitecture`, `DonorConstructionType`, `Acceptor`, `Donor`, `HomoEnergy`, `LumoEnergy`, `HomoLumoEnergyGap`, `OpticalEnergyGap`, `OpenCircuitPotential`, `ShortCircuitCurrentDensity`, `PowerConversionEfficiency`, `FillFactor` as subclass of `DimensionalQuantity`
  - Added class `http://xmlns.com/foaf/0.1/Agent`, `http://xmlns.com/foaf/0.1/Person`, `http://xmlns.com/foaf/0.1/Organization`, `http://purl.org/ontology/bibo/Journal`
  - Inherited class `OntoKin:PublicationSpecification`, `OntoKin:JournalSpecification`, `OntoKin:ProceedingsSpecification`, `OntoKin:PreprintSpecification` from [`OntoKin`](http://theworldavatar.com/ontology/ontokin/OntoKin.owl) ontology

- Relationships

  - Removed below object properties
    - `<Property hasSpeciesLink SpeciesLink>`
  - Added below object properties
    - `<Composition hasSpeciesLink SpeciesLink>`
    - `<Concentration hasSpeciesLink SpeciesLink>`
    - `<Experiment hasPerformer http://xmlns.com/foaf/0.1/Agent>`
    - `<X refersTo DimensionalQuantity>` to make the direct connection between measured data point `X` with the physical `DimensionalQuantity` it represents
    - `<Acceptor hasComponent Component>`, `<Donor hasComponent Component>`
    - Redundant `hasDataPointX` and `hasUncertainty` related to `X1`-`X11`
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
    - Redundant `hasValue` related to `X1`-`X11`
  - Inherited publication-related data property from [`OntoKin`](http://theworldavatar.com/ontology/ontokin/OntoKin.owl) ontology
      - `<OntoKin:JournalSpecification http://purl.org/ontology/bibo/volume ^^xsd:integer>`
      - `<OntoKin:PublicationSpecification http://purl.org/ontology/bibo/pageStart ^^xsd:integer>`
      - `<OntoKin:PublicationSpecification http://purl.org/ontology/bibo/pageEnd ^^xsd:integer>`
      - `<OntoKin:PublicationSpecification http://purl.org/dc/terms/publisher ^^xsd:string>`
      - `<BibliographyLink http://purl.org/dc/terms/title ^^xsd:string>`
      - `<http://purl.org/ontology/bibo/Journal http://purl.org/dc/terms/title ^^xsd:string>`
      - `<http://purl.org/ontology/bibo/Journal http://purl.org/ontology/bibo/issn ^^xsd:string>`
      - `<http://xmlns.com/foaf/0.1/Person http://xmlns.com/foaf/0.1/familyName ^^xsd:string>`
      - `<http://xmlns.com/foaf/0.1/Person http://xmlns.com/foaf/0.1/givenName ^^xsd:string>`
      - `<http://xmlns.com/foaf/0.1/Person http://xmlns.com/foaf/0.1/name ^^xsd:string>`
  - Updated below data properties
    - `<Property hasLabel ^^xsd:string>` to `<DimensionalQuantity hasLabel ^^xsd:string>`
    - `<Property hasUnits ^^xsd:string>` to `<DimensionalQuantity hasUnits ^^xsd:string>`
    - `<Property hasDescription ^^xsd:string>` to `<DimensionalQuantity hasDescription ^^xsd:string>`
    - `<Property hasSourceType ^^xsd:string>` to `<DimensionalQuantity hasSourceType ^^xsd:string>`
    - `<Property hasDerivedPropertyExists ^^xsd:string>` to `<DimensionalQuantity hasDerivedPropertyExists ^^xsd:string>`
    - `<owl:Thing hasVal ^^xsd:string>` to `<owl:Thing hasValue ^^xsd:string>`
    - `<SpeciesLink hasSMILES ^^xsd:string>` to `<SpeciesLink OntoSpecies:SMILES ^^xsd:string>`
    - `<SpeciesLink hasInChI ^^xsd:string>` to `<SpeciesLink OntoSpecies:inChI ^^xsd:string>`
    - `<SpeciesLink hasCAS ^^xsd:string>` to `<SpeciesLink OntoSpecies:casRegistryID ^^xsd:string>`
  - Modified below data properties to object properties
    - `<Provenance createdBy ^^xsd:string>` to `<Provenance createdBy http://xmlns.com/foaf/0.1/Agent>`
    - `<Modification modifiedBy ^^xsd:string>` to `<Modified modifiedBy http://xmlns.com/foaf/0.1/Agent>`

![OntoChemExp core concepts v1.2 as of 29 April 2021](https://lucid.app/publicSegments/view/23814531-dc4e-47f1-81ba-787ff40709bd/image.png)

### v1.0 to v1.1 (as of 1 April 2021)

- Concepts
  - Removed class `PreferredKey`
  - Added class `ExpSpecs`, `Provenance`, `Modification`, `IgnitionType`, `TimeShift`
- Relationships
  - Removed below object properties
    - `<Experiment OntoChemExp:hasPreferredKey PreferredKey>`
  - Added below object properties
    - `<Experiment OntoChemExp:hasExpSpecs ExpSpecs>`
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

### v1.0

![OntoChemExp core concepts v1.0](https://lucid.app/publicSegments/view/5ac5846a-b0ae-4077-abb1-3d1bf1a3a01c/image.png)