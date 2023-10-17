# OntoZoning
## 1. Introduction
OntoZoning is an ontology developed to represent relationships between zones and their associated zone types and uses. A simple overview of the data model is illustrated in Figure 1. The detailed zoning types and uses is available within the ontology itself. Do note that the list is non-exhaustive.

*Figure 1. Data model for OntoZoning*
```mermaid
    erDiagram 
    "Zone" ||--o{ "geo:Feature" : rdfs:subClassOf
    "Zone" ||--o{ "ontoplot:Plot" : hasPlot
    "Zone" ||--o{ "ZoneType" : hasZoneType
    "ZoneType" ||--o{ "ZoneUse" : allowsUse
    "ZoneType" ||--o{ "ZoneUse" : mayAllowUse
    "ZoneType" ||--o{ "ZoneUse" : doesNotAllowUse
    "ZoneUse" ||--o{ "UseQuantum" : hasMaxUseQuantum
    "ZoneUse" ||--o{ "UseQuantum" : hasMinUseQuantum    
    "UseQuantum" {
        hasValue decimal
    }
```
## 2. Modelling Decisions
>ZoneType

- As defined in Singapore's Master Plan 2019, there are 32 unique zoning types, which have been incorporated into this ontology. These zones may host multiple uses. 
- All zone types have a 'Zone' suffix in their class names to bring clarity, especially given the presence of a 'SpecialUse' zone type.
- Certain zone types are subsets of another zone type, and have been reflected as such in the ontology. As subsets, these zone types include their parent's uses and have additional uses based on their legal definitions. Examples are:

Zone Type | Zone Type Subset
--- | ---
Business1 | Business1White
Business2 | Business2White
BusinessPark | BusinessParkWhite
Residential | ResidentialOrInstitution
Residential | ResidentialWithCommercialAtFirstStorey

>ZoneUse

- The term 'zone use' is interpreted to refer to specific land uses, activities, programmes, built structures, or building parts that have implications on legal status and exhaustivity within the zone. 
- Zone uses are universal concepts that may be found in any country; our current list is non-exhaustive, and the class set is infinite by definition. This universal and non-exhaustive class allows the exhaustively defined regulatory framework to function (i.e. it removes the need to regulate use exhaustively). 
- In the first iteration, the list of zone uses have been extracted from the following sources:
    - URA Written Statement
    - Google Places data
    - Activities from the [Land Based Classification Standards (LBCS)](https://enterpriseintegrationlab.github.io/icity/LBCSv2/doc/index-en.html) ontology.

>UseQuantum

- Use quantums are detailed regulations attached only to land uses. They describes the ratio of allowed uses, if multiple uses are allowed within the specific zoning type.