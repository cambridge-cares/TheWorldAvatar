# OntoZoning
## 1. Introduction
OntoZoning is an ontology developed to represent relationships between zones and their associated land use types and land uses, that is applicable across different planning contexts. A simple overview of the data model is illustrated in Figure 1. The detailed land use types and land uses is available within the ontology itself. Do note that the list is non-exhaustive.

The namespace for this ontology is:

<p align="center"><i>https://www.theworldavatar.com/kg/ontozoning/</i></p>

*Figure 1. Data model for OntoZoning*
```mermaid
    erDiagram 
    "Zone" ||--o{ "geo:Feature" : "rdfs:subClassOf"
    "Zone" ||--o{ "ontoplot:Plot" : hasPlot
    "ontoplot:Plot" ||--o{ "geo:Feature" : "rdfs:subClassOf"
    "ontoplot:Plot" ||--o{ "LandUseType" : hasLandUseType
    "LandUseType" {
        rdfs:label name
        rdfs:comment description
    }
    "opr:LandUseRegulation" ||--o{ "LandUseType" : "opr:appliesTo"
    "opr:LandUseRegulation" ||--o{ "LandUse" : "opr:allowsUse"
    "opr:LandUseRegulation" ||--o{ "LandUse" : "opr:mayAllowUse"
    "opr:LandUseRegulation" ||--o{ "LandUse" : "opr:doesNotAllowUse"
    "LandUse" ||--o{ "LandUse" : "subcategoryOf"
```

## 2. Legend
Prefix | Namespace
--- | ---
[ontoplot](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoplot) | `https://www.theworldavatar.com/kg/ontoplot/`
[opr](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoplanningregulation) | `https://www.theworldavatar.com/kg/ontoplanningregulation/`

## 3. Modelling Decisions
>LandUseType

- The ontology utilises the zoning categories defined in Singapore's Master Plan to formulate preliminary land use types that can be extrapolated to various planning contexts. In cases where a land use type does not align with existing concepts, it can be instantiated as part of the existing concepts. Notably, the `rdfs:label` and `rdfs:comment` properties can be employed to incorporate the name and description of the land use type. For instance, the 'White' land use in Singapore can be instantiated under the `MixedUse` concept, assigned a label of `White` and a corresponding description. Should the existing types prove insufficient, you are encouraged to extend the ontology. 
- A 'Type' suffix is appended to specific types like `Hotel` to distinguish them from their land use counterparts.
- Please be aware that the land use type has exclusively been assigned to `ontoplot:Plot`. We acknowledge the possibility that various planning departments may store the land use type either within a zone (containing the plot) or directly associated with the plot. However, at present, there is no imperative reason or application necessitating the instantiation of the land use zone in the Abox. Directly assignment to plots streamlines the retrieval process and reduces the cognitive complexity of this concept.

>LandUse

- The term 'land use' is interpreted to refer to specific land uses, green spaces, roads, activities, programmes, built structures, or building parts that have implications on legal status and exhaustivity within the land. At this moment, land uses can include `GreeneryUse`, `RoadUse`, `BuildingUse`, `Activities`, and several other categories. 
- In the first iteration, the list of land uses have been extracted from the following sources:
    - URA Written Statement
    - Google Places data
    - Activities from the [Land Based Classification Standards (LBCS)](https://enterpriseintegrationlab.github.io/icity/LBCSv2/doc/index-en.html) ontology.