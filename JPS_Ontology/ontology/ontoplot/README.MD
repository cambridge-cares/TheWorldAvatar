# Ontology for Plot
## 1. Introduction
OntoPlot is an ontology developed to describe the attributes of land plots. A simple overview of the key attributes of the data model are illustrated in Figure 1. 

The namespace for this ontology is:

<p align="center"><i>https://www.theworldavatar.com/kg/ontoplot/</i></p>

*Figure 1. Data model of plot attributes in OntoPlot*
```mermaid
    erDiagram 
    "Plot" ||--o{ "Plot" : hasNeighbour
    "Plot" ||--o{ "om:Area" : hasPlotArea
    "Plot" ||--o{ "om:Area" : hasMaximumPermittedGFA
    "Plot" ||--o{ "om:AmountOfMoney" : hasPrice
    "Plot" ||--o{ "fibo:Person" : hasOwner
    "Plot" {
        isCornerPlot boolean
    }
    "fibo:Person" ||--o{ "fibo:PersonName" : hasName
    "fibo:PersonName" {
        hasFullLegalName string
        hasSurname string
    }
```
## 2. Legend
Prefix | Namespace
--- | ---
[opr](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoplanningregulation) | `https://www.theworldavatar.com/kg/ontoplanningregulation/`
[ontozoning](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontozoning) | `https://www.theworldavatar.com/kg/ontozoning/`
[om](https://github.com/HajoRijgersberg/OM) | `http://www.ontology-of-units-of-measure.org/resource/om-2/`
[fibo](https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/Person) | `https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/`