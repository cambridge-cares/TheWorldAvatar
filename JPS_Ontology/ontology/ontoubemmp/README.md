# Ontology for Urban Building Energy Modelling (UBEM) and Master Planning (MP) 
## 1. Introduction
The UBEMMP ontology is developed to represent the concepts and relationships related to energy and utility consumption and renewable energy production at the building and larger scale.

### 1.1 Related Ontologies
As part of TheWorldAvatar(TWA), this ontology has a modular design that can be easily incorporated in conjunction with other ontologies. This will provide more comprehensive data coverage across domains and scales. Related ontologies that have been integrated are listed below:

| Ontology                                                                                                                  | Incorporated data                                            |
|---------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [OntoBuildingEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) | Building properties, uses, values, and location              |
| [OntoBIM](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobim)                      | Topological relationships between a building and its elements |
| [OntoBuildingStructure](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuildingstructure)                | Wall and roof facades to receive solar irradiation                          |
| [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice)                | Device properties for solar devices                          |

### 1.2 Related Agents
This ontology is used in the following agent:
1) [CEA Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/CEAAgent)

## 2. Data Model
This diagram includes all relationships in this ontology.
```mermaid
    erDiagram
    "bot:Zone" || -- o{ "UtilitiesConsumption" : consumesUtilities
    "bot:Zone" }|--|| "bot:Building" : "rdfs:subClassOf"
    "bot:Building" ||--o{ "EnergyConsumption" : consumesEnergy
    "bot:Building" ||--o| "SolarIrradiation" : receivesRadiation
    "bot:Building" ||--o| "Radabshz" : receivesRadiation
    "ontoDevice:Device" }|--|| "SolarDevice" : "rdfs:subClassOf"
    "ontoDevice:Device" ||--o{ "EnergyConsumption" : consumesEnergy
    "ontoDevice:Device" ||--o{ "om:Area" : hasArea
    "SolarDevice" ||--o| "CO2Savings" : producesCO2Savings
    "SolarDevice" ||--o| "SolarIrradiation" : receivesRadiation
    "SolarDevice" ||--o{ "Radabshz" : receivesRadiation
    "SolarDevice" }|--|| "PVPanel" : "rdfs:subClassOf"
    "SolarDevice" }|--|| "PVTCollector" : "rdfs:subClassOf"
    "PVPanel" ||--o{ "om:Power" : hasPeakPower
    "PVPanel" ||--o{ "om:SpecificYield" : hasSpecificYield
    "PVTCollector" ||--o{ "om:Power" : hasPeakPower
    "PVTCollector" ||--o{ "om:SpecificYield" : hasSpecificYield

    "ontobuildingstructure:Roof" ||--o| "SolarIrradiation" : receivesRadiation
    "ontobuildingstructure:Roof" ||--o{ "Radabshz" : receivesRadiation
    "ontobuildingstructure:Wall" ||--o| "SolarIrradiation" : receivesRadiation
    "ontobuildingstructure:Wall" ||--o{ "Radabshz" : receivesRadiation
    "ontobuildingstructure:RoofFacade" ||--o{ "om:Area" : hasSolarSuitableArea
    "ontobuildingstructure:RoofFacade" ||--o{ "SolarDevice" : hasSolarSuitableArea
    "ontobuildingstructure:WallFacade" ||--o{ "om:Area" : hasTheoreticalEnergyProduction
    "ontobuildingstructure:WallFacade" ||--o{ "SolarDevice" : hasTheoreticalEnergyProduction

    "bot:Building" ||--o{ "ontobuildingstructure:RoofFacade" : "ontobuildingstructure:hasFacade"
    "bot:Building" ||--o{ "ontobuildingstructure:WallFacade" : "ontobuildingstructure:hasFacade"
    "SolarDevice" ||--o{ "EnergySupply" : producesEnergy
```

This diagram below is broken down further to allow an easier view.
```mermaid
    erDiagram
    "bot:Zone" }|--|| "bot:Building" : "rdfs:subClassOf"
    "bot:Building" ||--o{ "ontobuildingstructure:RoofFacade" : "ontobuildingstructure:hasFacade"
    "bot:Building" ||--o{ "ontobuildingstructure:WallFacade" : "ontobuildingstructure:hasFacade"

    "ontobuildingstructure:RoofFacade" ||--o{ "om:Area" : hasSolarSuitableArea
    "ontobuildingstructure:RoofFacade" ||--o{ "SolarDevice" : hasSolarSuitableArea
    "ontobuildingstructure:WallFacade" ||--o{ "om:Area" : hasTheoreticalEnergyProduction
    "ontobuildingstructure:WallFacade" ||--o{ "SolarDevice" : hasTheoreticalEnergyProduction
    "SolarDevice" ||--o{ "EnergySupply" : producesEnergy


```