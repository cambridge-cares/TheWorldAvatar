# Ontology for Asset Management
## 1. Introduction
The OntoCompany ontology has been developed to describe the properties of companies and the operations of the industrial facilities they own. It contains several classes and relations specifically included for the purpose of calculating the waste heat emissions of the industrial facilities.

## 2. Legend
| Prefix                                                                                                                  | Namespace                                           |
|---------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [ontobim](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobim)                      | `https://www.theworldavatar.com/kg/ontobim/` |
| [ontocape_technical_system](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontocape/upper_level/technical_system.owl) | `http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#`     |
| [rdfs](https://www.w3.org/TR/rdf12-schema/)                      | `http://www.w3.org/2000/01/rdf-schema#` |
| [om](https://github.com/HajoRijgersberg/OM)                      | `http://www.ontology-of-units-of-measure.org/resource/om-2/` |
| [icontact](http://ontology.eil.utoronto.ca/icontact.html)                      | `http://ontology.eil.utoronto.ca/icontact.owl#` |
| [ontochemplant](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontochemplant)                      | `https://www.theworldavatar.com/kg/ontochemplant/` |
| [ontopowsys_powsysfunction](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontopowsys/PowSysFunction.owl) | `http://www.theworldavatar.com/kg/ontopowsys/powsysfunction/` | 
| [ontocape_chemical_process_system](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl) | `http://www.theworldavatar.com/ontology/ontocape/chemicalprocesssystem/cpsrealization/plant/` | 

## 3. Data Model
The ontology can be divided into these respective domains:

### 3.1. Top-level Classes

```mermaid
    erDiagram
    "Company" ||--o{ "IndustrialFacility" : isOwnerOf
    "Company" ||--o{ "ontopowsys_powsysfunction:PowerConsumption" : hasPowerConsumption
    "Company" ||--o{ "Revenue" : hasRevenue
    "Company" {
        hasSSICCode string
        hasProductionTechnology string
        hasBusinessActivity string
        hasYearOfEstablishment gYear
        hasNumberOfEmployees integer
    }
    "IndustrialFacility" ||--o{ "IndustrialFacilityProperty" : hasIndustrialFacilityProperty

    "IndustrialFacility" ||--o{ "GeneratedHeat" : hasGeneratedHeat
    "IndustrialFacility" ||--o{ "FloorArea" : hasFloorArea


    "IndustrialFacility" ||--o{ "Industry" : belongsToIndustry

    "Industry" }|--|| "PrintingIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "PrecisionEngineeringIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "PharmaceuticalIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "FoodIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "BeverageIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "ChemicalIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "SemiconductorIndustry" : "rdfs:subClassOf"
    "Industry" }|--|| "InformationandCommunicationTechnologyIndustry" : "rdfs:subClassOf"


    "IndustrialFacility" }|--|| "DataCentre" : "rdfs:subClassOf"
    "IndustrialFacility" }|--|| "Factory" : "rdfs:subClassOf"
    "IndustrialFacilityProperty" }|--|| "DataCentreProperty" : "rdfs:subClassOf"
    "IndustrialFacilityProperty" }|--|| "FactoryProperty" : "rdfs:subClassOf"
    "Factory" }|--|| "ontochemplant:ChemicalPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "SemiconductorPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "FoodPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "PrintingPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "PharmaceuticalPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "PrecisionEngineeringPlant" : "rdfs:subClassOf"
```

### 3.2. Subclasses

```mermaid
    erDiagram
    "DataCentre" ||--o{ "MaximumITCapacity" : hasMaximumITCapacity
    "Factory" ||--o{ "DesignCapacity" : hasDesignCapacity
    "Factory" ||--o{ "SpecificEnergyConsumption" : hasSpecificEnergyConsumption
    "Factory" ||--o{ "ThermalEfficiency" : hasThermalEfficiency
    "Factory" ||--o{ "PlantCO2Emission" : hasPlantCO2Emission
    


    "IndustrialFacility" }|--|| "DataCentre" : "rdfs:subClassOf"
    "IndustrialFacility" }|--|| "Factory" : "rdfs:subClassOf"
    "IndustrialFacilityProperty" }|--|| "DataCentreProperty" : "rdfs:subClassOf"
    "IndustrialFacilityProperty" }|--|| "FactoryProperty" : "rdfs:subClassOf"
    "Factory" }|--|| "ontochemplant:ChemicalPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "SemiconductorPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "FoodPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "PrintingPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "PharmaceuticalPlant" : "rdfs:subClassOf"
    "Factory" }|--|| "PrecisionEngineeringPlant" : "rdfs:subClassOf"
```



## Why was the OntoCompany.owl file produced by the TBox Generator edited ##

The generated OWL file was edited to describe the fact that a chemical plant belongs to the chemical industry, a beverage plant belongs to the beverage industry, a food plant belongs to the food industry, and so on. Suppose we put the union of the classes Chemical Plant, Beverage Plant and Food Plant in the domain and the union of the classes Chemical Industry, Beverage Industry and Food Industry in the range of the object property belongs to. In that case, it can be interpreted that a chemical plant belongs to the chemical, beverage, or food industries (or all these industry classes). In order to stop this interpretation, we added the belongs to property in the definition of each plant class.
