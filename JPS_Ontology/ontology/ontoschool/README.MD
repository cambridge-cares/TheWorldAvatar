# Ontology for School
## 1. Introduction
OntoSchool is developed to represent schools and their information.

## 2. Legend
| Prefix                                                                                                                  | Namespace                                           |
|---------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [icontact](http://ontology.eil.utoronto.ca/icontact.owl) | `http://ontology.eil.utoronto.ca/icontact.owl#`     |
| [fibo_organization](https://spec.edmcouncil.org/fibo/ontology) | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/Organizations/`     |
| [ontobuiltenv](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontobuiltenv/OntoBuiltEnv.owl) | `https://www.theworldavatar.com/kg/ontobuiltenv/`     |
| [rdfs](https://www.w3.org/TR/rdf12-schema/) | `https://www.w3.org/2000/01/rdf-schema#`     |
| [xsd](https://www.w3.org/2001/XMLSchema) | `https://www.w3.org/2001/XMLSchema#`     |
| [ontoschool](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoschool)                      | `https://www.theworldavatar.com/kg/ontoschool/` |
## 3. Data Model
The ontology can be divided into these respective domains:

### 3.1. School and subclasses
```mermaid
    erDiagram
    "fibo_organization:Organization" }|--|| "ontoschool:School" : "rdfs:subClassOf"
    "ontoschool:School" }|--|| "ontoschool:GovernmentSchool" : "rdfs:subClassOf"
    "ontoschool:School" }|--|| "ontoschool:GovernmentAidedSchool" : "rdfs:subClassOf"
    "ontoschool:School" }|--|| "ontoschool:IndependentSchool" : "rdfs:subClassOf"
    "ontoschool:School" }|--|| "ontoschool:SpecialisedSchool" : "rdfs:subClassOf"
    "ontoschool:School" }|--|| "ontoschool:SpecialisedIndependentSchool" : "rdfs:subClassOf"

```
### 3.2. Co-Curicular Activities
```mermaid
    erDiagram
    "ontoschool:School" ||--o{ "ontoschool:CCAGroups" : "ontoschool:hasCCAGroups"
    "ontoschool:CCAGroups" }|--|| "ontoschool:PhysicalSports" : "rdfs:subClassOf"
    "ontoschool:CCAGroups" }|--|| "ontoschool:ClubsAndSocieties" : "rdfs:subClassOf"
    "ontoschool:CCAGroups" }|--|| "ontoschool:UniformedGroups" : "rdfs:subClassOf"
    "ontoschool:CCAGroups" }|--|| "ontoschool:VisualAndPerformingArts" : "rdfs:subClassOf"
    "ontoschool:CCAGroups" }|--|| "ontoschool:StudentLeadership" : "rdfs:subClassOf"
    "ontoschool:CCAGroups" ||--o{ "ontoschool:CCA" : "ontoschool:hasCCA"
```
### 3.3. Contact Number, Email, Website
```mermaid
    erDiagram
    "ontoschool:School" ||--o{ "icontact:PhoneNumber" : "ontoschool:hasContactNumber"
    "icontact:PhoneNumber" {
        icontact-hasPhoneNumber nonNegativeInteger
    }
    "ontoschool:School" {
        icontact-hasWebSite string
        icontact-hasEmail string
    }
```
### 3.4. Education Level
```mermaid
    erDiagram
    "ontoschool:School" ||--o{ "ontoschool:EducationLevel" : "ontoschool:hasEducationLevel"
    "ontoschool:EducationLevel" }|--|| "ontoschool:PrimaryEducation" : "rdfs:subClassOf"
    "ontoschool:EducationLevel" }|--|| "ontoschool:SecondaryEducation" : "rdfs:subClassOf"
    "ontoschool:SecondaryEducation" }|--|| "ontoschool:JuniorCollege" : "rdfs:subClassOf"
    "ontoschool:EducationLevel" }|--|| "ontoschool:TertiaryEducation" : "rdfs:subClassOf"
    "ontoschool:EducationLevel" }|--|| "ontoschool:MixedLevels" : "rdfs:subClassOf"
```
### 3.5. Gender Composition
```mermaid
    erDiagram
    "ontoschool:School" ||--o{ "ontoschool:GenderComposition" : "ontoschool:hasGenderComposition"
    "ontoschool:GenderComposition" }|--|| "ontoschool:Monogender" : "rdfs:subClassOf"
    "ontoschool:Monogender" }|--|| "ontoschool:AllBoys" : "rdfs:subClassOf"
    "ontoschool:Monogender" }|--|| "ontoschool:AllGirls" : "rdfs:subClassOf"
    "ontoschool:GenderComposition" }|--|| "ontoschool:Coeducational" : "rdfs:subClassOf"
```
### 3.6. Address and Built Structures
```mermaid
    erDiagram
    "ontoschool:School" ||--o{ "icontact:Address" : "ontoschool:hasAddress"
        "ontoschool:School" ||--o{ "ontobuiltenv:Property" : "ontoschool:hasProperty"
```