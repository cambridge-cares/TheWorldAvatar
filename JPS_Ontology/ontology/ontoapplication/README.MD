# Ontology for Application
## 1. Introduction
OntoApplication is developed to represent software applications and their services. 

## 2. Data Model
This diagram includes all relationships in this ontology.
```mermaid
    erDiagram
    "ontodevice:SmartPhone" ||--o{ "MobileApplication" : hasInstalled
    "Application" }|--|| "MobileApplication" : "rdfs:subClassOf"
    "Application" ||--o{ "Inbox" : hasInbox
    "Inbox" ||--o{ "Message" : contains
    "Message" ||--o{ "MessageCategory" : hasMessageCategory
    "MessageCategory" }|--|| "AssetInformation" : "rdfs:subClassOf"
    "MessageCategory" }|--|| "MaintenanceInformation" : "rdfs:subClassOf"
    "Application" {
        hasApplicationID string
        hasFireBaseID string
    }
    "Message" {
        hasTitle string
        hasContent string
        hasMessageID string
    }
```