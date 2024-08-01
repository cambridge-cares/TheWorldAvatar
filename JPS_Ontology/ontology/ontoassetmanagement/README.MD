# Ontology for Asset Management
## 1. Introduction
OntoApplication is developed to represent assets and its related information (e.g. purchase documents, location).

## 2. Legend
| Prefix                                                                                                                  | Namespace                                           |
|---------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| [foaf](http://xmlns.com/foaf/0.1/) | `http://xmlns.com/foaf/0.1/`     |
| [rdfs](https://www.w3.org/TR/rdf12-schema/)                      | `http://www.w3.org/2000/01/rdf-schema#` |
| [time](https://www.w3.org/TR/owl-time/)                      | `http://www.w3.org/2006/time#` |
| [bot](https://w3c-lbd-cg.github.io/bot/)                      | `https://w3id.org/bot#` |
| [om](https://github.com/HajoRijgersberg/OM)                      | `http://www.ontology-of-units-of-measure.org/resource/om-2/` |
| [icontact](http://ontology.eil.utoronto.ca/icontact.html)                      | `http://ontology.eil.utoronto.ca/icontact.owl#` |
| [fibo-parties](https://spec.edmcouncil.org/fibo/ontology/FND/Parties/Parties/)                      | `https://spec.edmcouncil.org/fibo/ontology/FND/Parties/Parties/` |
| [fibo-clientandaccounts](https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/ClientsAndAccounts/)                      | `https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/ClientsAndAccounts/` |
| [fibo-paymentsandschedules](https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/)                      | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/` |
| [fibo-currencyamount](https://spec.edmcouncil.org/fibo/ontology/FND/Accounting/CurrencyAmount/)                      | `https://spec.edmcouncil.org/fibo/ontology/FND/Accounting/CurrencyAmount/` |
| [fibo-organizations](https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/Organizations/)                      | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/Organizations/` |
| [fibo-formalorganizations](https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/)                      | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/` |
| [fibo-people](https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/)                      | `https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/` |
| [p2p-doc](https://purl.org/p2p-o/document)                      | `https://purl.org/p2p-o/document#` |
| [p2p-docline](https://purl.org/p2p-o/documentline)                      | `https://purl.org/p2p-o/documentline#` |
| [p2p-invoice](https://purl.org/p2p-o/invoice)                      | `https://purl.org/p2p-o/invoice#` |
| [p2p-item](https://purl.org/p2p-o/item)                      | `https://purl.org/p2p-o/item#` |
| [ontobim](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobim)                      | `https://www.theworldavatar.com/kg/ontobim/` |
| [ontocape_technical_system](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontocape/upper_level/technical_system.owl) | `http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#`     |

## 3. Data Model
The ontology can be divided into these respective domains:

### 3.1. Purchase Document
```mermaid
    erDiagram
    "foaf:Document" ||--o{ "time:Instant" : createdOn
    "foaf:Document" ||--o{ "foaf:Document" : references
    "foaf:Document" ||--o{ "Attachment" : hasAttachment
    "foaf:Document" ||--o{ "fibo-parties:PartyInRole" : hasConcernedParty
    "foaf:Document" {
        hasExternalRemarks string
        hasInternalRemarks string
    }
    "fibo-parties:PartyInRole" }|--|| "ApprovingParty" : "rdfs:subClassOf"
    "fibo-parties:PartyInRole" }|--|| "ValidatingParty" : "rdfs:subClassOf"
    "Attachment" {
        availableAt anyURI
    }
    "foaf:Document" }|--|| "p2p-doc:PurchaseOrder" : "rdfs:subClassOf"
    "p2p-doc:PurchaseOrder" {
        purchaseOrderNumber string
    }
    "foaf:Document" }|--|| "p2p-doc:E-Invoice" : "rdfs:subClassOf"
    "foaf:Document" }|--|| "Quotation" : "rdfs:subClassOf"
    "Quotation" {
        quotationNumber string
    }
    "foaf:Document" }|--|| "PurchaseRequisition" : "rdfs:subClassOf"
    "PurchaseRequisition" {
        purchaseRequisitionNumber string
    }
    "foaf:Document" }|--|| "DeliveryOrder" : "rdfs:subClassOf"
    "DeliveryOrder" {
        deliveryOrderNumber string
    }
    "p2p-doc:E-Invoice" }|--|| "ProformaInvoice" : "rdfs:subClassOf"
    "p2p-doc:E-Invoice" ||--o{ "fibo-clientandaccounts:Account" : "hasReferenceAccount"
    "p2p-doc:PurchaseOrder" ||--o{ "fibo-clientandaccounts:Account" : "hasReferenceAccount"
```
### 3.2. Document Line
```mermaid
    erDiagram
    "p2p-docline:DocumentLine" }|--|| "p2p-docline:InvoiceLine" : "rdfs:subClassOf"
    "p2p-docline:DocumentLine" }|--|| "p2p-docline:PurchaseOrderLine" : "rdfs:subClassOf"
    "p2p-docline:DocumentLine" }|--|| "QuotationLine" : "rdfs:subClassOf"
    "p2p-docline:DocumentLine" }|--|| "PurchaseRequisitionLine" : "rdfs:subClassOf"
    "p2p-docline:DocumentLine" }|--|| "DeliveryOrderLine" : "rdfs:subClassOf"
    "Quotation" ||--o{ "QuotationLine" : hasQuotationLine
    "p2p-doc:PurchaseOrder" ||--o{ "p2p-docline:PurchaseOrderLine" : hasPurchaseOrderLine
    "PurchaseRequisition" ||--o{ "PurchaseRequisitionLine" : hasPurchaseRequisitionLine
    "DeliveryOrder" ||--o{ "DeliveryOrderLine" : hasDeliveryOrderLine
    "p2p-doc:E-Invoice" ||--o{ "p2p-docline:InvoiceLine" : "p2p-invoice:hasInvoiceLine"
```
### 3.3. Payment Terms
```mermaid
    erDiagram
    "p2p-doc:PurchaseOrder" ||--o{ "PaymentTerms" : hasPaymentTerms
    "p2p-doc:E-Invoice" ||--o{ "PaymentTerms" : hasPaymentTerms
    "PurchaseRequisition" ||--o{ "PaymentTerms" : hasPaymentTerms
    "DeliveryOrder" ||--o{ "PaymentTerms" : hasPaymentTerms
    "Quotation" ||--o{ "PaymentTerms" : hasPaymentTerms
    "PaymentTerms" ||--o{ "fibo-paymentsandschedules:Payment" : governs
    "fibo-paymentsandschedules:Payment" ||--o{ "time:Instant" : hasEffectiveDate
    "fibo-paymentsandschedules:Payment" ||--o{ "fibo-currencyamount:MonetaryAmount" : "fibo-paymentsandschedules:hasPaymentAmount"
    "fibo-currencyamount:MonetaryAmount" ||--o{ "om:AmountOfMoney" : hasAmount
```
### 3.4. Delivery Details
```mermaid
    erDiagram
    "p2p-doc:PurchaseOrder" ||--o{ "DeliveryDetails" : hasDeliveryDetails
    "p2p-doc:E-Invoice" ||--o{ "DeliveryDetails" : hasDeliveryDetails
    "PurchaseRequisition" ||--o{ "DeliveryDetails" : hasDeliveryDetails
    "DeliveryOrder" ||--o{ "DeliveryDetails" : hasDeliveryDetails
    "Quotation" ||--o{ "DeliveryDetails" : hasDeliveryDetails
    "DeliveryDetails" ||--o{ "time:Instant" : estimatedDateOfDelivery
    "DeliveryDetails" ||--o{ "time:Instant" : actualDateOfDelivery
    "DeliveryDetails" ||--o{ "ImportDuties" : hasImportDuties
    "DeliveryDetails" ||--o{ "DeliveryMethod" : hasDeliveryMethod
    "DeliveryDetails" {
        trackingNumber string
    }
    "DeliveryModeFreight" ||--o{ "DeliveryMethod" : a
    "DeliveryModeMail" ||--o{ "DeliveryMethod" : a
    "DeliveryModeOwnFleet" ||--o{ "DeliveryMethod" : a
    "DeliveryModePickUp" ||--o{ "DeliveryMethod" : a
    "DeliveryDetails" ||--o{ "fibo-parties:IndependentParty" : hasCourier
    "fibo-parties:IndependentParty" }|--|| "fibo-organizations:Organization" : "rdfs:subClassOf"
    "fibo-organizations:Organization" }|--|| "fibo-formalorganizations:FormalOrganization" : "rdfs:subClassOf"
    "DHL" ||--o{ "fibo-formalorganizations:FormalOrganization" : a
    "FederalExpress" ||--o{ "fibo-formalorganizations:FormalOrganization" : a
    "UnitedParcelService" ||--o{ "fibo-formalorganizations:FormalOrganization" : a
    "DeliveryDetails" ||--o{ "icontact:Address" : deliverTo
    "DeliveryDetails" ||--o{ "icontact:Address" : deliverFrom
```
### 3.5. Price Details
```mermaid
    erDiagram
    "DeliveryDetails" ||--o{ "PriceDetails" : hasPriceDetails
    "p2p-docline:DocumentLine" ||--o{ "PriceDetails" : hasPriceDetails
    "PriceDetails" ||--o{ "TaxRate" : hasTaxRate
    "om:Percentage" }|--|| "TaxRate" : "rdfs:subClassOf"
    "PriceDetails" ||--o{ "Tax" : hasTax
    "om:AmountOfMoney" }|--|| "Tax" : "rdfs:subClassOf"
    "Tax" }|--|| "ImportDuties" : "rdfs:subClassOf"
    "PriceDetails" ||--o{ "Discount" : hasDiscount
    "om:AmountOfMoney" }|--|| "Discount" : "rdfs:subClassOf"
    "PriceDetails" ||--o{ "Price" : hasPrice
    "om:AmountOfMoney" }|--|| "Price" : "rdfs:subClassOf"
    "Price" }|--|| "UnitPrice" : "rdfs:subClassOf"
    "Price" }|--|| "DiscountedUnitPrice" : "rdfs:subClassOf"
    "Price" }|--|| "TotalDiscountedPretaxPrice" : "rdfs:subClassOf"
    "Price" }|--|| "HomeTotalDiscountedAfterTaxPrice" : "rdfs:subClassOf"
```
### 3.6. Item
```mermaid
    erDiagram
    "p2p-docline:DocumentLine" ||--o{ "p2p-item:Item" : "p2p-docline:hasItem"
    "p2p-item:Item" ||--o{ "bot:Element" : references
    "p2p-item:Item" ||--o{ "ontocape_technical_system:TechnicalSystem" : references
    "p2p-item:Item" ||--o{ "fibo-organizations:Organization" : isSuppliedBy
    "p2p-item:Item" ||--o{ "fibo-organizations:Organization" : isManufacturedBy
    "fibo-organizations:Organization" ||--o{ "icontact:Address" : hasAddress
    "p2p-item:Item" ||--o{ "DataSheet" : hasDataSheet
    "foaf:Document" }|--|| "DataSheet" : "rdfs:subClassOf"
    "DataSheet" }|--|| "SpecSheet" : "rdfs:subClassOf"
    "DataSheet" }|--|| "Manual" : "rdfs:subClassOf"
    "DataSheet" {
        availableAt anyURI
    }
```
### 3.7. Element
```mermaid
    erDiagram
    "bot:Element" ||--o{ "bot:Element" : isStoredAt
    "bot:Element" ||--o{ "UserDefinedCategory" : isCategorizedUnder
    "bot:Element" ||--o{ "ontocape_technical_system:TechnicalSystem" : isStoredAt
    "bot:Element" ||--o{ "fibo-parties:IndependentParty" : assignedTo
    "bot:Element" ||--o{ "fibo-parties:IndependentParty" : previouslyAssignedTo
    "bot:Element" ||--o{ "Workspace" : isLocatedAt
    "bot:Element" ||--o{ "MaintenanceSchedule" : hasMaintenanceSchedule
    "bot:Element" ||--o{ "fibo-organizations:Organization" : isSuppliedBy
    "bot:Element" ||--o{ "fibo-organizations:Organization" : isManufacturedBy
    "bot:Element" ||--o{ "DataSheet" : hasDataSheet
    "DataSheet" }|--|| "SpecSheet" : "rdfs:subClassOf"
    "DataSheet" }|--|| "Manual" : "rdfs:subClassOf"
    "DataSheet" {
        availableAt anyURI
    }
    "MaintenanceSchedule" ||--o{ "MaintenanceTask" : hasTask
    "MaintenanceTask" ||--o{ "time:Instant" : performedAt
    "MaintenanceTask" ||--o{ "time:Instant" : scheduledFor
    "MaintenanceTask" ||--o{ "fibo-parties:IndependentParty" : isPerformedBy
    "MaintenanceTask" ||--o{ "time:Interval" : hasInterval
    "bot:Element" ||--o{ "AssetUpdateHistory" : hasUpdateHistory
    "AssetUpdateHistory" ||--o{ "AssetDataUpdate" : hasAssetUpdate
    "AssetDataUpdate" ||--o{ "fibo-parties:IndependentParty" : wasExecutedBy
    "bot:Element" ||--o{ "Project" : allocatedTo
    "bot:Element" {
        serialNumber string
        hasItemInventoryIdentifier string
        hasServiceTag string
        hasCurrentLocation string
    }
```
### 3.8. Technical System
```mermaid
    erDiagram
    "ontocape_technical_system:TechnicalSystem" ||--o{ "bot:Element" : isStoredAt
    "ontocape_technical_system:TechnicalSystem" ||--o{ "UserDefinedCategory" : isCategorizedUnder
    "ontocape_technical_system:TechnicalSystem" ||--o{ "ontocape_technical_system:TechnicalSystem" : isStoredAt
    "ontocape_technical_system:TechnicalSystem" ||--o{ "fibo-parties:IndependentParty" : assignedTo
    "ontocape_technical_system:TechnicalSystem" ||--o{ "fibo-parties:IndependentParty" : previouslyAssignedTo
    "ontocape_technical_system:TechnicalSystem" ||--o{ "Workspace" : isLocatedAt
    "ontocape_technical_system:TechnicalSystem" ||--o{ "MaintenanceSchedule" : hasMaintenanceSchedule
    "ontocape_technical_system:TechnicalSystem" ||--o{ "fibo-organizations:Organization" : isSuppliedBy
    "ontocape_technical_system:TechnicalSystem" ||--o{ "fibo-organizations:Organization" : isManufacturedBy
    "ontocape_technical_system:TechnicalSystem" ||--o{ "DataSheet" : hasDataSheet
    "DataSheet" }|--|| "SpecSheet" : "rdfs:subClassOf"
    "DataSheet" }|--|| "Manual" : "rdfs:subClassOf"
    "DataSheet" {
        availableAt anyURI
    }
    "MaintenanceSchedule" ||--o{ "MaintenanceTask" : hasTask
    "MaintenanceTask" ||--o{ "time:Instant" : performedAt
    "MaintenanceTask" ||--o{ "time:Instant" : scheduledFor
    "MaintenanceTask" ||--o{ "fibo-parties:IndependentParty" : isPerformedBy
    "MaintenanceTask" ||--o{ "time:Interval" : hasInterval
    "ontocape_technical_system:TechnicalSystem" ||--o{ "AssetUpdateHistory" : hasUpdateHistory
    "AssetUpdateHistory" ||--o{ "AssetDataUpdate" : hasAssetUpdate
    "AssetDataUpdate" ||--o{ "fibo-parties:IndependentParty" : wasExecutedBy
    "ontocape_technical_system:TechnicalSystem" ||--o{ "Project" : allocatedTo
    "ontocape_technical_system:TechnicalSystem" {
        serialNumber string
        hasItemInventoryIdentifier string
        hasServiceTag string
        hasCurrentLocation string
    }
```
### 3.9. Furniture and Workspace
```mermaid
    erDiagram
    "bot:Element" ||--o{ "Workspace" : isLocatedAt
    "Workspace" ||--o{ "ontobim:Room" : isLocatedin
    "fibo-people:Person" ||--o{ "Workspace" : hasAllocatedWorkspace
    "Workspace" {
        hasWorkspaceIdentifier string
    }
    "bot:Element" }|--|| "Furniture" : "rdfs:subClassOf"
    "Furniture" }|--|| "Cabinet" : "rdfs:subClassOf"
    "Furniture" {
        hasFurnitureIdentifier string
    }
    "Cabinet" }|--|| "BuiltInCabinet" : "rdfs:subClassOf"
    "Cabinet" }|--|| "PedestalCabinet" : "rdfs:subClassOf"
```
### 3.10. Project, Research Grant, Budget and Budget Category
```mermaid
    erDiagram
    "bot:Element" ||--o{ "Project" : allocatedTo
    "Project" ||--o{ "Project" : hasSubproject
    "Project" ||--o{ "fibo-parties:IndependentParty" : hasMembers
    "Project" ||--o{ "ResearchGrant" : hasGrant
    "Project" {
        hasProjectIdentifier string
    }
    "ResearchGrant" ||--o{ "fibo-clientandaccounts:Account" : hasAccount
    "fibo-clientandaccounts:Account" ||--o{ "Budget" : hasBudget
    "Budget" ||--o{ "BudgetCategory" : hasBudgetCategory
    "BudgetCategory" ||--o{ "ServiceCode" : hasServiceCode
    "ServiceCode" {
        hasServiceCodeIdentifier string
    }
    "BudgetCategory" ||--o{ "ServiceCategory" : hasServiceCategory
    "ServiceCategory" {
        hasServiceCategoryIdentifier string
    }
    "p2p-item:Item" ||--o{ "BudgetCategory" : purchasedUnder
    "BudgetCategory" ||--o{ "om:AmountOfMoney" : hasRemainingAmount
```