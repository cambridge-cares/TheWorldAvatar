# Ontology for Asset Management
## 1. Introduction
OntoApplication is developed to represent assets and its related information (e.g. purchase documents, location).

## 2. Data Model
The ontology can be divided into these respective domains:

#### 2.1 Purchase Document

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
#### 2.2 Document Line
```mermaid
    erDiagram
    "p2p-docLine:DocumentLine" }|--|| "p2p-docLine:InvoiceLine" : "rdfs:subClassOf"
    "p2p-docLine:DocumentLine" }|--|| "p2p-docLine:PurchaseOrderLine" : "rdfs:subClassOf"
    "p2p-docLine:DocumentLine" }|--|| "QuotationLine" : "rdfs:subClassOf"
    "p2p-docLine:DocumentLine" }|--|| "PurchaseRequisitionLine" : "rdfs:subClassOf"
    "p2p-docLine:DocumentLine" }|--|| "DeliveryOrderLine" : "rdfs:subClassOf"
    "Quotation" ||--o{ "QuotationLine" : hasQuotationLine
    "p2p-doc:PurchaseOrder" ||--o{ "p2p-docLine:PurchaseOrderLine" : hasPurchaseOrderLine
    "PurchaseRequisition" ||--o{ "PurchaseRequisitionLine" : hasPurchaseRequisitionLine
    "DeliveryOrder" ||--o{ "DeliveryOrderLine" : hasDeliveryOrderLine
    "p2p-doc:E-Invoice" ||--o{ "p2p-docLine:InvoiceLine" : "p2p-invoice:hasInvoiceLine"
```
#### 2.3 Payment Terms

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
#### 2.4 Delivery Details

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
    "Tax" }|--|| "ImportDuties" : "rdfs:subClassOf"
    "DeliveryDetails" ||--o{ "DeliveryMethod" : hasDeliveryMethod
    "DeliveryModeFreight" ||--o{ "DeliveryMethod" : a
    "DeliveryModeMail" ||--o{ "DeliveryMethod" : a
    "DeliveryModeOwnFleet" ||--o{ "DeliveryMethod" : a
    "DeliveryModePickUp" ||--o{ "DeliveryMethod" : a
    "DeliveryDetails" ||--o{ "fibo-parties:IndependentParty" : hasCourier
    "DHL" ||--o{ "fibo-parties:IndependentParty" : a
    "FederalExpress" ||--o{ "fibo-parties:IndependentParty" : a
    "UnitedParcelService" ||--o{ "fibo-parties:IndependentParty" : a
    "DeliveryDetails" ||--o{ "icontact:Address" : deliverTo
    "DeliveryDetails" ||--o{ "icontact:Address" : deliverFrom
```
#### 2.5 Price Details
```mermaid
    erDiagram
    "DeliveryDetails" ||--o{ "PriceDetails" : hasPriceDetails
    "p2p-docLine:DocumentLine" ||--o{ "PriceDetails" : hasPriceDetails
    "PriceDetails" ||--o{ "TaxRate" : hasTaxRate
    "om:Percentage" }|--|| "TaxRate" : "rdfs:subClassOf"
    "PriceDetails" ||--o{ "Tax" : hasTax
    "om:AmountOfMoney" }|--|| "Tax" : "rdfs:subClassOf"
    "PriceDetails" ||--o{ "Discount" : hasDiscount
    "om:AmountOfMoney" }|--|| "Discount" : "rdfs:subClassOf"
    "PriceDetails" ||--o{ "Price" : hasPrice
    "om:AmountOfMoney" }|--|| "Price" : "rdfs:subClassOf"
    "Price" }|--|| "UnitPrice" : "rdfs:subClassOf"
    "Price" }|--|| "DiscountedUnitPrice" : "rdfs:subClassOf"
    "Price" }|--|| "TotalDiscountedPretaxPrice" : "rdfs:subClassOf"
    "Price" }|--|| "DiscountedHomeTotalAfterTaxPrice" : "rdfs:subClassOf"
```
#### 2.6 Item and Element
```mermaid
    erDiagram
    "p2p-docLine:DocumentLine" ||--o{ "p2p-item:Item" : "p2p-docLine:hasItem"
    "p2p-item:Item" ||--o{ "bot:Element" : references
    "p2p-item:Item" ||--o{ "fibo-organizations:Organization" : isSuppliedBy
    "p2p-item:Item" ||--o{ "fibo-organizations:Organization" : isManufacturedBy
    "bot:Element" ||--o{ "fibo-organizations:Organization" : isSuppliedBy
    "bot:Element" ||--o{ "fibo-organizations:Organization" : isManufacturedBy
    "fibo-organizations:Organization" ||--o{ "icontact:Address" : hasAddress
    "bot:Element" ||--o{ "bot:Element" : isStoredAt
    "bot:Element" ||--o{ "fibo-parties:IndependentParty" : assignedTo
    "bot:Element" ||--o{ "fibo-parties:IndependentParty" : previouslyAssignedTo
    "bot:Element" ||--o{ "Workspace" : isLocatedAt
    "bot:Element" ||--o{ "MaintenanceSchedule" : hasMaintenanceSchedule
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
#### 2.7 Furniture and Workspaces
```mermaid
    erDiagram
    "bot:Element" ||--o{ "Workspace" : isLocatedAt
    "Workspace" ||--o{ "ontobim:Room" : isLocatedin
    "fibo-person:Person" ||--o{ "Workspace" : hasAllocatedWorkspace
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
#### 2.8 Project, Research Grant, Budget and Budget Categories
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
```
