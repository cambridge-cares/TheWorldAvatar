# 1. Ontology for Waste Management

OntoWasteManagement is designed to represent and support the day-to-day waste management operations, ranging from handling collection requests and billing reports, meeting reporting requirements, to fleet and bin management. This document serves to explain the modelling decisions and provide example usage of the ontology alongside the external ontologies.

The namespace for the ontology is:

<p align="center"><i>https://www.theworldavatar.com/kg/ontowastemanagement/</i></p>

## Table of Contents

- [1. Ontology for Waste Management](#1-ontology-for-waste-management)
- [2. Data Model](#2-data-model)
  - [Legend](#legend)
  - [2.1 Service Agreement](#21-service-agreement)
  - [2.2 Waste Services](#22-waste-services)
    - [2.2 Waste Categories](#221-waste-categories)
    - [2.2 Waste Disposal Facilities](#222-waste-disposal-facilities)
  - [2.3 Assets](#23-assets)
    - [2.3.1 Truck](#231-truck)
    - [2.3.2 Bin](#232-bin)
  - [2.4 Status](#24-status)
  - [2.5 Reporting](#25-reporting)

# 2. Data model

## Legend

| Prefix            | Namespace                                                                                 |
| ----------------- | ----------------------------------------------------------------------------------------- |
| cmns-col          | `https://www.omg.org/spec/Commons/Collections/`                                           |
| cmns-dt           | `https://www.omg.org/spec/Commons/DatesAndTimes/`                                         |
| cmns-dsg          | `https://www.omg.org/spec/Commons/Designators/`                                           |
| cmns-pts          | `https://www.omg.org/spec/Commons/PartiesAndSituations/`                                  |
| cmns-rlcmp        | `https://www.omg.org/spec/Commons/RolesAndCompositions/`                                  |
| fibo-fnd-agr-ctr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/`                     |
| fibo-fnd-arr-rep  | `https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/`                   |
| fibo-fnd-aap-ppl  | `https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/`                   |
| fibo-fnd-dt-bd    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/BusinessDates/`              |
| fibo-fnd-dt-fd    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/`             |
| fibo-fnd-pas-pas  | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/`  |
| fibo-fnd-pas-psch | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/` |
| fibo-fnd-plc-adr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/`                         |
| fibo-fnd-plc-fac  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Facilities/`                        |
| fibo-fnd-plc-loc  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/`                         |
| fibo-fnd-pty-pty  | `https://spec.edmcouncil.org/fibo/ontology/FND/Parties/Parties/`                          |
| fibo-fnd-rel-rel  | `https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/`                      |
| fibo-fnd-org-fm   | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/`        |
| lcc-cr            | `https://www.omg.org/spec/LCC/Countries/CountryRepresentation/`                           |
| om                | `http://www.ontology-of-units-of-measure.org/resource/om-2/`                              |
| sf                | `http://www.opengis.net/ont/sf#`                                                          |
| geo               | `http://opengis.net/ont/geosparql#`                                                       |
| time              | `https://www.w3.org/TR/owl-time/#time`                                                    |
| vcard             | `https://www.w3.org/2006/vcard/ns`                                                        |
| vc                | `https://spec.edmcouncil.org/auto/ontology/VC/VehicleCore/`                               |
| ontobim           | `https://www.theworldavatar.com/kg/ontobim/`                                              |
| ontoderivation    | `https://www.theworldavatar.com/kg/ontoderivation/`                                       |
| ontoprofile       | `https://www.theworldavatar.com/kg/ontoprofile/`                                          |
| ontoservice       | `https://www.theworldavatar.com/kg/ontoservice/`                                          |
| ontowm            | `https://www.theworldavatar.com/kg/ontowastemanagement/`                                  |

## 2.1. Service agreement

The basis of this ontology revolves around the `fibo-fnd-pas-pas:ServiceAgreement` concept to specify the agreed upon service requirements and terms. The general usage of this concept can be found as part of the [OntoService](https://www.theworldavatar.com/kg/ontoservice/) ontology, including its duration, involved parties, payment obligations, and commitments, as well as the organisation profiles in the [OntoProfile](https://www.theworldavatar.com/kg/ontoprofile/) ontology. This ontology specifically demonstrates the extension of `OntoService` and `OntoProfile` to the waste operation industry.

Figure 1: TBox representation for a Waste Service Agreement following the FIBO ontology

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:ServiceAgreement" {
        rdfs-label agreement_no_string
    }
    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "fibo-fnd-pas-pas:Client" : "fibo-fnd-arr-rep:isRequestedBy"
    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "fibo-fnd-pas-pas:ServiceProvider" : "fibo-fnd-agr-ctr:hasContractParty"

    "fibo-fnd-pas-pas:ServiceProvider" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"
    "fibo-fnd-pas-pas:ServiceProvider" ||--o{ "ontowm:WasteService"  : "fibo-fnd-rel-rel:provides"

    "fibo-fnd-pas-pas:Client" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"

    "fibo-fnd-pas-pas:ServiceAgreement" ||--|{ "ontowm:WasteService" : "fibo-fnd-rel-rel:governs"
    "ontowm:WasteService" ||--o{ "cmns-dt:ExplicitTimePeriod" : "cmns-dt:hasTimePeriod"
    "ontowm:WasteService" ||--|| "ontowm:Waste" : "ontowm:hasWasteType"
    "ontowm:WasteService" ||--|| "ontowm:Bin" : "ontowm:hasBinType"
    "ontowm:WasteService" {
        rdfs-label label_string
        rdfs-comments remarks_string
        ontowm-isSingleStream boolean
    }

    "fibo-fnd-org-fm:FormalOrganization" ||--o{ "ontobim:Facility" : "ontoprofile:hasFacility "

    "bot:Building" ||--|| "lcc-cr:Location" : "ontoservice:hasServiceLocation"
    "bot:Building" ||--o{ "ontobim:Facility" : "ontobim:hasFacility "
    "bot:Building" ||--o{ "fibo-fnd-plc-adr:ConventionalStreetAddress" : "fibo-fnd-plc-adr:hasAddress"
    "fibo-fnd-plc-adr:ConventionalStreetAddress" ||--|| "lcc-cr:Country" : "fibo-fnd-plc-loc:hasCountry"
    "fibo-fnd-plc-adr:ConventionalStreetAddress" ||--o{ "fibo-fnd-plc-adr:StreetAddress" : "fibo-fnd-plc-adr:hasStreetAddress"
    "fibo-fnd-plc-adr:ConventionalStreetAddress" {
        fibo-fnd-plc-loc-hasCityName string
        fibo-fnd-plc-adr-hasPostalCode string
    }
    "fibo-fnd-plc-adr:StreetAddress" ||--o{ "fibo-fnd-plc-adr:PrimaryAddressNumber" : "fibo-fnd-plc-adr:hasPrimaryAddressNumber"
    "fibo-fnd-plc-adr:StreetAddress" ||--o{ "fibo-fnd-plc-adr:StreetName" : "fibo-fnd-plc-adr:hasStreetName"

    "fibo-fnd-plc-adr:PrimaryAddressNumber" {
        fibo-fnd-rel-rel-hasTag block_number_string
    }
    "fibo-fnd-plc-adr:StreetName" {
        fibo-fnd-rel-rel-hasTag string
    }

    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "cmns-dt:DatePeriod" : "cmns-pts:holdsDuring"
    "cmns-dt:DatePeriod" ||--o{ "cmns-dt:Date" : "cmns-dt:hasStartDate"
    "cmns-dt:DatePeriod" ||--o{ "cmns-dt:Date" : "cmns-dt:hasEndDate"
    "cmns-dt:Date" {
        cmns-dt-hasDateValue xsd-date
    }
```

Furthermore, the waste service will be given a contact person, who is an employee of the client. This person will play a role of a service provider who provides a contact service for the waste service specified in the agreement. The waste service is also assigned a corresponding waste category and bin type for service execution.

Figure 2: TBox representation for the client's point of contact (contact service) for the service following the FIBO ontology

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:ServiceAgreement" {
        rdfs-label agreement_no_string
    }
    "fibo-fnd-pas-pas:ServiceAgreement" ||--|{ "fibo-fnd-pas-pas:Service" : "fibo-fnd-rel-rel:governs"

    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "fibo-fnd-pas-pas:Client" : "fibo-fnd-arr-rep:isRequestedBy"
    "fibo-fnd-org-fm:Employer" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"
    "fibo-fnd-pas-pas:Client" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"

    "fibo-fnd-org-fm:Employee" ||--o{ "fibo-fnd-org-fm:Employer" : "fibo-fnd-org-fm:isEmployeeOf"
    "fibo-fnd-org-fm:Employee" ||--o{ "fibo-fnd-aap-ppl:Person"  : "cmns-rlcmp:isPlayedBy"
    "fibo-fnd-pas-pas:ServiceProvider" ||--o{ "fibo-fnd-aap-ppl:Person"  : "cmns-rlcmp:isPlayedBy"
    "fibo-fnd-pas-pas:ServiceProvider" ||--o{ "ontoservice:ContactService"  : "fibo-fnd-rel-rel:provides"
    "ontoservice:ContactService" ||--|{ "ontowm:WasteService" : "ontoservice:servesAsContactFor"
```

In monitoring the services rendered during the lifecycle of a service agreement, the [OntoService](https://www.theworldavatar.com/kg/ontoservice/) ontology can describe, represent, and generate the occurences of the lifecycle, stages, and events according to the real-time occurrences of the service delivered. A comprehensive description of the lifecycle representation is available at [section 2.2 of OntoService](../ontoservice#22-service-agreement-lifecycle). Briefly, each stage will comprise of several events `ContractLifecycleEvent` which occurs multiple times, each represented by an `ContractLifecycleEventOccurrence` instance. Each occurrence can either holds during a date period or occur at an instantaneous time. Events may include missed or terminated service, which should instantiate an occurrence with descriptions/comments if they were to occur. For scheduled service events, a regular schedule represents the upcoming events within the service agreement. When the occurrences occur, they must be instantiated to link with both the event and schedule instances.

Figure 3: TBox representation for the service agreement's overall lifecycle

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycle" ||--|{ "fibo-fnd-pas-pas:ServiceAgreement" : "fibo-fnd-arr-lif:isLifecycleOf"
    "fibo-fbc-pas-fpas:ContractLifecycle" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "fibo-fnd-arr-lif:hasStage"
    "ontoservice:CreationStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ServiceExecutionStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ExpirationStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:CreationStage" : "cmns-dt:succeeds"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ServiceExecutionStage" : "cmns-dt:succeeds"

    "ontoservice:ServiceExecutionStage" ||--o{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "cmns-col:comprises"
    "fibo-fbc-pas-fpas:ContractLifecycleEvent" {
        rdfs-label name-string
        rdfs-comment description-string
    }
    "ontoservice:MissedServiceEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:TerminatedServiceEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:ServiceDeliveryEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:ServiceDeliveryEvent" ||--|| "fibo-fnd-dt-fd:RegularSchedule" : "fibo-fnd-dt-fd:hasSchedule"
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "fibo-fnd-dt-oc:hasOccurrence"
    "fibo-fnd-dt-fd:RegularSchedule" {
        fibo-fnd-dt-fd-hasCount integer
    }

    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "cmns-dt:Date" : "cmns-dt:hasStartDate"
    "cmns-dt:Date" {
        cmns-dt-hasDateValue xsd-date
    }
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "cmns-dt:ExplicitTimePeriod" : "ontoservice:hasTimeSlot"
    "cmns-dt:ExplicitTimePeriod" ||--o{ "cmns-dt:TimeOfDay" : "cmns-dt:hasStart"
    "cmns-dt:ExplicitTimePeriod" ||--o{ "cmns-dt:TimeOfDay" : "cmns-dt:hasEndTime"
    "cmns-dt:TimeOfDay" {
        cmns-dt_hasTimeValue xsd_time_string
    }
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fnd-dt-fd:RecurrenceInterval" : "fibo-fnd-dt-fd:hasRecurrenceInterval"
    "fibo-fnd-dt-bd:DayOfWeek" ||--o{ "fibo-fnd-dt-fd:RecurrenceInterval" : "rdfs:subClassOf"
    "fibo-fnd-dt-bd:DayOfWeek" {
        NamedIndividual fibo-fnd-dt-fd-Monday
        NamedIndividual fibo-fnd-dt-fd-Friday
        NamedIndividual fibo-fnd-dt-fd-Sunday
    }
    "fibo-fnd-dt-bd:DayOfMonth" ||--o{ "fibo-fnd-dt-fd:RecurrenceInterval" : "rdfs:subClassOf"
    "fibo-fnd-dt-bd:DayOfMonth" {
        fibo-fnd-dt-fd-hasOrdinalNumber integer
    }

    "fibo-fbc-pas-fpas:ContractLifecycleEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "lcc-cr:Location" : "fibo-fnd-plc-loc:isLocatedAt"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|| "vc:Truck" : "ontoservice:hasAssignedTransport"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontoservice:TotalPrice" : "ontoservice:hasTotalPrice"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" {
        fibo-fnd-dt-oc-hasEventDate xsd-dateTime
        rdfs-comment remark-string
    }
```

## 2.2. Waste services

In the waste operation industry, several waste services are available below. These services are then performed with a corresponding `ContractLifecycleEventOccurrence` to represent that specific occurrence, and serves as a record to be analysed for quality, efficiency, and compliance with service agreements.

1. **Bin delivery service**: Delivery of an empty bin to site. Must be assigned to an OTC truck
2. **Bin exchange service**: Delivery of an empty bin to site while towing away the existing full bin. Must be assigned to an OTC truck
3. **Tow away service**: Collects and disposes of the waste from the service site without returning the bin afterwards. Must be assigned to an OTC truck
4. **Touch and go service**: Brings an empty bin to collect and dispose of the waste on the service site without returning the bin afterwards. Must be assigned to an OTC truck
5. **Dump and return service**: Brings an empty bin to collect and dispose of the waste on the service site and returns the bin afterwards. Must be assigned to an OTC truck

Figure 4: TBox representation of a waste service event occurrence

```mermaid
    erDiagram
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:ServiceDeliveryEvent" : "cmns-col:comprises"
    "ontoservice:ServiceDeliveryEvent" {
        rdfs-label name-string
        rdfs-comment description-string
    }
    "ontoservice:ServiceDeliveryEvent" ||--|| "fibo-fnd-dt-fd:RegularSchedule" : "fibo-fnd-dt-fd:hasSchedule"
    "ontoservice:ServiceDeliveryEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "lcc-cr:Location" : "fibo-fnd-plc-loc:isLocatedAt"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontoservice:TotalPrice" : "ontoservice:hasTotalPrice"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ " cmns-dt:DatePeriod" : "cmns-pts:holdsDuring"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" {
        rdfs-comment remark-string
    }

    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontowm:Bin" : "ontowm:hasAssignedBin"
    "ontowm:Bin" ||--|| "ontowm:BinStatus" : "ontowm:hasStatus"

    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontowm:Waste" : "ontowm:transportsWaste"
    "ontowm:Waste" ||--|| "ontowm:NetWeight" : "ontowm:hasNetWeight"

    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|| "ontowm:RearEndLoaderTruck" : "ontoservice:hasAssignedTransport"
    "ontowm:RearEndLoaderTruck" ||--|| "fibo-fnd-org-fm:Employee" : "ontowm:hasAssignedDriver"
    "ontowm:RearEndLoaderTruck" ||--|{ "ontowm:GrossWeight" : "ontowm:hasGrossWeight"
    "ontowm:RearEndLoaderTruck" ||--|{ "ontowm:UnladenWeight" : "ontowm:hasUnladenWeight"
```

## 2.2.1 Waste categories

The following waste categories are represented in this ontology:

1. Industrial waste
2. Hardcore waste
3. Sludge waste
4. Biological waste
5. Horticultural waste
6. Tree waste
7. Mixed waste
8. General waste
9. Recycled waste
10. Paper waste
11. Metal waste
12. Plastic waste
13. Glass waste
14. Electronic waste

## 2.2.2 Waste disposal facilities

The waste collected during a waste service often have to be disposed at a particular facility, which is represented in the following manner:

Figure 5: TBox representation of waste disposal facilities

```mermaid
    erDiagram
    "ontowm:WasteDisposalFacility" ||--o{ "ontobim:Facility" : "rdfs:subClassOf"
    "ontowm:WasteDisposalFacility" ||--o{ "ontowm:Waste" : "ontowm:handlesWaste"
    "ontowm:WasteDisposalFacility" ||--o{ "ontowm:DisposalVariableCharge" : "ontowm:hasDisposalCharge"

    "ontowm:WasteDisposalFacility" ||--o{ "fibo-fnd-plc-adr:PhysicalAddress" : "fibo-fnd-plc-adr:hasAddress"
    "fibo-fnd-plc-adr:PhysicalAddress" ||--|| "lcc-cr:Country" : "fibo-fnd-plc-loc:hasCountry"
    "fibo-fnd-plc-adr:PhysicalAddress" {
        fibo-fnd-plc-loc-hasCityName string
        fibo-fnd-plc-adr-hasAddressLine1 string
        fibo-fnd-plc-adr-hasPostalCode string
    }
```

## 2.3 Assets

This ontology provides representation of assets managed by organisation with waste operations, such as bins and garbage trucks. These assets are also intended to have geospatial and temporal representations, which will typically follow the `sf:Point` representation as follows:

Figure 6: TBox representation of geospatial and temporal representation of assets

```mermaid
    erDiagram
    "sf:Point" {geo-asWKT geo-wktLiteral}
    "sf:Point" ||--|| "time:TemporalEntity" : "time:hasTime"
    "time:Instant" ||--o{ "time:TemporalEntity" : "rdfs:subClassOf"
    "time:Interval" ||--o{ "time:TemporalEntity" : "rdfs:subClassOf"
```

### 2.3.1 Truck

The following truck types are employed in the waste management sector:

1. **Rear End Loader Truck**: A truck designed for municipal waste collection, featuring a rear-loading mechanism with a hydraulic lift to compact waste into the truck body.
2. **Hooklift Truck**: A versatile truck equipped with a hydraulic hooklift system, used for transporting various container types, including heavy industrial and commercial waste containers.

Figure 7: TBox representation of a truck for the waste management sector

```mermaid
    erDiagram
    "vc:Truck" ||--|| "fibo-fnd-org-fm:Employee" : "ontowm:hasAssignedDriver"
    "vc:Truck" ||--|{ "sf:Point" : "ontowm:hasGeoLocation"

    "vc:Truck" {
        vc-vehicleIdentificationNumber plate_number_string
    }

    "ontowm:RearEndLoaderTruck" ||--o{ "vc:Truck" : "rdfs:subClassOf"
    "ontowm:HookliftTruck" ||--o{ "vc:Truck" : "rdfs:subClassOf"
```

### 2.3.2 Bin

There are six categories of bins:

1. **OTC bin**: A bin for attaching to an OTC truck
2. **3-feet bin**: An open top container bin with a length of 3 feet
3. **5-feet bin**: An open top container bin with a length of 5 feet
4. **7-feet bin**: An open top container bin with a length of 7 feet
5. **Compactor bin**: An open top container bin with the ability to compact waste

Figure 8: TBox representation of a bin

```mermaid
    erDiagram
    "ontowm:Bin" ||--|{ "sf:Point" : "ontowm:hasGeoLocation"
    "ontowm:Bin" ||--|| "ontowm:BinStatus" : "ontowm:hasStatus"

    "ontowm:Bin" {
        ontowm-hasBinNumber bin_number_string
    }
```

## 2.4 Status

The ontology represents statuses as enums in the corresponding ABox. The available statuses are as follows:

1. Bin Status

   - Filled
   - Empty
   - Available
   - Unavailable
   - Decomissioned

## 2.5 Reporting

This section focuses on reporting matters such as billing, specifically as an the extension of the [OntoService](https://www.theworldavatar.com/kg/ontoservice/) ontology to the waste operation industry. The derived information framework is used in representing how the total price of a waste collection service is calculated and represented in the knowledge graph. The computation of the total price is as follows:

```math
Total Price = Gross Price + Tax \\
Gross Price = Base Charge + Variable Charge + Excess Variable Charge \\
Excess Variable Charge = (Net Waste Weight - Service Tonnage Cap) \\
Net Waste Weight = Gross Truck Weight - Unladen Truck Weight
```

Figure 9: ABox representation of the computation of the total price charged for each occurrence of a waste service

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontoservice:TotalPrice" : "ontoservice:charges"
    "ontoservice:TotalPrice" ||--|| "inst:TotalPriceDerivation" : "ontoderivation:belongsTo"
    "inst:TotalPriceDerivation" ||--|{ "ontoservice:GrossPrice" : "ontoderivation:isDerivedFrom"
    "inst:TotalPriceDerivation" ||--|{ "ontoservice:Tax" : "ontoderivation:isDerivedFrom"
    "inst:TotalPriceDerivation" {
        rdf-type ontoderivation-Derivation
    }

    "ontoservice:TotalPrice" ||--o{ "om:AmountOfMoney" : "rdfs:subClassOf"
    "ontoservice:GrossPrice" ||--o{ "om:AmountOfMoney" : "rdfs:subClassOf"
    "ontoservice:Tax" ||--o{ "om:Percentage" : "rdfs:subClassOf"
    "ontoservice:Tax" {
        rdfs-label tax_policy_string
    }

    "ontoservice:GrossPrice" ||--|| "inst:GrossPriceDerivation" : "ontoderivation:belongsTo"
    "inst:GrossPriceDerivation" ||--|{ "ontoservice:BaseCharge" : "ontoderivation:isDerivedFrom"
    "inst:GrossPriceDerivation" ||--|{ "ontoservice:VariableCharge" : "ontoderivation:isDerivedFrom"
    "inst:GrossPriceDerivation" {
        rdf-type ontoderivation-Derivation
    }

    "ontowm:VariableCharge" ||--o{ "om:AmountOfMoney" : "rdfs:subClassOf"

    "ontoservice:VariableCharge" ||--|| "inst:VariableChargeDerivation" : "ontoderivation:belongsTo"
    "inst:VariableChargeDerivation" ||--|{ "ontoservice:VariableRate" : "ontoderivation:isDerivedFrom"
    "inst:VariableChargeDerivation" ||--|{ "ontoservice:ServiceTonnage" : "ontoderivation:isDerivedFrom"
    "ontoservice:VariableRate" ||--o{ "ontoservice:ServiceTonnage"  : "ontoservice:dependsOn"
    "inst:VariableChargeDerivation" {
        rdf-type ontoderivation-Derivation
    }

    "ontoservice:VariableCharge" ||--|| "inst:ExcessVariableChargeDerivation" : "ontoderivation:belongsTo"
    "inst:ExcessVariableChargeDerivation" ||--|{ "ontoservice:VariableRate" : "ontoderivation:isDerivedFrom"
    "inst:ExcessVariableChargeDerivation" ||--|{ "ontoservice:ServiceMetricCap" : "ontoderivation:isDerivedFrom"
    "inst:ExcessVariableChargeDerivation" ||--|{ "ontowm:NetWasteWeight" : "ontoderivation:isDerivedFrom"
    "ontoservice:VariableRate" ||--o{ "ontoservice:ServiceTonnage"  : "ontoservice:dependsOn"
    "inst:VariableChargeDerivation" {
        rdf-type ontoderivation-Derivation
    }

    "ontowm:NetWasteWeight" ||--|| "inst:NetWeightDerivation" : "ontoderivation:belongsTo"
    "inst:NetWeightDerivation" ||--|{ "ontowm:GrossTruckWeight" : "ontoderivation:isDerivedFrom"
    "inst:NetWeightDerivation" ||--|{ "ontowm:UnladenTruckWeight" : "ontoderivation:isDerivedFrom"
    "ontowm:GrossTruckWeight" ||--|{ "time:Instant" : "ontowm:hasMeasurementTime"
    "ontowm:UnladenTruckWeight" ||--|{ "time:Instant" : "ontowm:hasMeasurementTime"
    "inst:NetWeightDerivation" {
        rdf-type ontoderivation-Derivation
    }
```
