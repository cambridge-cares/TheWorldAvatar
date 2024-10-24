# 1. Ontology for Services

OntoService is designed to represent the terms, conditions, and obligations associated with service delivery as well as the execution details. The ontology is primarily an extension of the [Financial Industry Business Ontology (FIBO)](https://spec.edmcouncil.org/fibo/). This document serves to explain the modelling decisions and provide example usage of the ontology alongside the external ontologies.

The namespace for the ontology is:

<p align="center"><i>https://www.theworldavatar.com/kg/ontoservice/</i></p>

## Table of Contents

- [1. Ontology for Services](#1-ontology-for-services)
- [2. Data Model](#2-data-model)
  - [Legend](#legend)
  - [2.1 Service Agreement](#21-service-agreement)
  - [2.2 Service Agreement Lifecycle](#22-service-agreement-lifecycle)
    - [2.2.1 Creation and Expiration Stage](#221-creation-and-expiration-stage)
    - [2.2.2 Service Execution Stage](#222-service-execution-stage)
  - [2.3 Reporting](#23-reporting)

# 2. Data Model

## Legend

| Prefix            | Namespace                                                                                        |
| ----------------- | ------------------------------------------------------------------------------------------------ |
| bot               | `https://w3id.org/bot#`                                                                          |
| cmns-cls          | `https://www.omg.org/spec/Commons/Classifiers/`                                                  |
| cmns-col          | `https://www.omg.org/spec/Commons/Collections/`                                                  |
| cmns-dt           | `https://www.omg.org/spec/Commons/DatesAndTimes/`                                                |
| cmns-pts          | `https://www.omg.org/spec/Commons/PartiesAndSituations/`                                         |
| cmns-rlcmp        | `https://www.omg.org/spec/Commons/RolesAndCompositions/`                                         |
| fibo-fbc-pas-fpas | `https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/FinancialProductsAndServices` |
| fibo-fnd-agr-ctr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/`                            |
| fibo-fnd-arr-lif  | `https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/`                         |
| fibo-fnd-arr-rep  | `https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/`                          |
| fibo-fnd-dt-oc    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/`                       |
| fibo-fnd-pas-pas  | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/`         |
| fibo-fnd-pas-psch | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/`        |
| fibo-fnd-plc-adr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/`                                |
| fibo-fnd-plc-loc  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/`                                |
| fibo-fnd-rel-rel  | `https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations`                              |
| fibo-fnd-org-fm   | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/`               |
| lcc-cr            | `https://www.omg.org/spec/LCC/Countries/CountryRepresentation/`                                  |
| om                | `http://www.ontology-of-units-of-measure.org/resource/om-2/`                                     |
| sf                | `http://www.opengis.net/ont/sf#`                                                                 |
| geo               | `http://opengis.net/ont/geosparql#`                                                              |
| rdfs              | `http://www.w3.org/2000/01/rdf-schema#`                                                          |
| ontobim           | `https://www.theworldavatar.com/kg/ontobim/`                                                     |
| ontoderivation    | `https://www.theworldavatar.com/kg/ontoderivation/`                                              |
| ontoprofile       | `https://www.theworldavatar.com/kg/ontoprofile/`                                                 |
| ontoservice       | `https://www.theworldavatar.com/kg/ontoservice/`                                                 |
| vc                | `https://spec.edmcouncil.org/auto/ontology/VC/VehicleCore/`                                      |

## 2.1. Service Agreement

The basis of this ontology revolves around the `fibo-fnd-pas-pas:ServiceAgreement` concept. The agreement specifies the requirements and terms of the service requested by clients. This section has been split into several aspects to improve readability and understanding of the concepts - namely, (1) service agreement duration and parties, (2) payment obligations, and (3) lifecycle.

The service agreement will first define the duration, parties involved, requested service and service location. The representation of the service location enables the association of facility with a specific geolocation for service delivery within the building or site as well as the contact person in charge at the location for the required service (See [OntoProfile](https://www.theworldavatar.com/kg/ontoprofile/)). Additional service details and remarks can also be attached to the `Service` concept when required.

Figure 1: TBox representation for a Service Agreement following the FIBO ontology

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:ServiceAgreement" {
        rdfs-label agreement_no_string
    }
    "fibo-fnd-pas-pas:ServiceAgreement" ||--|{ "fibo-fnd-pas-pas:Service" : "fibo-fnd-rel-rel:governs"
    "fibo-fnd-pas-pas:Service" ||--o{ "cmns-dt:ExplicitTimePeriod" : "cmns-dt:hasTimePeriod"
    "fibo-fnd-pas-pas:Service" {
        rdfs-label label_string
        rdfs-comments remarks_string
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

    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "fibo-fnd-pas-pas:Client" : "fibo-fnd-arr-rep:isRequestedBy"
    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "fibo-fnd-pas-pas:ServiceProvider" : "fibo-fnd-agr-ctr:hasContractParty"

    "fibo-fnd-pas-pas:ServiceProvider" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"
    "fibo-fnd-pas-pas:ServiceProvider" ||--o{ "fibo-fnd-pas-pas:Service"  : "fibo-fnd-rel-rel:provides"
    "fibo-fnd-pas-pas:Client" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"
```

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
    "ontoservice:ContactService" ||--|{ "fibo-fnd-pas-pas:Service" : "ontoservice:servesAsContactFor"
```

The billing charges are described at the service agreement level, which comprise of two types of billing - base charge and variable rate:

- **Base charge**: Fixed service charge
- **Variable rate**: The rates that is being charged based on a service metric.
- **Excess variable rate**: The rates that is being charged based on a service metric that have exceeded the cap specified.

Figure 3: TBox representation of the payment obligations stated in the service agreement for any service rendered

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:ServiceAgreement" ||--|{ "fibo-fnd-pas-pas:Service" : "fibo-fnd-rel-rel:governs"
    "fibo-fnd-pas-pas:Service" ||--o{ "fibo-fnd-pas-psch:PaymentObligation" : "ontoservice:obligedTo"
    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "ontoservice:ServicePaymentObligation" : "fibo-fnd-rel-rel:confers"
    "ontoservice:ServicePaymentObligation" ||--o{ "fibo-fnd-pas-psch:PaymentObligation" : "rdfs:subClassOf"
    "ontoservice:ServicePaymentObligation" ||--|{ "ontoservice:BaseCharge"  : "ontoservice:hasBaseCharge"
    "ontoservice:BaseCharge" ||--o{ "om:AmountOfMoney" : "rdfs:subClassOf"

    "ontoservice:ServicePaymentObligation" ||--o{ "ontoservice:VariableRate"  : "ontoservice:hasVariableRate"
    "ontoservice:VariableRate" ||--o{ "om:AmountOfMoney"  : "ontoservice:hasNumerator"
    "ontoservice:VariableRate" ||--o{ "om:Quantity" : "ontoservice:hasDenominator"
    "ontoservice:ExcessVariableRate" ||--o{ "ontoservice:VariableRate" : "rdfs:subClassOf"

    "ontoservice:VariableRate" ||--o{ "ontoservice:ServiceMetric"  : "ontoservice:dependsOn"
    "ontoservice:ExcessVariableRate" ||--o{ "ontoservice:ServiceMetricCap"  : "ontoservice:dependsOn"

    "ontoservice:ServiceMetricCap" ||--o{ "ontoservice:ServiceMetric" : "rdfs:subClassOf"
    "ontoservice:ServiceTonnage" ||--o{ "ontoservice:ServiceMetric" : "rdfs:subClassOf"

    "ontoservice:ServiceMetric" ||--o{ "om:Quantity"  : "ontoservice:hasQuantity"
    "ontoservice:ServiceTonnage" ||--o{ "om:Mass"  : "ontoservice:hasQuantity"
```

## 2.2. Service Agreement Lifecycle

The events occurring during the service agreement can be represented within a contract lifecycle. This usually consists of three stages in sequence of creation, service execution, and expiration. It is recommended to instantiate a `cmns-dt:succeeds` relationship between this three stages as seen in the figure below. Each stage will comprise of several events `ContractLifecycleEvent` which occurs multiple times, each represented by an `ContractLifecycleEventOccurrence` instance. Each occurrence can include a date time, location, as well as remarks. The following subsections will describe the events occurring within each stage of the lifecycle.

Figure 4: TBox representation of the service agreement's overall lifecycle

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycle" ||--|{ "fibo-fnd-pas-pas:ServiceAgreement" : "fibo-fnd-arr-lif:isLifecycleOf"

    "fibo-fbc-pas-fpas:ContractLifecycle" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "fibo-fnd-arr-lif:hasStage"
    "ontoservice:CreationStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ServiceExecutionStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ExpirationStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:CreationStage" : "cmns-dt:succeeds"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ServiceExecutionStage" : "cmns-dt:succeeds"

    "fibo-fbc-pas-fpas:ContractLifecycleStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "cmns-col:comprises"
    "fibo-fbc-pas-fpas:ContractLifecycleEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "lcc-cr:Location" : "fibo-fnd-plc-loc:isLocatedAt"
    "lcc-cr:Location" ||--|{ "geo:Feature" : "rdfs:subClassOf"

    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" {
        fibo-fnd-dt-oc-hasEventDate xsd-dateTime
        rdfs-comment remark-string
    }
```

## 2.2.1 Creation and Expiration Stage

In the creation stage, the service agreement will need to be created before it is approved, as represented by the `ContractCreation` and `ContractApproval` events. During the expiration stage, the service agreement can end in four situations:

1. All contractually stipulated services have been performed successfully - `ContractDischarge`
2. Agreement was created in fradulent circumstances and services need not be fulfilled - `ContractRescission`
3. Early termination of an agreement initiated and agreed upon both parties - `ContractTermination`
4. Early termination of this agreement when amendments are required, and a separate new agreement should be created - `ContractTermination`

These events are recommended to be linked to their stages using the `cmns-col:comprises` property. Each of these events should occur once, with one instance of `ContractLifecycleEventOccurrence` with a specific date time and remarks if required.

Figure 5: TBox representation of the service agreement's creation and expiration lifecycle stage

```mermaid
    erDiagram
   "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:CreationStage" : "cmns-dt:succeeds"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ServiceExecutionStage" : "cmns-dt:succeeds"

    "ontoservice:CreationStage" ||--o{ "ontoservice:ContractCreation" : "cmns-col:comprises"
    "ontoservice:CreationStage" ||--o{ "ontoservice:ContractApproval" : "cmns-col:comprises"
    "ontoservice:ContractApproval" ||--o{ "ontoservice:ContractCreation" : "cmns-dt:succeeds"
    "ontoservice:ContractCreation" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:ContractApproval" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"

    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ContractDischarge" : "cmns-col:comprises"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ContractRescission" : "cmns-col:comprises"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ContractTermination" : "cmns-col:comprises"
    "ontoservice:ContractDischarge" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:ContractRescission" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:ContractTermination" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"

    "fibo-fbc-pas-fpas:ContractLifecycleEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" {
        fibo-fnd-dt-oc-hasEventDate xsd-dateTime
        rdfs-comment remark-string
    }
```

## 2.2.2 Service Execution Stage

In the service execution stage, the possible events are as follows:

1. `PendingService`: A service event that is currently scheduled but has not yet started
2. `TerminatedService`: A service event that has not started and will not be delivered
3. `ServiceInProgress`: A service event that is currently in progress
4. `CompletedService`: A service event that has completed successfully
5. `UncompletedService`: A service event that has been started but is not completed

The flow of service events is depicted as follows and can be represented using the `cmns-dt:succeeds` relationship:

```mermaid
flowchart LR
    PendingService --Received request from client to cancel scheduled service--> TerminatedService
    PendingService --Start the scheduled service--> ServiceInProgress
    ServiceInProgress --Successful completion--> CompletedService
    ServiceInProgress --Unable to find client at service site--> UncompletedService
```

Multiple occurrences of each event can be instantiated with the `ContractLifecycleEventOccurrence` concept, which must be assigned a specific date time and location. Remarks are optional to assign. These occurrences will serve as a record to be analysed for quality, efficiency, and compliance with service agreements. Additionally, the occurrence of each service in progress, completed or uncompleted service can be assigned a transport if required. Furthermore, completed service occurrences can also be assigned a monetary charge.

Figure 6: TBox representation of the service agreement's service execution lifecycle stage

```mermaid
    erDiagram
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:CreationStage" : "cmns-dt:succeeds"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ServiceExecutionStage" : "cmns-dt:succeeds"
    "ontoservice:ServiceExecutionStage" ||--o{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "cmns-col:comprises"
    "fibo-fbc-pas-fpas:ContractLifecycleEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "lcc-cr:Location" : "fibo-fnd-plc-loc:isLocatedAt"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" {
        fibo-fnd-dt-oc-hasEventDate xsd-dateTime
        rdfs-comment remark-string
    }

    "ontoservice:PendingService" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:ServiceInProgress" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:TerminatedService" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:CompletedService" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"
    "ontoservice:UncompletedService" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdfs:subClassOf"

    "ontoservice:PendingService" o{--|| "ontoservice:ServiceInProgress" : "cmns-dt:succeeds"
    "ontoservice:PendingService" o{--|| "ontoservice:TerminatedService" : "cmns-dt:succeeds"
    "ontoservice:ServiceInProgress" o{--|| "ontoservice:CompletedService" : "cmns-dt:succeeds"
    "ontoservice:ServiceInProgress" o{--|| "ontoservice:UncompletedService" : "cmns-dt:succeeds"
```

Figure 7: TBox representation of the service events and their occurrences

```mermaid
    erDiagram

    "ontoservice:PendingService" o{--|| "ontoservice:ServiceInProgress" : "cmns-dt:succeeds"
    "ontoservice:PendingService" o{--|| "ontoservice:TerminatedService" : "cmns-dt:succeeds"
    "ontoservice:ServiceInProgress" o{--|| "ontoservice:CompletedService" : "cmns-dt:succeeds"
    "ontoservice:ServiceInProgress" o{--|| "ontoservice:UncompletedService" : "cmns-dt:succeeds"

    "ontoservice:ServiceInProgress" ||--|{ "ontoservice:ServiceInProgressOccurrence" : "cmns-cls:classifies"
    "ontoservice:ServiceInProgressOccurrence" {
        rdfs-subClassOf fibo-fbc-pas-fpas-ContractLifecycleEventOccurrence
    }
    "ontoservice:CompletedService" ||--|{ "ontoservice:CompletedServiceOccurrence" : "cmns-cls:classifies"
    "ontoservice:CompletedServiceOccurrence" {
        rdfs-subClassOf fibo-fbc-pas-fpas-ContractLifecycleEventOccurrence
    }
    "ontoservice:UncompletedService" ||--|{ "ontoservice:UncompletedServiceOccurrence" : "cmns-cls:classifies"
    "ontoservice:UncompletedServiceOccurrence" {
        rdfs-subClassOf fibo-fbc-pas-fpas-ContractLifecycleEventOccurrence
    }

    "ontoservice:ServiceInProgressOccurrence" ||--|| "vc:Vehicle" : "ontoservice:assignTransport"
    "ontoservice:CompletedServiceOccurrence" ||--|| "vc:Vehicle" : "ontoservice:assignTransport"
    "ontoservice:CompletedServiceOccurrence" ||--|{ "ontoservice:TotalPrice" : "ontoservice:charges"
    "ontoservice:UncompletedServiceOccurrence" ||--|| "vc:Vehicle" : "ontoservice:assignTransport"
```

## 2.3 Reporting

This section focuses on reporting matters such as billing. The derived information framework is used in representing how the total price of a service is calculated and represented in the knowledge graph. The computation of the total price is as follows:

```math
Total Price = Gross Price + Tax \\
Gross Price = Base Charge + Variable Charge + Excess Variable Charge \\
Variable Charge = (Service Metric - Service Metric Cap) \\
```

Figure 8: ABox representation of the provenance structure for the total service charge

```mermaid
    erDiagram
    "ontoservice:ServiceEvent" ||--|{ "ontoservice:TotalPrice" : "ontoservice:charges"
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
    "inst:VariableChargeDerivation" ||--|{ "ontoservice:ServiceMetric" : "ontoderivation:isDerivedFrom"
    "ontoservice:VariableRate" ||--o{ "ontoservice:ServiceMetric"  : "ontoservice:dependsOn"

    "inst:VariableChargeDerivation" {
        rdf-type ontoderivation-Derivation
    }
```
