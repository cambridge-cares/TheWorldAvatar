# 1. Ontology for Services

OntoService is designed to represent the terms, conditions, and obligations associated with service delivery as well as the execution details. The ontology is primarily an extension of the [Financial Industry Business Ontology (FIBO)](https://spec.edmcouncil.org/fibo/). This document serves to explain the modelling decisions and provide example usage of the ontology alongside the external ontologies.

The namespace for the ontology is:

<p align="center"><i>https://www.theworldavatar.com/kg/ontoservice/</i></p>

## Table of Contents

- [1. Ontology for Services](#1-ontology-for-services)
- [2. Data Model](#2-data-model)
  - [Legend](#legend)
  - [2.1 Service Agreement](#21-service-agreement)
    - [2.1.1 Overview](#211-overview)
    - [2.1.2 Employer-Employee relationships](#212-employer-employee-relationships)
  - [2.2 Person](#22-person)
    - [2.2.1 Overview](#221-overview)
  - [2.3 Relationships with facilities](#23-relationships-with-facilities)

# 2. Data Model

## Legend

| Prefix            | Namespace                                                                                 |
| ----------------- | ----------------------------------------------------------------------------------------- |
| bot               | `https://w3id.org/bot#`                                                                   |
| cmns-dt           | `https://www.omg.org/spec/Commons/DatesAndTimes/`                                         |
| cmns-pts          | `https://www.omg.org/spec/Commons/PartiesAndSituations/`                                  |
| cmns-rlcmp        | `https://www.omg.org/spec/Commons/RolesAndCompositions/`                                  |
| fibo-fnd-agr-ctr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/`                     |
| fibo-fnd-arr-rep  | `https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/`                   |
| fibo-fnd-pas-pas  | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/`  |
| fibo-fnd-pas-psch | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/` |
| fibo-fnd-rel-rel  | `https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations`                       |
| fibo-fnd-org-fm   | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/`        |
| om                | `http://www.ontology-of-units-of-measure.org/resource/om-2/`                              |
| sf                | `http://www.opengis.net/ont/sf#`                                                          |
| geo               | `http://opengis.net/ont/geosparql#`                                                       |
| rdfs              | `http://www.w3.org/2000/01/rdf-schema#`                                                   |
| ontobim           | `https://www.theworldavatar.com/kg/ontobim/`                                              |
| ontoderivation    | `https://www.theworldavatar.com/kg/ontoderivation/`                                       |
| ontoprofile       | `https://www.theworldavatar.com/kg/ontoprofile/`                                          |
| ontoservice       | `https://www.theworldavatar.com/kg/ontoservice/`                                          |

## 2.1. Service Agreement

The basis of this ontology revolves around the `fibo-fnd-pas-pas:ServiceAgreement` concept. The agreement specifies the requirements and terms of the service requested by clients. This section has been split into three figures to improve readability and understanding of the concepts - namely, (1) service agreement duration and parties, (2) payment obligations and (3) service commitment.

Figure 1: TBox representation for a Service Agreement following the FIBO ontology

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:ServiceAgreement" {
        rdfs-label agreement_no_string
    }
    "fibo-fnd-pas-pas:ServiceAgreement" ||--|{ "fibo-fnd-pas-pas:Service" : "fibo-fnd-rel-rel:governs"
    "fibo-fnd-pas-pas:Service" {
        rdfs-label label_string
        rdfs-comments description_string
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
    "fibo-fnd-pas-pas:Client" ||--o{ "fibo-fnd-org-fm:FormalOrganization"  : "cmns-rlcmp:isPlayedBy"
```

The billing charges are described at the service agreement level, which comprise of two types of billing - base charge and variable rate:

- **Base charge**: Fixed service charge
- **Variable rate**: The rates that is being charged based on a service metric.
- **Excess variable rate**: The rates that is being charged based on a service metric that have exceeded the cap specified.

Figure 2: TBox representation of the payment obligations stated in the service agreement for any service rendered

```mermaid
    erDiagram
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

The service agreement will also mandate a service commitment, including the service time, schedule, facility (i.e. the location for service execution) and/or remarks.

- The representation of the service location enables the association of facility to a specific building with their own geolocation as well as the contact person in charge at the location for the required service.
- It is intended that this commitment does not instantiate any further attributes from their corresponding concepts but stores the repeatable categories. A scheduler agent is expected to detect this state and ingest the initial knowledge in Figure 3 to optimise and arrange services with executable details.

Figure 3: TBox representation of the contractual obligations for a service

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:ServiceAgreement" ||--o{ "ontoservice:ServiceCommitment" : "fibo-fnd-agr-ctr:hasContractualElement"

    "ontoservice:ServiceCommitment" {
        rdfs-subClassOf fibo-fnd-agr-ctr-ContractualCommitment
        rdfs-comments remarks_string
        ontoservice-hasPreferredTime xsd_time
    }
    "ontoservice:ServiceCommitment" ||--|| "ontobim:Facility" : "ontowm:services"
    "fibo-fnd-org-fm:FormalOrganization" ||--o{ "ontobim:Facility" : "ontoprofile:hasFacility "
    "fibo-fnd-org-fm:Employee" ||--o{ "ontobim:Facility"  : "ontoprofile:worksAt"
    "fibo-fnd-org-fm:Employee" ||--o{ "ontoprofile:ServiceContact"  : "ontoprofile:hasResponsibility"

    "ontoservice:ServiceCommitment" ||--|| "fibo-fnd-dt-fd:RegularSchedule" : "fibo-fnd-dt-fd:hasSchedule"
     "fibo-fnd-dt-fd:RegularSchedule" {
        fibo-fnd-dt-fd-hasCount integer
    }
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "cmns-dt:Date" : "cmns-dt:hasStartDate"
    "cmns-dt:Date" {
        cmns-dt-hasDateValue xsd-date
    }

    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fnd-dt-fd:RecurrenceInterval" : "fibo-fnd-dt-fd:hasRecurrenceInterval"
    "fibo-fnd-dt-fd:RecurrenceInterval" {
        hasSubClass fibo-fnd-dt-fd-Monday
        hasSubClass fibo-fnd-dt-fd-Friday
        hasSubClass fibo-fnd-dt-fd-Sunday
    }
    "fibo-fnd-dt-bd:DayOfMonth" ||--o{ "fibo-fnd-dt-fd:RecurrenceInterval" : "rdfs:subClassOf"
    "fibo-fnd-dt-bd:DayOfMonth" {
        fibo-fnd-dt-fd-hasOrdinalNumber integer
    }
```

## 2.2. Service events

Service events represent the specific occurrence a service being performed, and serves as a record to be analysed for quality, efficiency, and compliance with service agreements. The scheduler agent is expected to generate a new service event instance based on the associated service commitment and payment obligations following their service intervals.

Figure 4: TBox representation of a service event

```mermaid
    erDiagram
    "fibo-fnd-pas-pas:Service" ||--o{ "ontoservice:ServiceEvent" : "ontoservice:hasEvent"

    "ontoservice:ServiceEvent" ||--|| "ontoservice:ServiceStatus" : "ontoservice:hasStatus"
    "ontoservice:ServiceEvent" ||--|{ "ontoservice:TotalPrice" : "ontoservice:charges"

    "ontoservice:ServiceEvent" {
        ontoservice-hasOrderNumber string
        ontoservice-scheduledOn xsd_date_time
        ontoservice-completedOn xsd_date
    }
```

## 2.3 Reporting

This section focuses on reporting matters such as billing. The derived information framework is used in representing how the total price of a service is calculated and represented in the knowledge graph. The computation of the total price is as follows:

```math
Total Price = Gross Price + Tax \\
Gross Price = Base Charge + Variable Charge + Excess Variable Charge \\
Variable Charge = (Service Metric - Service Metric Cap) \\
```

Figure 5: ABox representation of the provenance structure for the total service charge

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
