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
    - [2.2.1 Service Agreement Occurrences](#221-service-agreement-occurrences)
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
| fibo-fnd-dt-fd    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/`                    |
| fibo-fnd-dt-bd    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/BusinessDates/`                     |
| fibo-fnd-dt-oc    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/`                       |
| fibo-fnd-pas-pas  | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/`         |
| fibo-fnd-pas-psch | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/`        |
| fibo-fnd-plc-adr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/`                                |
| fibo-fnd-plc-fac  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Facilities/`                               |
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

The service agreement will first define the duration, parties involved, requested service, and service location. The representation of the service location enables the association of facility with a specific geolocation for service delivery within the building or site as well as the contact person in charge at the location for the required service (See [OntoProfile](https://www.theworldavatar.com/kg/ontoprofile/)). Additional service details and remarks can also be attached to the `Service` concept when required.

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

    "fibo-fnd-pas-pas:Service" ||--|{ "fibo-fnd-plc-fac:Capability" : "fibo-fnd-rel-rel:provides"
    "fibo-fnd-plc-fac:Capability" ||--|{ "ontobim:Facility" : "fibo-fnd-rel-rel:involves"
    "fibo-fnd-org-fm:FormalOrganization" ||--o{ "ontobim:Facility" : "fibo-fnd-rel-rel:controls"

    "bot:Building" ||--|| "fibo-fnd-plc-loc:PhysicalLocation" : "ontoservice:hasServiceLocation"
    "fibo-fnd-plc-loc:PhysicalLocation" ||--|{ "geo:Feature" : "rdfs:subClassOf"
    "geo:Feature" ||--|{ "geo:Geometry" : "geo:hasGeometry"
    "geo:Geometry" {
        geo-asWKT wktLiteral
    }

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

A generic lifecycle, comprising of stages and their events, will be represented and instantiated once and reused across all agreement instances. The contract lifecycle usually consists of three stages in sequence of creation, service execution, and expiration. It is recommended to instantiate a `cmns-dt:succeeds` relationship between these three stages as seen in the figure below. Events occurring within each stages are also instantiated and assigned using the `cmns-col:comprises` property. The recommended lifecycle triples are also available as `TTL` format in this directory's `abox.ttl` file.

Figure 4: TBox representation of the service contract lifecycle

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycle" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "fibo-fnd-arr-lif:hasStage"
    "ontoservice:CreationStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ServiceExecutionStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ExpirationStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "rdfs:subClassOf"
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:CreationStage" : "cmns-dt:succeeds"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ServiceExecutionStage" : "cmns-dt:succeeds"

    "ontoservice:CreationStage" ||--o{ "ontoservice:ContractCreation" : "cmns-col:comprises"
    "ontoservice:CreationStage" ||--o{ "ontoservice:ContractApproval" : "cmns-col:comprises"
    "ontoservice:ContractApproval" ||--o{ "ontoservice:ContractCreation" : "cmns-dt:succeeds"
    "ontoservice:ContractCreation" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:ContractApproval" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"

    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:ServiceDeliveryEvent" : "cmns-col:comprises"
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:MissedServiceEvent" : "cmns-col:comprises"
    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:TerminatedServiceEvent" : "cmns-col:comprises"
    "ontoservice:ServiceDeliveryEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:MissedServiceEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:TerminatedServiceEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"

    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ContractDischarge" : "cmns-col:comprises"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ContractRescission" : "cmns-col:comprises"
    "ontoservice:ExpirationStage" ||--o{ "ontoservice:ContractTermination" : "cmns-col:comprises"
    "ontoservice:ContractDischarge" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:ContractRescission" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "ontoservice:ContractTermination" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
```

In the creation stage, the service agreement will need to be created before it is approved, as represented by the `ContractCreation` and `ContractApproval` events.

During the service execution stage, services can result in three kinds of outcomes:

1. `Service Delivery Event`: Delivery of the requested service
2. `Terminated Service Event`: A requested service that has been terminated either by the service provider or the client
3. `Missed Service Event`: A requested service that fails to be delivered to the client

During the expiration stage, the service agreement can end in four situations:

1. `ContractDischarge`: All contractually stipulated services have been performed successfully
2. `ContractRescission`: Agreement was created in fradulent circumstances and services need not be fulfilled
3. `ContractTermination`: Early termination of an agreement initiated and agreed upon both parties
4. `ContractTermination`: Early termination of this agreement when amendments are required, and a separate new agreement should be created

### 2.2.1 Service Agreement Occurrences

Occurrences serve to represent the lifecycle of each service agreement in a separate distinguishable instance to record the details of that agreement. By separating the occurrence from the overall lifecycle process, the ontology also reduces the duplication of triples created for the lifecycle representation. Each occurrence can either holds during a date period or occur at an instantaneous time. Note that occurrences will be generated at the overall lifecycle, lifecycle stage, and lifecycle event level, and they are linked in the same hierarchial order as their occurrence kind:

- `Lifecycle hasStage LifecycleStage` and `LifecycleOccurrence hasStage LifecycleStageOccurrence`
- `LifecycleStage comprises LifecycleEvent` and `LifecycleStageOccurrence comprises LifecycleEventOccurrence`
- `ServiceExecutionStage succeeds CreationStage` and `ServiceExecutionStageOccurrence succeeds CreationStageOccurrence`

Events in the creation and expiration stage are expected to occur once and we recommend to generate one instance of `ContractLifecycleEventOccurrence` with a specific date time and remarks if required. If the service is terminated or missed, it is recommended to instantiate a new occurrence for the corresponding event with the required time stamp and reason for its occurrence (`rdfs:comment`).

Figure 5: TBox representation of the service agreement's occurrences within a service contract lifecycle

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycleOccurrence" }|--|| "fibo-fnd-pas-pas:ServiceAgreement" : "fibo-fnd-arr-lif:hasLifecycle"
    "fibo-fbc-pas-fpas:ContractLifecycleOccurrence" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycle" : "fibo-fnd-rel-rel:exemplifies"
    "fibo-fbc-pas-fpas:ContractLifecycleOccurrence" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" : "fibo-fnd-arr-lif:hasStage"
    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "fibo-fnd-rel-rel:exemplifies"
    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-col:comprises"

    "fibo-fbc-pas-fpas:ContractLifecycle" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleStage" : "fibo-fnd-arr-lif:hasStage"

    "fibo-fbc-pas-fpas:ContractLifecycleStage" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "cmns-col:comprises"
    "fibo-fbc-pas-fpas:ContractLifecycleEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"

    "fibo-fbc-pas-fpas:ContractLifecycleOccurrence" ||--|{ "fibo-fnd-dt-oc:Occurrence" : "rdfs:subClassOf"
    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|{ "fibo-fnd-dt-oc:Occurrence" : "rdfs:subClassOf"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "fibo-fnd-dt-oc:Occurrence" : "rdfs:subClassOf"

    "fibo-fnd-dt-oc:Occurrence" ||--|{ " cmns-dt:DatePeriod" : "cmns-pts:holdsDuring"

    "fibo-fnd-dt-oc:Occurrence" {
        fibo-fnd-dt-oc-hasEventDate xsd-dateTime
        rdfs-comment remark-string
    }
```

### 2.2.2 Service Execution Stage

This stage governs the delivery of the requested service stated in the service agreement.

#### Regular Schedule

In representing the upcoming service delivery occurrences, a regular schedule is assigned to the stage occurrence. The start date of the schedule must follow the `StartDate` instance linked to the service execution lifecycle stage. The representation of the requested service time employs the `cmns-dt:ExplicitTimePeriod` concept via the `cmns-dt:hasTimePeriod` relationship. In representing the recurrence interval ie weekly on every monday and tuesday, users can employ the `fibo-fnd-dt-fd:hasRecurrenceInterval` relationship along with an [`fibo-fnd-dt-fd:ExplicitRecurrenceInterval`](https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/ExplicitRecurrenceInterval) concept. This will have a corresponding ISO 8601 duration string literal linked by the `hasDurationValue` relationship. For example, daily = `P1D`, weekly = `P7D`, and alternate day = `P2D`. In the case where services must be delivered for multiple days within a week, additional `fibo-fnd-dt-bd:BusinessRecurrenceInterval` recurrence intervals can be instantiated to represent the day of week or day of month required. The `fibo-fnd-dt-fd:hasCount` property denotes the number of scheduled service occurrence and will end the agreement once all services have been delivered.

Figure 6: TBox representation of a regular schedule

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|{ "ontoservice:ServiceExecutionStage" : "fibo-fnd-rel-rel:exemplifies"
    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|{ "cmns-dt:DatePeriod" : "cmns-pts:holdsDuring"
    "cmns-dt:DatePeriod" ||--o{ "inst:EndDate" : "cmns-dt:hasEndDate"
    "cmns-dt:DatePeriod" ||--o{ "inst:StartDate" : "cmns-dt:hasStartDate"
    "inst:EndDate" ||--o{ "cmns-dt:Date" : "rdfs:subClassOf"
    "inst:StartDate" ||--o{ "cmns-dt:Date" : "rdfs:subClassOf"

    "cmns-dt:Date" {
        cmns-dt-hasDateValue xsd-date
    }

    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:ServiceDeliveryEvent" : "cmns-col:comprises"
    "ontoservice:ServiceDeliveryEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"

    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|| "fibo-fnd-dt-fd:RegularSchedule" : "fibo-fnd-dt-fd:hasSchedule"
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "fibo-fnd-dt-oc:hasOccurrence"
    "fibo-fnd-dt-fd:RegularSchedule" {
        fibo-fnd-dt-fd-hasCount integer
    }

    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "inst:StartDate" : "cmns-dt:hasStartDate"
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "cmns-dt:ExplicitTimePeriod" : "cmns-dt:hasTimePeriod"
    "cmns-dt:ExplicitTimePeriod" ||--o{ "cmns-dt:TimeOfDay" : "cmns-dt:hasStart"
    "cmns-dt:ExplicitTimePeriod" ||--o{ "cmns-dt:TimeOfDay" : "cmns-dt:hasEndTime"
    "cmns-dt:TimeOfDay" {
        cmns-dt_hasTimeValue xsd_time_string
    }
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fnd-dt-fd:ExplicitRecurrenceInterval" : "fibo-fnd-dt-fd:hasRecurrenceInterval"
    "fibo-fnd-dt-fd:ExplicitRecurrenceInterval"{
        cmns-dt_hasDurationValue ISO8601_duration
    }

    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fnd-dt-bd:BusinessRecurrenceInterval" : "fibo-fnd-dt-fd:hasRecurrenceInterval"

    "fibo-fnd-dt-bd:DayOfWeek" ||--o{ "fibo-fnd-dt-bd:BusinessRecurrenceInterval" : "rdfs:subClassOf"
    "fibo-fnd-dt-bd:DayOfWeek" {
        NamedIndividual fibo-fnd-dt-fd-Monday
        NamedIndividual fibo-fnd-dt-fd-Tuesday
        NamedIndividual fibo-fnd-dt-fd-Wednesday
        NamedIndividual fibo-fnd-dt-fd-Thursday
        NamedIndividual fibo-fnd-dt-fd-Friday
        NamedIndividual fibo-fnd-dt-fd-Saturday
        NamedIndividual fibo-fnd-dt-fd-Sunday
    }
    "fibo-fnd-dt-bd:DayOfMonth" ||--o{ "fibo-fnd-dt-bd:BusinessRecurrenceInterval" : "rdfs:subClassOf"
    "fibo-fnd-dt-bd:DayOfMonth" {
        fibo-fnd-dt-fd-hasOrdinalNumber integer
    }
```

#### Service Delivery Event

Each service delivery occurrence can be instantiated with the `ContractLifecycleEventOccurrence` concept, which must be assigned a specific date time and location. These occurrences will serve as a record to be analysed for quality, efficiency, and compliance with service agreements. Additionally, the occurrence of each service can be assigned a transport and/or a monetary charge if required.

Figure 7: TBox representation of the service agreement's service execution lifecycle stage

```mermaid
    erDiagram
    "ontoservice:ServiceExecutionStage" }|--|| "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" : "fibo-fnd-rel-rel:exemplifies"
    "fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence" ||--|| "fibo-fnd-dt-fd:RegularSchedule" : "fibo-fnd-dt-fd:hasSchedule"
    "fibo-fnd-dt-fd:RegularSchedule" ||--o{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "fibo-fnd-dt-oc:hasOccurrence"

    "ontoservice:ServiceExecutionStage" ||--o{ "ontoservice:ServiceDeliveryEvent" : "cmns-col:comprises"
    "ontoservice:ServiceDeliveryEvent" ||--|| "fibo-fbc-pas-fpas:ContractLifecycleEvent" : "rdf:type"
    "fibo-fbc-pas-fpas:ContractLifecycleEvent" {
        rdfs-label name-string
        rdfs-comment description-string
    }

    "fibo-fbc-pas-fpas:ContractLifecycleEvent" ||--|{ "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" : "cmns-cls:classifies"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "fibo-fnd-plc-loc:PhysicalLocation" : "fibo-fnd-plc-loc:isLocatedAt"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|| "vc:Vehicle" : "ontoservice:hasAssignedTransport"
    "vc:Vehicle" ||--|| "ontoprofile:Driver" : "ontoservice:hasAssignedDriver"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontoservice:TotalPrice" : "ontoservice:hasTotalPrice"
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" {
        fibo-fnd-dt-oc-hasEventDate xsd-dateTime
        rdfs-comment remark-string
    }
```

## 2.3 Reporting

This section focuses on reporting matters such as billing. The derived information framework is used in representing how the total price of a service is calculated and represented in the knowledge graph. The computation of the total price is as follows:

$$
  \begin{align*}
    Total Price = Gross Price + Tax \\
    Gross Price = Base Charge + Variable Charge + Excess Variable Charge \\
    Variable Charge = (Service Metric - Service Metric Cap) \\
  \end{align*}
$$

Figure 8: ABox representation of the provenance structure for the total service charge

```mermaid
    erDiagram
    "fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence" ||--|{ "ontoservice:TotalPrice" : "ontoservice:hasTotalPrice"
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
