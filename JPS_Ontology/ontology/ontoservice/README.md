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

> Ontology Diagram

The representation of a class and instance is denoted by the node's shape. This means that an instance of the `Person` class will share the same label as the `Person` class itself, and they can only be distinguished by their shape. Literals are represented within the node of the class.

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none

    %% Diagram
    clazz[Class]
    instance[[Instance]]
    clazzLiteral["<h4>Class</h4><p style='font-size:0.75rem;'>dataproperty &quot;literal type&quot;</p>"]:::literal
```

For relations, unlabelled arrows references `rdfs:subClassOf` and `rdf:type` for default and dotted lines respectively. Default lines are relations that involve at least one class, whereas dotted lines are relations between instances of one or more classes.

```mermaid
flowchart LR
    subclass[SubClass] --> superclass[SuperClass]
    instance[[Instance]] -.-> clazz[Class]
    domainClazz[Class] -- relation --> rangeClazz[Class]
    domainInst[[Instance]] -- relation --> rangeClazz[Class]
    domainInst -. relation .-> rangeInst[[Instance]]
```

> Namespace Prefix

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
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[["<h4>fibo-fnd-pas-pas:ServiceAgreement</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;</p>"]]:::literal -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    Agreement -. fibo-fnd-rel-rel:governs .-> Service[["<h4>fibo-fnd-pas-pas:Service</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;<br>rdfs:comments &quot;remarks string&quot;</p>"]]:::literal
    Service -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:ExplicitTimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[Time]]
    StartTime -.-> Time["<h4>cmns-dt:TimeOfDay</h4><p style='font-size:0.75rem;'>cmns-dt:hasTimeValue &quot;xsd:time&quot;</p>"]:::literal
    EndTime -.-> Time

    Agreement -. fibo-fnd-agr-ctr:hasContractParty .-> ServiceProvider[[fibo-fnd-pas-pas:ServiceProvider]]
    ServiceProvider -. cmns-rlcmp:isPlayedBy .-> ProviderOrg[[FormalOrganization]]
    ProviderOrg -.-> Org
    ServiceProvider -. fibo-fnd-rel-rel:provides .-> Service
    Agreement -. fibo-fnd-arr-rep:isRequestedBy .-> Client[[fibo-fnd-pas-pas:Client]]
    Client -. cmns-rlcmp:isPlayedBy .-> ClientOrg[[FormalOrganization]]
    ClientOrg -.-> Org[[fibo-fnd-org-fm:FormalOrganization]]

    Service -. fibo-fnd-rel-rel:provides .-> Capability[[fibo-fnd-plc-fac:Capability]]
    Capability -. fibo-fnd-rel-rel:involves .-> Facility[[ontobim:Facility]]
    Org -. fibo-fnd-rel-rel:controls .-> Facility

    Building[[bot:Building]] -. ontobim:hasFacility .-> Facility
    Building -. ontoservice:hasServiceLocation .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]
    Location --> Feature[geo:Feature]
    Feature -. geo:hasGeometry .-> Geom[["<h4>cmns-dt:geo:Geometry</h4><p style='font-size:0.75rem;'>geo:asWKT &quot;geo:wktLiteral&quot;</p>"]]:::literal

    Building -. fibo-fnd-plc-adr:hasAddress .-> ConventionalStreetAddress[["<h4>fibo-fnd-plc-adr:ConventionalStreetAddress</h4><p style='font-size:0.75rem;'>fibo-fnd-plc-loc:hasCityName &quot;string&quot;<br>fibo-fnd-plc-adr:hasPostalCode &quot;string&quot;</p>"]]:::literal

    ConventionalStreetAddress -. fibo-fnd-plc-loc:hasCountry .-> Country[[lcc-cr:Country]]
    ConventionalStreetAddress -. fibo-fnd-plc-adr:hasStreetAddress .-> StreetAddress[[fibo-fnd-plc-adr:StreetAddress]]

    StreetAddress -. fibo-fnd-plc-adr:hasPrimaryAddressNumber .-> PrimaryAddressNumber[["<h4>fibo-fnd-plc-adr:PrimaryAddressNumber</h4><p style='font-size:0.75rem;'>fibo-fnd-rel-rel:hasTag &quot;string&quot;</p>"]]:::literal
    StreetAddress -. fibo-fnd-plc-adr:hasStreetName .-> StreetName[["<h4>fibo-fnd-plc-adr:StreetName</h4><p style='font-size:0.75rem;'>fibo-fnd-rel-rel:hasTag &quot;string&quot;</p>"]]:::literal
```

Figure 2: TBox representation for the client's point of contact (contact service) for the service following the FIBO ontology

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[["<h4>fibo-fnd-pas-pas:ServiceAgreement</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;</p>"]]:::literal -. fibo-fnd-rel-rel:governs .-> Service[[fibo-fnd-pas-pas:Service]]
    Agreement -. fibo-fnd-arr-rep:isRequestedBy .-> Client[[fibo-fnd-pas-pas:Client]]
    Employer[[fibo-fnd-org-fm:Employer]] -. cmns-rlcmp:isPlayedBy .-> Org[[fibo-fnd-org-fm:FormalOrganization]]
    Client -. cmns-rlcmp:isPlayedBy .-> Org

    Employee[[fibo-fnd-org-fm:Employee]] -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    Employee -. fibo-fnd-org-fm:isEmployeeOf .-> Employer
    ServiceProvider[[fibo-fnd-pas-pas:ServiceProvider]] -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    ServiceProvider -. fibo-fnd-rel-rel:provides .-> ContactService[[ontoservice:ContactService]]
    ContactService -. ontoservice:servesAsContactFor .-> Service
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
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Lifecycle[fibo-fbc-pas-fpas:ContractLifecycle] -- fibo-fnd-arr-lif:hasStage --> Stage[fibo-fbc-pas-fpas:ContractLifecycleStage]
    Stage -- cmns-col:comprises --> Event[fibo-fbc-pas-fpas:ContractLifecycleEvent]

    CreationStage[ontoservice:CreationStage] --> Stage
    ServiceExecutionStage[ontoservice:ServiceExecutionStage] --> Stage
    ExpirationStage[ontoservice:ExpirationStage] --> Stage
    ServiceExecutionStage -- cmns-dt:succeeds --> CreationStage
    ExpirationStage -- cmns-dt:succeeds --> ServiceExecutionStage

    CreationStage -- cmns-col:comprises --> ContractCreation[ontoservice:ContractCreation]
    CreationStage -- cmns-col:comprises --> ContractApproval[ontoservice:ContractApproval]
    ContractApproval -- cmns-dt:succeeds --> ContractCreation
    ContractCreation --> Event
    ContractApproval --> Event

    ServiceExecutionStage -- cmns-col:comprises --> ServiceDeliveryEvent[ontoservice:ServiceDeliveryEvent]
    ServiceExecutionStage -- cmns-col:comprises --> MissedServiceEvent[ontoservice:MissedServiceEvent]
    ServiceExecutionStage -- cmns-col:comprises --> TerminatedServiceEvent[ontoservice:TerminatedServiceEvent]
    ServiceDeliveryEvent --> Event
    MissedServiceEvent --> Event
    TerminatedServiceEvent --> Event

    ExpirationStage -- cmns-col:comprises --> ContractDischarge[ontoservice:ContractDischarge]
    ExpirationStage -- cmns-col:comprises --> ContractRescission[ontoservice:ContractRescission]
    ExpirationStage -- cmns-col:comprises --> ContractTermination[ontoservice:ContractTermination]
    ContractDischarge --> Event
    ContractRescission --> Event
    ContractTermination --> Event
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
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[[fibo-fnd-pas-pas:ServiceAgreement]]:::literal -. fibo-fnd-arr-lif:hasLifecycle .-> LifecycleOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleOccurrence]]
    LifecycleOccurrence -. fibo-fnd-arr-lif:hasStage .-> StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]]
    StageOccurrence -. cmns-col:comprises .-> EventOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence]]

    LifecycleOccurrence -- fibo-fnd-rel-rel:exemplifies --> Lifecycle[fibo-fbc-pas-fpas:ContractLifecycle]
    StageOccurrence -- fibo-fnd-rel-rel:exemplifies --> Stage[fibo-fbc-pas-fpas:ContractLifecycleStage]
    EventOccurrence -- fibo-fnd-rel-rel:exemplifies --> Event[fibo-fbc-pas-fpas:ContractLifecycleEvent]

    Lifecycle -- fibo-fnd-arr-lif:hasStage --> Stage
    Stage -- cmns-col:comprises --> Event

    LifecycleOccurrence --> Occurrence["<h4>fibo-fnd-dt-oc:Occurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]:::literal
    StageOccurrence --> Occurrence
    EventOccurrence --> Occurrence
    Occurrence -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date
```

### 2.2.2 Service Execution Stage

This stage governs the delivery of the requested service stated in the service agreement.

#### Regular Schedule

In representing the upcoming service delivery occurrences, a regular schedule is assigned to the stage occurrence. The start date of the schedule must follow the `StartDate` instance linked to the service execution lifecycle stage. The representation of the requested service time employs the `cmns-dt:ExplicitTimePeriod` concept via the `cmns-dt:hasTimePeriod` relationship. In representing the recurrence interval ie weekly on every monday and tuesday, users can employ the `fibo-fnd-dt-fd:hasRecurrenceInterval` relationship along with an [`fibo-fnd-dt-fd:ExplicitRecurrenceInterval`](https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/ExplicitRecurrenceInterval) concept. This will have a corresponding ISO 8601 duration string literal linked by the `hasDurationValue` relationship. For example, daily = `P1D`, weekly = `P7D`, and alternate day = `P2D`. In the case where services must be delivered for multiple days within a week, additional `fibo-fnd-dt-bd:BusinessRecurrenceInterval` recurrence intervals can be instantiated to represent the day of week or day of month required. The `fibo-fnd-dt-fd:hasCount` property denotes the number of scheduled service occurrence and will end the agreement once all services have been delivered.

Figure 6: TBox representation of a regular schedule

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]] -. cmns-col:comprises .-> EventOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence]]
    StageOccurrence -- fibo-fnd-rel-rel:exemplifies --> ServiceExecutionStage[ontoservice:ServiceExecutionStage]
    EventOccurrence -- fibo-fnd-rel-rel:exemplifies --> Event[ontoservice:ServiceDeliveryEvent]
    ServiceExecutionStage -. cmns-col:comprises .-> Event
    StageOccurrence -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    StageOccurrence -. fibo-fnd-dt-fd:hasSchedule .-> Schedule[["<h4>fibo-fnd-dt-fd:RegularSchedule</h4><p style='font-size:0.75rem;'>fibo-fnd-dt-fd:hasCount &quot;xsd:integer&quot;</p>"]]:::literal
    Schedule -. fibo-fnd-dt-oc:hasOccurrence .-> EventOccurrence

    Schedule -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:ExplicitTimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[Time]]
    StartTime -.-> Time["<h4>cmns-dt:TimeOfDay</h4><p style='font-size:0.75rem;'>cmns-dt:hasTimeValue &quot;xsd:time&quot;</p>"]:::literal
    EndTime -.-> Time

    Schedule -. cmns-dt:hasStartDate .-> StartDate
    Schedule -. fibo-fnd-dt-fd:hasRecurrenceInterval .-> Recurrence[["<h4>fibo-fnd-dt-fd:ExplicitRecurrenceInterval</h4><p style='font-size:0.75rem;'>cmns-dt:hasDurationValue &quot;ISO8601_duration&quot;</p>"]]:::literal
    Schedule -. fibo-fnd-dt-fd:hasRecurrenceInterval .-> Recurrence[[fibo-fnd-dt-bd:BusinessRecurrenceInterval]]

    DayOfMonth["<h4>fibo-fnd-dt-bd:DayOfMonth</h4><p style='font-size:0.75rem;'>fibo-fnd-dt-fd:hasOrdinalNumber &quot;integer&quot;</p>"]:::literal --> Recurrence
    DayOfWeek[fibo-fnd-dt-bd:DayOfWeek] --> Recurrence
    Monday[[fibo-fnd-dt-fd:Monday]] -.-> DayOfWeek
    Tuesday[[fibo-fnd-dt-fd:Tuesday]] -.-> DayOfWeek
    Wednesday[[fibo-fnd-dt-fd:Wednesday]] -.-> DayOfWeek
    Thursday[[fibo-fnd-dt-fd:Thursday]] -.-> DayOfWeek
    Friday[[fibo-fnd-dt-fd:Friday]] -.-> DayOfWeek
    Saturday[[fibo-fnd-dt-fd:Saturday]] -.-> DayOfWeek
    Sunday[[fibo-fnd-dt-fd:Sunday]] -.-> DayOfWeek
```

#### Service Delivery Event

Each service delivery occurrence can be instantiated with the `ContractLifecycleEventOccurrence` concept, which must be assigned a specific date time and location. These occurrences will serve as a record to be analysed for quality, efficiency, and compliance with service agreements. Additionally, the occurrence of each service can be assigned a transport and/or a monetary charge if required.

Figure 7: TBox representation of the service agreement's service execution lifecycle stage

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]] -. cmns-col:comprises .-> EventOccurrence[["<h4>fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal
    StageOccurrence -- fibo-fnd-rel-rel:exemplifies --> ServiceExecutionStage[[ontoservice:ServiceExecutionStage]]
    EventOccurrence -- fibo-fnd-rel-rel:exemplifies --> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    ServiceExecutionStage -- cmns-col:comprises --> ServiceDeliveryEvent

    StageOccurrence -. fibo-fnd-dt-fd:hasSchedule .-> Schedule[[fibo-fnd-dt-fd:RegularSchedule]]
    Schedule -. fibo-fnd-dt-oc:hasOccurrence .-> EventOccurrence
    ServiceDeliveryEvent -.-> Event["<h4>fibo-fbc-pas-fpas:ContractLifecycleEvent</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;<br>rdfs:comment &quot;string&quot;</p>"]:::literal

    EventOccurrence -. fibo-fnd-plc-loc:isLocatedAt .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]
    EventOccurrence -. ontoservice:hasAssignedTransport .-> Vehicle[[vc:Vehicle]]
    EventOccurrence -. ontoservice:hasTotalPrice .-> TotalPrice[[ontoservice:TotalPrice]]
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
