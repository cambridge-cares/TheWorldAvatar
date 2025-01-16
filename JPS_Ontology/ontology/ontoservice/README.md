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
    - [2.3.1 Billing](#231-billing)

# 2. Data Model

## Legend

> Ontology Diagram

The representation of a class and instance is denoted by the node's shape. This means that an instance of the `Person` class will share the same label as the `Person` class itself, and they can only be distinguished by their shape. Literals are represented within the node of the class or instances. The label of instance nodes represent either a label for the purpose of explaining the diagram or the class it is `rdf:type` of (it is a class label if a prefix is available). Instance with labels will also be link to their respective class eg `prefix:Class` to clarify the instance. Multiple instance nodes for the same class in one diagram describes distinct instances of the class that is typically linked by different relations and instances.

```mermaid
flowchart TD
    instance[[Instance Label]]
    clazz[prefix:Class]
    clazzLiteral["<h4>prefix:Class</h4><p style='font-size:0.75rem;'>data property &quot;literal type&quot;</p>"]:::literal
```

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none

    %% Diagram
    subgraph equivalent instance with data property representation
    direction LR
    instanceDataProperty[["<h4>prefix:Class with data property</h4><p style='font-size:0.75rem;'>data property &quot;literal type&quot;</p>"]]:::literal
    instDataProperty[[Instance of Class Label]] -. rdf:type .-> instClazzLiteral["<h4>prefix:Class</h4><p style='font-size:0.75rem;'>data property &quot;literal type&quot;</p>"]:::literal
    end

    subgraph equivalent instance representation
    direction LR
    instClass[[prefix:Class]]
    instClassInst[[Instance of Class Label]] -. rdf:type .-> parentClass[prefix:Class]
    end
```

Relations between instances of one or more classes are indicated as dotted lines.

```mermaid
flowchart LR
    Inst[[Instance]] -. relation .-> rangeInst[[Instance]]
```

Relations linked to a class will also be applied to their instances.

```mermaid
flowchart LR
    subgraph equivalent relations for object properties
    direction LR
    multiinstance[[multiple class instance]] -.->  clazz2[prefix:Class]
    multiinstance -. relations .-> objInst2[[Object Instance]]

    instance[[multiple class instance]] -.->  clazz[prefix:Class]
    clazz -. relations .-> objInst[[Object Instance]]
    end
```

Unlabelled arrows references `rdfs:subClassOf` and `rdf:type` for solid and dotted lines respectively.

```mermaid
flowchart TB
    subgraph equivalent rdftype
    direction LR
    instance[[Instance]] -.-> clazz[prefix:Class]
    instance2[[Instance]] -. rdf:type .-> clazz2[prefix:Class]
    end

    subgraph equivalent subclassof for classes
    direction LR
    subclass[prefix:SubClass] --> superclass[prefix:SuperClass]
    subclass2[prefix:SubClass] -. rdfs:subClassOf .-> superclass2[prefix:SuperClass]
    end
```

```mermaid
flowchart LR
    subgraph equivalent subclassof for instances
    direction LR
    instanceSubClass[[prefix:SubClass]] ---> superclass1[prefix:SuperClass]
    inst[[Instance]] -.-> subClazz[prefix:SubClass]
    subClazz --> superclass2[prefix:SuperClass]
    end
```

> Namespace Prefix

| Prefix            | Namespace                                                                                        |
| ----------------- | ------------------------------------------------------------------------------------------------ |
| bot               | `https://w3id.org/bot#`                                                                          |
| cmns-cls          | `https://www.omg.org/spec/Commons/Classifiers/`                                                  |
| cmns-col          | `https://www.omg.org/spec/Commons/Collections/`                                                  |
| cmns-cxtdsg       | `https://www.omg.org/spec/Commons/ContextualDesignators/`                                        |
| cmns-doc          | `https://www.omg.org/spec/Commons/Documents/`                                                    |
| cmns-dsg          | `https://www.omg.org/spec/Commons/Designators/`                                                  |
| cmns-dt           | `https://www.omg.org/spec/Commons/DatesAndTimes/`                                                |
| cmns-pts          | `https://www.omg.org/spec/Commons/PartiesAndSituations/`                                         |
| cmns-qtu          | `https://www.omg.org/spec/Commons/QuantitiesAndUnits/`                                           |
| cmns-rlcmp        | `https://www.omg.org/spec/Commons/RolesAndCompositions/`                                         |
| fibo-fbc-fi-ip    | `https://spec.edmcouncil.org/fibo/ontology/FBC/FinancialInstruments/InstrumentPricing`           |
| fibo-fbc-pas-fpas | `https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/FinancialProductsAndServices` |
| fibo-fnd-acc-cur  | `https://spec.edmcouncil.org/fibo/ontology/FND/Accounting/CurrencyAmount/`                       |
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

The basis of this ontology revolves around the `fibo-fnd-pas-pas:ServiceAgreement` concept. The agreement specifies the requirements and terms of the service requested by clients.

The service agreement will define the duration, parties involved, requested service, and service location. The representation of the service location enables the association of facility with a specific geolocation for service delivery within the building or facility site as well as the contact person in charge at the location for the required service (See [OntoProfile](https://www.theworldavatar.com/kg/ontoprofile/)). Additional service details and remarks can also be attached to the `Service` concept when required. The available pricing models is described further in the [billing section](#231-billing).

Figure 1: TBox representation for a Service Agreement following the FIBO ontology

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[["<h4>fibo-fnd-pas-pas:ServiceAgreement</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;</p>"]]:::literal -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Start Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[End Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    Agreement -. fibo-fnd-rel-rel:governs .-> Service[["<h4>fibo-fnd-pas-pas:Service</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;<br>rdfs:comments &quot;remarks string&quot;</p>"]]:::literal
    Service -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:ExplicitTimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Start Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[End Time]]
    StartTime -.-> Time["<h4>cmns-dt:TimeOfDay</h4><p style='font-size:0.75rem;'>cmns-dt:hasTimeValue &quot;xsd:time&quot;</p>"]:::literal
    EndTime -.-> Time

    Agreement -. fibo-fnd-rel-rel:confers .-> PaymentObligation[[fibo-fnd-pas-psch:PaymentObligation]]
    PaymentObligation -. cmns-cxtdsg:appliesTo .-> Service
    PaymentObligation -. fibo-fnd-rel-rel:mandates .-> PricingModel[[fibo-fbc-fi-ip:PricingModel]]

    Agreement -. fibo-fnd-agr-ctr:hasContractParty .-> ServiceProvider[[fibo-fnd-pas-pas:ServiceProvider]]
    ServiceProvider -. cmns-rlcmp:isPlayedBy .-> ProviderOrg[[Service Provider Organization]]
    ProviderOrg -.-> Org
    ServiceProvider -. fibo-fnd-rel-rel:provides .-> Service
    Agreement -. fibo-fnd-arr-rep:isRequestedBy .-> Client[[fibo-fnd-pas-pas:Client]]
    Client -. cmns-rlcmp:isPlayedBy .-> ClientOrg[[Client Organization]]
    ClientOrg -.-> Org[fibo-fnd-org-fm:FormalOrganization]

    Service -. fibo-fnd-rel-rel:provides .-> Capability[[fibo-fnd-plc-fac:Capability]]
    Capability -. fibo-fnd-rel-rel:involves .-> Facility[[ontobim:Facility]]
    Org -. fibo-fnd-rel-rel:controls .-> Facility

    Facility -. ontoservice:hasServiceLocation .-> Location[Service Location]
    Building[[bot:Building]] -. ontobim:hasFacility .-> Facility
    Building -. ontoservice:hasServiceLocation .-> Location
    Location --> Feature[geo:Feature]
    Location --> PhysicalLocation[fibo-fnd-plc-loc:PhysicalLocation]
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

## 2.2. Service Agreement Lifecycle

A generic lifecycle, comprising of stages and their events, will be represented and instantiated once and reused across all agreement instances. The contract lifecycle usually consists of three stages in sequence of creation, service execution, and expiration. It is recommended to instantiate a `cmns-dt:succeeds` relationship between these three stages as seen in the figure below. Events occurring within each stages are also instantiated and assigned using the `cmns-col:comprises` property. The recommended lifecycle triples are also available as `TTL` format in this directory's `abox.ttl` file.

Figure 3: TBox representation of the service contract lifecycle

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Lifecycle[[fibo-fbc-pas-fpas:ContractLifecycle]] -. fibo-fnd-arr-lif:hasStage .-> Stage[[ontoservice:ServiceAgreementLifecycleStage]]
    Stage -. cmns-col:comprises .-> Event[[ontoservice:ServiceAgreementLifecycleEvent]]

    CreationStage[[ontoservice:CreationStage]] --> Stage
    ServiceExecutionStage[[ontoservice:ServiceExecutionStage]] --> Stage
    ExpirationStage[[ontoservice:ExpirationStage]] --> Stage
    ServiceExecutionStage -. cmns-dt:succeeds .-> CreationStage
    ExpirationStage -. cmns-dt:succeeds .-> ServiceExecutionStage

    CreationStage -. cmns-col:comprises .-> ContractCreation[[ontoservice:ContractCreation]]
    CreationStage -. cmns-col:comprises .-> ContractApproval[[ontoservice:ContractApproval]]
    ContractApproval -. cmns-dt:succeeds .-> ContractCreation
    ContractCreation --> Event
    ContractApproval --> Event

    ServiceExecutionStage -. cmns-col:comprises .-> OrderReceivedEvent[[ontoservice:OrderReceivedEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> ServiceDispatchEvent[[ontoservice:ServiceDispatchEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> IncidentReportEvent[[ontoservice:IncidentReportEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> TerminatedServiceEvent[[ontoservice:TerminatedServiceEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]]
    ServiceDispatchEvent -. cmns-dt:succeeds .-> OrderReceivedEvent
    ServiceDeliveryEvent -. cmns-dt:succeeds .-> ServiceDispatchEvent
    CalculationEvent -. cmns-dt:succeeds .-> ServiceDeliveryEvent
    IncidentReportEvent -. cmns-dt:succeeds .-> ServiceDeliveryEvent
    OrderReceivedEvent --> Event
    ServiceDispatchEvent --> Event
    ServiceDeliveryEvent --> Event
    IncidentReportEvent --> Event
    TerminatedServiceEvent --> Event

    ExpirationStage -. cmns-col:comprises .-> ContractDischarge[[ontoservice:ContractDischarge]]
    ExpirationStage -. cmns-col:comprises .-> ContractRescission[[ontoservice:ContractRescission]]
    ExpirationStage -. cmns-col:comprises .-> ContractTermination[[ontoservice:ContractTermination]]
    ContractDischarge --> Event
    ContractRescission --> Event
    ContractTermination --> Event
```

In the creation stage, the service agreement will need to be created before it is approved, as represented by the `ContractCreation` and `ContractApproval` events.

During the service execution stage, the sequence of events should occur in the following manner during a successful delivery. It should be noted that the event may be completed with either a `CalculationEvent`, `IncidentReportEvent`, or `TerminatedServiceEvent`. The `Terminated Service Event` represents the termination of an upcoming service either by the service provider or the client, which may occur at any time after the first event.

1. `Order Received Event`: When a new service order is received and acknowledged by the system after the approval
2. `Service Dispatch Event`: Assignment of service personnel, resources, and/or location(s) to perform the requested service
3. `Service Delivery Event`: Delivery of the requested service
4. `Calculation Event`: Records a summary of the service trip, tailored to the specific domain; multiple calculation events can be instantiated if multiple calculations/quantities are reported
   - `Incident Report Event`: An alternate possible event in which an incident occurred during the service delivery, resulting in the failure to complete

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

Events in the creation and expiration stage are expected to occur once and we recommend to generate one instance of `ContractLifecycleEventOccurrence` with a specific date time and remarks if required. If the service is terminated or faced an incident, it is recommended to instantiate a new occurrence for the corresponding event with the required time stamp and reason for its occurrence (`rdfs:comment`). Moreover, `EventStatus` can be used to describe the status of specific event occurrences, namely, whether the event is pending action, in progress, or completed.

Figure 4: TBox representation of the service agreement's occurrences within a service contract lifecycle

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

    LifecycleOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Lifecycle[[fibo-fbc-pas-fpas:ContractLifecycle]]
    StageOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Stage[[ontoservice:ServiceAgreementLifecycleStage]]
    EventOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Event[[ontoservice:ServiceAgreementLifecycleEvent]]

    Lifecycle -. fibo-fnd-arr-lif:hasStage .-> Stage
    Stage -. cmns-col:comprises .-> Event

    LifecycleOccurrence --> Occurrence["<h4>fibo-fnd-dt-oc:Occurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]:::literal
    StageOccurrence --> Occurrence
    EventOccurrence --> Occurrence
    Occurrence -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Start Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[End Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    EventStatus[[ontoservice:EventStatus]] -. cmns-dsg:describes .-> EventOccurrence
```

### 2.2.2 Service Execution Stage

This stage governs the delivery of the requested service stated in the service agreement.

#### Regular Schedule

In representing the upcoming service delivery occurrences, a regular schedule is assigned to the stage occurrence. The start date of the schedule must follow the `StartDate` instance linked to the service execution lifecycle stage. The representation of the requested service time employs the `cmns-dt:ExplicitTimePeriod` concept via the `cmns-dt:hasTimePeriod` relationship. In representing the recurrence interval ie weekly on every monday and tuesday, users can employ the `fibo-fnd-dt-fd:hasRecurrenceInterval` relationship along with an [`fibo-fnd-dt-fd:ExplicitRecurrenceInterval`](https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/ExplicitRecurrenceInterval) concept. This will have a corresponding ISO 8601 duration string literal linked by the `hasDurationValue` relationship. For example, daily = `P1D`, weekly = `P7D`, and alternate day = `P2D`. In the case where services must be delivered for multiple days within a week, additional `fibo-fnd-dt-bd:BusinessRecurrenceInterval` recurrence intervals can be instantiated to represent the day of week or day of month required. The `fibo-fnd-dt-fd:hasCount` property denotes the number of scheduled service occurrence and will end the agreement once all services have been delivered.

Figure 5: TBox representation of a regular schedule

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]] -. cmns-col:comprises .-> EventOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence]]
    StageOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceExecutionStage[[ontoservice:ServiceExecutionStage]]
    EventOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Event[[ontoservice:ServiceDeliveryEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> Event
    StageOccurrence -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Service Start Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[Service End Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    StageOccurrence -. fibo-fnd-dt-fd:hasSchedule .-> Schedule[["<h4>fibo-fnd-dt-fd:RegularSchedule</h4><p style='font-size:0.75rem;'>fibo-fnd-dt-fd:hasCount &quot;xsd:integer&quot;</p>"]]:::literal
    Schedule -. fibo-fnd-dt-oc:hasOccurrence .-> EventOccurrence

    Schedule -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:ExplicitTimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Start Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[End Time]]
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

#### Successful Service Delivery

The typical sequence of events for a successful service delivery is depicted in the figure below. Each event's occurrence can be instantiated with the `ContractLifecycleEventOccurrence` concept, which must be assigned a specific date, time, and location (if required). The process begins with the `OrderReceivedEvent`, which kickstarts the workflow. The next event is the `ServiceDispatchEvent`, where users can assign resources, personnel, and locations to specific orders. Personnel can be assigned using the `fibo-fnd-rel-rel:designates` relation and `fibo-fnd-org-fm:Employee` subclasses, while resources (such as equipment `saref:Device` or facility `ontobim:Facility`) can be assigned using the `fibo-fnd-rel-rel:involves` relation. For example, a driver can be designated for the delivery, and their assigned transport and other details can be tracked as described in [`OntoProfile`](https://www.theworldavatar.com/kg/ontoprofile/). Please do note that while the delivery typically occurs at the service site, some deliveries required an additional destination at a separate facility.

Following this, the `ServiceDeliveryEvent` occurs when the services are executed. Users can supplement information on any exchange of assets or equipment using the `fibo-fnd-rel-rel:exchanges` relation. Once the service is delivered, users can log any relevant information with the subsequent `CalculationEvent`, such as price, weight, distance, etc. These occurrences will serve as a record to be analysed for quality, efficiency, and compliance with service agreements.

It is recommended that the `EventStatus` concept is only used to describe the status of each event occurrence for both a `ServiceDispatchEvent` and `ServiceDeliveryEvent`. A dispatch event may have either pending or completed statuses, whereas a delivery event may be in the pending, in progress, or completed states.

Figure 6: TBox representation of a successful service delivery lifecycle

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]] -. fibo-fnd-rel-rel:exemplifies .-> ServiceExecutionStage[[ontoservice:ServiceExecutionStage]]

    StageOccurrence -. cmns-col:comprises .-> OrderReceivedOccurrence[[OrderReceivedOccurrence]]
    OrderReceivedOccurrence -. fibo-fnd-rel-rel:exemplifies .-> OrderReceivedEvent[[ontoservice:OrderReceivedEvent]]
    OrderReceivedOccurrence -.-> EventOccurrence["<h4>fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]:::literal

    StageOccurrence -. cmns-col:comprises .-> DispatchOccurrence[[ServiceDispatchOccurrence]]
    DispatchOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDispatchEvent[[ontoservice:ServiceDispatchEvent]]
    DispatchOccurrence -.-> EventOccurrence
    DispatchOccurrence -. cmns-dt:succeeds .-> OrderReceivedOccurrence
    DispatchEventStatus[[ontoservice:EventStatus]] -. cmns-dsg:describes .-> DispatchOccurrence

    StageOccurrence -. cmns-col:comprises .-> DeliveryOccurrence[["<h4>DeliveryOccurrence</h4><p style='font-size:0.75rem;'>ontoservice:hasScheduledTime &quot;xsd:time&quot;</p>"]]:::literal
    DeliveryOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    DeliveryOccurrence -.-> EventOccurrence
    DeliveryOccurrence -. cmns-dt:succeeds .-> DispatchOccurrence
    DeliveryEventStatus[[ontoservice:EventStatus]] -. cmns-dsg:describes .-> DeliveryOccurrence

    StageOccurrence -. cmns-col:comprises .-> Calculation[["<h4>fibo-fnd-dt-oc:Calculation</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal
    CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]] -. cmns-cls:classifies .-> Calculation
    Calculation -. cmns-dt:succeeds .-> DeliveryOccurrence

    StageOccurrence -. fibo-fnd-dt-fd:hasSchedule .-> Schedule[[fibo-fnd-dt-fd:RegularSchedule]]
    Schedule -. fibo-fnd-dt-oc:hasOccurrence .-> DeliveryOccurrence
    DispatchOccurrence -. fibo-fnd-rel-rel:designates .-> Driver[[ontoprofile:EmployedDriver]]
    DispatchOccurrence -. fibo-fnd-rel-rel:involves .-> AssignableThing[[No restrictions: Thing]]
    DeliveryOccurrence -. fibo-fnd-rel-rel:exchanges .->  ExchangeableThing[[No restrictions: Thing]]
    EventOccurrence -. fibo-fnd-plc-loc:isLocatedAt .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]
```

#### Calculation

Once the service is completed, it is expected that the user will log certain values, which serves as inputs for some form of calculation such as collection weight or distance travelled. The calculation have an associated expression, that may ingest any constant or variable values. Outputs are represented via the `cmns-qtu:hasQuantityValue`. Multiple calculations can be instantiated per delivery for different measures, and will typically be reported in at least one report. For more information on reporting representation, please read the [next section](#23-reporting).

Figure 7: TBox representation of a calculation during the service lifecycle

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]] -- cmns-cls:classifies --> Calculation[["<h4>fibo-fnd-dt-oc:Calculation</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal

    Calculation -. cmns-qtu:hasQuantityValue .-> OutputValue[[Output]]
    Calculation -. cmns-qtu:hasExpression .-> Expression[[cmns-qtu:Expression]]
    Expression -. cmns-qtu:hasArgument .-> Constant[[cmns-qtu:Constant]]
    Expression -. cmns-qtu:hasArgument .-> Variable[[cmns-qtu:Variable]]
    OutputValue --> Input[cmns-qtu:ScalarQuantityValue]
    Constant --> Input
    Variable --> Input
```

## 2.3 Reporting

In reporting the services delivered as per the service agreement, a `Report` reports on the individual service occurrence via the `Record` concept. Given that an agreement may have multiple service delivery dates, multiple records can be instantiated per report. These records record a value, that may be directly or indirectly computed from the measures logged upon the successful completion of the service. In the example below, the record directly records the output of the calculation event.

Figure 8: TBox representation of a report for a service agreement

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Report[[fibo-fnd-arr-rep:Report]] -. fibo-fnd-arr-rep:hasReportDate .-> Date[["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]]:::literal
    Report -. fibo-fnd-rel-rel:isProvidedBy .-> ReportingParty[[fibo-fnd-arr-rep:ReportingParty]]
    Report -. cmns-doc:isAbout .-> Agreement[[fibo-fnd-pas-pas:ServiceAgreement]]
    Report -. fibo-fnd-arr-rep:isReportedTo .-> Client

    Agreement -. fibo-fnd-arr-rep:isRequestedBy .-> Client[[fibo-fnd-pas-pas:Client]]
    Agreement -. fibo-fnd-agr-ctr:hasContractParty .-> ServiceProvider[[fibo-fnd-pas-pas:ServiceProvider]]
    ServiceProvider -. cmns-rlcmp:isPlayedBy .-> Org[[fibo-fnd-org-fm:FormalOrganization]]
    ReportingParty -. cmns-rlcmp:isPlayedBy .-> Org

    Agreement -. fibo-fnd-arr-lif:hasLifecycle .-> LifecycleOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleOccurrence]]
    LifecycleOccurrence -. fibo-fnd-arr-lif:hasStage .-> StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]]
    StageOccurrence -. cmns-col:comprises .-> DeliveryOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence]]
    DeliveryOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    StageOccurrence -. cmns-col:comprises .-> Calculation[["<h4>fibo-fnd-dt-oc:Calculation</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal
    Calculation -. cmns-qtu:hasQuantityValue .-> OutputValue[[cmns-qtu:ScalarQuantityValue]]
    CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]] -- cmns-cls:classifies --> Calculation

    Report -. fibo-fnd-arr-rep:reportsOn .-> Record[[cmns-doc:Record]]
    Record -. cmns-doc:isAbout .-> DeliveryOccurrence
    Record -. cmns-doc:records .-> OutputValue
```

### 2.3.1 Billing

The billable amount for each service delivery is recorded as a new `CalculatedPrice` instance, derived from a pricing model and its inputs, along with any additional inputs. These additional inputs may come from a calculation event or other sources. The pricing model should be stipulated as part of the service agreement. Note that the variable fee must use **price per quantity** as a measurement unit. In the example below, price per tonne is used, and these extensions can be made in the `abox.ttl`.

Figure 9: TBox representation of a billing record for a service agreement

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[[fibo-fnd-pas-pas:ServiceAgreement]] -. fibo-fnd-rel-rel:confers .-> PaymentObligation[[fibo-fnd-pas-psch:PaymentObligation]]
    PaymentObligation -. fibo-fnd-rel-rel:mandates .-> PricingModel[[fibo-fbc-fi-ip:PricingModel]]

    DeliveryOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence]] -. fibo-fnd-rel-rel:exemplifies .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    Record[[cmns-doc:Record]] -. cmns-doc:isAbout .-> DeliveryOccurrence
    Record -. cmns-doc:records .-> ServiceFee[[fibo-fnd-acc-cur:CalculatedPrice]]

    ServiceFee -. cmns-cxtdsg:uses .-> PricingModel[[fibo-fbc-fi-ip:PricingModel]]
    ServiceFee -. cmns-qtu:hasExpression .-> CalculationExpression[[cmns-qtu:Expression]]

    PricingModel -. cmns-qtu:hasArgument .-> FlatFee[[Flat Fee]]
    FlatFee -.-> MonetaryPrice["<h4>fibo-fnd-acc-cur:MonetaryPrice</h4><p style='font-size:0.75rem;'>fibo-fnd-acc-cur:hasAmount &quot;xsd:decimal&quot;</p>"]:::literal

    PricingModel -. cmns-qtu:hasArgument .-> VariableFee[[ontoservice:VariableFee]]
    VariableFee -.-> UnitPrice[fibo-fnd-acc-cur:UnitPrice]
    UnitPrice -.-> MonetaryPrice
    VariableFee -. cmns-qtu:hasLowerBound .-> MinQuantityValue[[cmns-qtu:ScalarQuantityValue]]
    VariableFee -. cmns-qtu:hasUpperBound .-> MaxQuantityValue[[cmns-qtu:ScalarQuantityValue]]
    VariableFee -. cmns-qtu:hasMeasurementUnit .-> pricepertonne[[ontoservice:pricePerTonne]]

    CalculationExpression -. cmns-qtu:hasArgument .-> FlatFee
    CalculationExpression -. cmns-qtu:hasArgument .-> Output[[cmns-qtu:ScalarQuantityValue]]
    CalculationExpression -. cmns-qtu:hasArgument .-> VariableFee

    Calculation[["<h4>fibo-fnd-dt-oc:Calculation</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal -. cmns-qtu:hasQuantityValue .-> Output
    CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]] -. cmns-cls:classifies .-> Calculation
```

The representation of the pricing model is intended to be highly flexible to accommadate different types of pricing models such as:

1. **Flat fee pricing model**: A fixed fee regardless of the service details - Instantiate a flat fee argument with ONLY _ONE_ monetary price instance
2. **Fixed trip variable weight pricing model**: A fixed delivery charge and a variable fee depending on the weight collected/delivered - Instantiate _ONE_ flat fee argument along with _ONE_ variable fee argument with no bounds
3. **Variable excess weight pricing model**: A fixed fee up to a weight cap and a variable fee depending on the excess weight above the cap - Instantiate _ONE_ flat fee argument for the fixed fee; _ONE_ variable fee argument of `0` rate from lower and upper bounds of `0` and `weight cap` respectively; _ONE_ variable fee argument with ONLY lower bounds of the `weight cap`

Figure 10: TBox representation of potential pricing models

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    FlatFeePricingModel[[Flat Fee Pricing Model]] -. cmns-qtu:hasArgument .-> FlatFee[[Flat Fee]]

    FixedTripVariableWeightPricingModel[[Fixed Trip Variable Weight Pricing Model]] -. cmns-qtu:hasArgument .-> FixedTripFee[[Fixed Trip Fee]]
    FixedTripVariableWeightPricingModel -. cmns-qtu:hasArgument .-> VariableWeightFee[[Variable Weight Fee]]

    VariableExcessWeightPricingModel[[Variable Excess Weight Pricing Model]] -. cmns-qtu:hasArgument .-> CappedBaseFee[[Fixed Fee Up To Weight Cap]]
    VariableExcessWeightPricingModel -. cmns-qtu:hasArgument .-> VariableWeightBaseFee[["<h4>Variable Base Fee Before Weight Cap</h4><p style='font-size:0.75rem;'>fibo-fnd-acc-cur:hasAmount &quot;0&quot;</p>"]]:::literal
    VariableWeightBaseFee -. cmns-qtu:hasLowerBound .-> ZeroQuantity[["<h4>Zero Quantity Value</h4><p style='font-size:0.75rem;'>cmns-qtu:hasNumericValue &quot;0&quot;</p>"]]:::literal
    VariableWeightBaseFee -. cmns-qtu:hasUpperBound .-> WeightCap[["<h4>Weight Cap Value</h4><p style='font-size:0.75rem;'>cmns-qtu:hasNumericValue &quot;weight cap&quot;</p>"]]:::literal
    VariableExcessWeightPricingModel -. cmns-qtu:hasArgument .-> VariableExcessWeightFee[[Variable Excess Weight Fee]]
    VariableExcessWeightFee -. cmns-qtu:hasLowerBound .-> WeightCap

    ScalarQuantityValue["<h4>cmns-qtu:ScalarQuantityValue</h4><p style='font-size:0.75rem;'>cmns-qtu:hasNumericValue &quot;xsd:decimal&quot;</p>"]:::literal -. cmns-qtu:hasMeasurementUnit .-> tonne[[ontoservice:tonne]]
    ZeroQuantity -.-> ScalarQuantityValue
    WeightCap -.-> ScalarQuantityValue

    FlatFee -.-> MonetaryPrice["<h4>fibo-fnd-acc-cur:MonetaryPrice</h4><p style='font-size:0.75rem;'>fibo-fnd-acc-cur:hasAmount &quot;xsd:decimal&quot;</p>"]:::literal
    FixedTripFee -.-> MonetaryPrice
    CappedBaseFee -.-> MonetaryPrice
    VariableWeightFee -.-> VariableFee[ontoservice:VariableFee]
    VariableWeightBaseFee -.-> VariableFee
    VariableExcessWeightFee -.-> VariableFee
    VariableFee -. cmns-qtu:hasMeasurementUnit .-> pricepertonne[[ontoservice:pricePerTonne]]
    VariableFee -.-> MonetaryPrice
```
