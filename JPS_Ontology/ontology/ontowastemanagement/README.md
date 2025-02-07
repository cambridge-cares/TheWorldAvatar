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
    - [2.2.1 Service Lifecycle](#221-service-lifecycle)
    - [2.2.2 Waste Categories](#222-waste-categories)
  - [2.3 Assets](#23-assets)
    - [2.3.1 Truck](#231-truck)
    - [2.3.2 Bin](#232-bin)

# 2. Data model

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

Relations between instances of one or more classes are indicated as dotted lines. Solid lines indicate that the range of the relation **MUST** be a class, and **NOT** an instance.

```mermaid
flowchart LR
    Inst[[Instance]] -. relation .-> rangeInst[[Instance]]
    domainInst[[Instance]] -- relation --> rangeClazz[prefix:Class]
```

Relations linked to a class will also be applied to their instances.

```mermaid
flowchart LR
    subgraph equivalent relations for object properties
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

| Prefix            | Namespace                                                                                         |
| ----------------- | ------------------------------------------------------------------------------------------------- |
| cmns-col          | `https://www.omg.org/spec/Commons/Collections/`                                                   |
| cmns-cxtdsg       | `https://www.omg.org/spec/Commons/ContextualDesignators/`                                         |
| cmns-dt           | `https://www.omg.org/spec/Commons/DatesAndTimes/`                                                 |
| cmns-dsg          | `https://www.omg.org/spec/Commons/Designators/`                                                   |
| cmns-pts          | `https://www.omg.org/spec/Commons/PartiesAndSituations/`                                          |
| cmns-qtu          | `https://www.omg.org/spec/Commons/QuantitiesAndUnits/`                                            |
| cmns-rlcmp        | `https://www.omg.org/spec/Commons/RolesAndCompositions/`                                          |
| fibo-fbc-fi-ip    | `https://spec.edmcouncil.org/fibo/ontology/FBC/FinancialInstruments/InstrumentPricing`            |
| fibo-fbc-pas-fpas | `https://spec.edmcouncil.org/fibo/ontology/FBC/ProductsAndServices/FinancialProductsAndServices/` |
| fibo-fnd-acc-cur  | `https://spec.edmcouncil.org/fibo/ontology/FND/Accounting/CurrencyAmount/`                        |
| fibo-fnd-agr-ctr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/`                             |
| fibo-fnd-arr-rep  | `https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/`                           |
| fibo-fnd-aap-ppl  | `https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/`                           |
| fibo-fnd-dt-bd    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/BusinessDates/`                      |
| fibo-fnd-dt-fd    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/`                     |
| fibo-fnd-dt-oc    | `https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/`                        |
| fibo-fnd-pas-pas  | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/`          |
| fibo-fnd-pas-psch | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/`         |
| fibo-fnd-plc-adr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/`                                 |
| fibo-fnd-plc-fac  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Facilities/`                                |
| fibo-fnd-plc-loc  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/`                                 |
| fibo-fnd-pty-pty  | `https://spec.edmcouncil.org/fibo/ontology/FND/Parties/Parties/`                                  |
| fibo-fnd-rel-rel  | `https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/`                              |
| fibo-fnd-org-fm   | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/`                |
| fibo-fnd-utl-alx  | `https://spec.edmcouncil.org/fibo/ontology/FND/Utilities/Analytics/`                              |
| fibo-loan-ln-ln   | `https://spec.edmcouncil.org/fibo/ontology/LOAN/LoansGeneral/Loans/`                              |
| lcc-cr            | `https://www.omg.org/spec/LCC/Countries/CountryRepresentation/`                                   |
| om                | `http://www.ontology-of-units-of-measure.org/resource/om-2/`                                      |
| sf                | `http://www.opengis.net/ont/sf#`                                                                  |
| geo               | `http://opengis.net/ont/geosparql#`                                                               |
| time              | `https://www.w3.org/TR/owl-time/#time`                                                            |
| vcard             | `https://www.w3.org/2006/vcard/ns`                                                                |
| vc                | `https://spec.edmcouncil.org/auto/ontology/VC/VehicleCore/`                                       |
| ontobim           | `https://www.theworldavatar.com/kg/ontobim/`                                                      |
| ontoderivation    | `https://www.theworldavatar.com/kg/ontoderivation/`                                               |
| ontoprofile       | `https://www.theworldavatar.com/kg/ontoprofile/`                                                  |
| ontoservice       | `https://www.theworldavatar.com/kg/ontoservice/`                                                  |
| ontowm            | `https://www.theworldavatar.com/kg/ontowastemanagement/`                                          |

## 2.1. Service agreement

The basis of this ontology revolves around the `fibo-fnd-pas-pas:ServiceAgreement` concept to specify the agreed upon service requirements and terms. The general usage of this concept can be found as part of the [OntoService](https://www.theworldavatar.com/kg/ontoservice/) ontology, including its duration, involved parties, payment obligations, and commitments, as well as the organisation profiles in the [OntoProfile](https://www.theworldavatar.com/kg/ontoprofile/) ontology. Please refer to [OntoService](../ontoservice#23-reporting) for any reporting or billing matters. This ontology specifically demonstrates the extension of `OntoService` and `OntoProfile` to the waste operation industry.

Figure 1: TBox representation for a Service Agreement in the waste management sector following the FIBO ontology

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

    Agreement -. fibo-fnd-rel-rel:governs .-> Service[["<h4>ontowm:WasteService</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;<br>rdfs:comments &quot;remarks string&quot;</p>"]]:::literal
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

Furthermore, the service will be given a contact person, who is an employee of the client. This person will play a role of a service provider who provides a contact service for the service specified in the agreement.

Figure 2: TBox representation for the client's point of contact (contact service) for the service following the FIBO ontology

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[["<h4>fibo-fnd-pas-pas:ServiceAgreement</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;</p>"]]:::literal -. fibo-fnd-rel-rel:governs .-> Service[[ontowm:WasteService]]
    Agreement -. fibo-fnd-arr-rep:isRequestedBy .-> Client[[fibo-fnd-pas-pas:Client]]
    Employer[[fibo-fnd-org-fm:Employer]] -. cmns-rlcmp:isPlayedBy .-> Org[[fibo-fnd-org-fm:FormalOrganization]]
    Client -. cmns-rlcmp:isPlayedBy .-> Org

    Employee[[fibo-fnd-org-fm:Employee]] -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    Employee -. fibo-fnd-org-fm:isEmployeeOf .-> Employer
    ServiceProvider[[fibo-fnd-pas-pas:ServiceProvider]] -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    ServiceProvider -. fibo-fnd-rel-rel:provides .-> ContactService[[ontoservice:ContactService]]
    ContactService -. ontoservice:servesAsContactFor .-> Service
```

## 2.2. Waste services

The services available in the waste operation industry are as follows:

1. **Bin delivery service**: Delivery of an empty bin to site. Must be assigned to a hooklift truck
2. **Bin exchange service** `WasteCollectionService`: Delivery of an empty bin to site while towing away the existing full bin. Must be assigned to a hooklift truck
3. **Tow away service** `WasteCollectionService`: Collects the bin containing waste from the service site and dispose the waste at the disposal site without returning the bin afterwards to the service site. Must be assigned to a hooklift truck
4. **Touch and go service** `WasteCollectionService`: Brings an empty bin to collect and dispose of the waste on the service site without returning the bin afterwards. Must be assigned to a hooklift truck
5. **Dump and return service** `WasteCollectionService`: An empty truck will collect the bin containing waste from the service site, and dispose the waste at the disposal site before returning the newly emptied bin afterwards. Must be assigned to a hooklift truck

When setting up the agreement, the (sub)classes of the bin and waste categories can be assigned through their corresponding relationships with the service. Notably, waste categories can only be assigned to waste collection services, where waste is collected and disposed. The `isRecyclable` property is assignable to the waste if the service collects recyclable waste. The disposal of waste must occur at specific waste disposal facilities, which will be represented with their locations, operating hours, providers, and waste categories.

Figure 3: TBox representation of waste services

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[["<h4>fibo-fnd-pas-pas:ServiceAgreement</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;</p>"]]:::literal -. fibo-fnd-rel-rel:governs .-> Service[["<h4>ontowm:WasteService</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;<br>rdfs:comments &quot;remarks string&quot;</p>"]]:::literal

    BinDeliveryService[ontowm:BinDeliveryService] --> Service
    WasteCollectionService[ontowm:WasteCollectionService]:::literal --> Service

    Service -- ontowm:hasBinType --> BinClazz[ontowm:Bin]
    WasteCollectionService -- ontowm:hasWasteType --> WasteClazz["<h4>ontowm:Waste</h4><p style='font-size:0.75rem;'>ontowm:isRecyclable &quot;boolean&quot;</p>"]

    ServiceProvider[[fibo-fnd-pas-pas:ServiceProvider]] -. fibo-fnd-rel-rel:provides .-> WasteDisposalService[[ontowm:WasteDisposalService]]
    WasteDisposalService -. fibo-loan-ln-ln:hasCost .-> MonetaryPrice["<h4>fibo-fnd-acc-cur:MonetaryPrice</h4><p style='font-size:0.75rem;'>fibo-fnd-acc-cur:hasAmount &quot;xsd:decimal&quot;</p>"]:::literal
    WasteDisposalService -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:ExplicitTimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Start Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[End Time]]
    StartTime -.-> Time["<h4>cmns-dt:TimeOfDay</h4><p style='font-size:0.75rem;'>cmns-dt:hasTimeValue &quot;xsd:time&quot;</p>"]:::literal
    EndTime -.-> Time

    WasteDisposalService -. fibo-fnd-rel-rel:provides .-> Capability[[fibo-fnd-plc-fac:Capability]]
    Capability -- fibo-fnd-rel-rel:involves --> WasteClazz

    ServiceProvider -. fibo-fnd-pas-pas:provisions .-> WasteDisposalFacility[[ontowm:WasteDisposalFacility]]
    WasteDisposalFacility -. fibo-fnd-plc-loc:isLocatedAt .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]
    WasteDisposalFacility -. fibo-fnd-rel-rel:provides .-> Capability
    WasteDisposalFacility --> ontobim:Facility
```

### 2.2.1 Service Lifecycle

In monitoring the services rendered during the lifecycle of a service agreement, the [OntoService](https://www.theworldavatar.com/kg/ontoservice/) ontology can describe, represent, and generate the occurences of the lifecycle, stages, and events according to the real-time occurrences of the service delivered. A comprehensive description of the lifecycle representation is available at [section 2.2 of OntoService](../ontoservice#22-service-agreement-lifecycle). Briefly, each service agreement's lifecycle should be recorded with their own set of occurrence instances. These instances serves as a record to be analysed for quality, efficiency, and compliance with service agreements. Each occurrence can either holds during a date period or occur at an instantaneous time. During the service execution stage, there may be incidents or terminated service events, that can be represented with the corresponding occurrence and descriptions/comments if they were to occur.

Figure 4: TBox representation for the service agreement's overall lifecycle

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Agreement[[fibo-fnd-pas-pas:ServiceAgreement]]:::literal -. fibo-fnd-arr-lif:hasLifecycle .-> LifecycleOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleOccurrence]]
    LifecycleOccurrence -. fibo-fnd-arr-lif:hasStage .-> StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]]
    StageOccurrence -. cmns-col:comprises .-> EventOccurrence[["<h4>fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal

    LifecycleOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Lifecycle[[fibo-fbc-pas-fpas:ContractLifecycle]]
    StageOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Stage[[ontoservice:ServiceAgreementLifecycleStage]]
    EventOccurrence -. fibo-fnd-rel-rel:exemplifies .-> Event[[fibo-fbc-pas-fpas:ContractLifecycleEvent]]
    Lifecycle -. fibo-fnd-arr-lif:hasStage .-> Stage
    Stage -. cmns-col:comprises .-> Event

    CreationStage[[ontoservice:CreationStage]] --> Stage
    ServiceExecutionStage[[ontoservice:ServiceExecutionStage]] --> Stage
    ExpirationStage[[ontoservice:ExpirationStage]] --> Stage
    ServiceExecutionStage -. cmns-dt:succeeds .-> CreationStage
    ExpirationStage -. cmns-dt:succeeds .-> ServiceExecutionStage

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
```

When the service agreement is initially drafted, upcoming scheduled service events are represented within a regular schedule. For more information on regular schedules, please refer to [the corresponding section in the OntoService](../ontoservice/README.md#regular-schedule) ontology. Users can dispatch drivers and resources such as the bin(s) and waste disposal site during the `ServiceDispatchEvent` phase. For each successful delivery occurrence of the requested service, a `ContractLifecycleEventOccurrence` instance is linked to both the schedule and `ServiceDeliveryEvent` instances. This occurrence usually represents the collection and/or exchange of bins from the service site that occurs within a time period. A bin that was collected/exchanged during a bin exchange service can be represented using the `fibo-fnd-rel-rel:exchanges` relation.

Figure 5: TBox representation of a service dispatch event occurrence for waste services

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]] -- fibo-fnd-rel-rel:exemplifies --> ServiceExecutionStage[[ontoservice:ServiceExecutionStage]]
    ServiceExecutionStage -. cmns-col:comprises .-> OrderReceivedEvent[[ontoservice:OrderReceivedEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> ServiceDispatchEvent[[ontoservice:ServiceDispatchEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]]
    ServiceDispatchEvent -. cmns-dt:succeeds .-> OrderReceivedEvent
    ServiceDeliveryEvent -. cmns-dt:succeeds .-> ServiceDispatchEvent
    CalculationEvent -. cmns-dt:succeeds .-> ServiceDeliveryEvent

    StageOccurrence -. cmns-col:comprises .-> DispatchOccurrence[[ServiceDispatchOccurrence]]
    DispatchOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDispatchEvent
    DispatchOccurrence -.-> EventOccurrence

    DispatchEventStatus[[ontoservice:EventStatus]] -. cmns-dsg:describes .-> DispatchOccurrence
    DispatchOccurrence -. fibo-fnd-rel-rel:designates .-> Driver[[ontoprofile:EmployedDriver]]
    DispatchOccurrence -. fibo-fnd-rel-rel:involves .-> Bin[[ontowm:Bin]]
    DispatchOccurrence -. fibo-fnd-rel-rel:involves .-> Facility[[ontowm:WasteDisposalFacility]]
    Facility -. fibo-fnd-plc-loc:isLocatedAt .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]

    StageOccurrence -. cmns-col:comprises .-> OrderReceivedOccurrence[[OrderReceivedOccurrence]]
    OrderReceivedOccurrence -. fibo-fnd-rel-rel:exemplifies .-> OrderReceivedEvent
    OrderReceivedOccurrence -.-> EventOccurrence["<h4>fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]:::literal
    DispatchOccurrence -. cmns-dt:succeeds .-> OrderReceivedOccurrence
```

Figure 6: TBox representation of a service delivery event occurrence for waste services

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    StageOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleStageOccurrence]] -. fibo-fnd-rel-rel:exemplifies .-> ServiceExecutionStage[[ontoservice:ServiceExecutionStage]]
    ServiceExecutionStage -. cmns-col:comprises .-> ServiceDispatchEvent[[ontoservice:ServiceDispatchEvent]]
    ServiceExecutionStage -. cmns-col:comprises .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]

    StageOccurrence -. cmns-col:comprises .-> DispatchOccurrence[[ServiceDispatchOccurrence]]
    DispatchOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDispatchEvent
    DispatchOccurrence -.-> EventOccurrence["<h4>fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]:::literal
    EventOccurrence -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DispatchEventStatus[[ontoservice:CompletedStatus]] -. cmns-dsg:describes .-> DispatchOccurrence

    StageOccurrence -. cmns-col:comprises .-> DeliveryOccurrence[["<h4>DeliveryOccurrence</h4><p style='font-size:0.75rem;'>ontoservice:hasScheduledTime &quot;xsd:time&quot;</p>"]]:::literal
    DeliveryOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDeliveryEvent
    DeliveryOccurrence -.-> EventOccurrence
    DeliveryOccurrence -. cmns-dt:succeeds .-> DispatchOccurrence
    DeliveryOccurrence -. fibo-fnd-plc-loc:isLocatedAt .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]
    DeliveryOccurrence -. fibo-fnd-rel-rel:exchanges .-> Bin[[ontowm:Bin]]
    DeliveryOccurrence -. ontoservice:hasTotalPrice .-> TotalPrice[[ontoservice:TotalPrice]]
    DeliveryEventStatus[[ontoservice:EventStatus]] -. cmns-dsg:describes .-> DeliveryOccurrence

    StageOccurrence  -. fibo-fnd-dt-fd:hasSchedule .-> Schedule[["<h4>fibo-fnd-dt-fd:RegularSchedule</h4><p style='font-size:0.75rem;'>fibo-fnd-dt-fd:hasCount &quot;xsd:integer&quot;</p>"]]:::literal
    Schedule -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:ExplicitTimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Start Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[End Time]]
    StartTime -.-> Time["<h4>cmns-dt:TimeOfDay</h4><p style='font-size:0.75rem;'>cmns-dt:hasTimeValue &quot;xsd:time&quot;</p>"]:::literal
    EndTime -.-> Time

    Schedule -. cmns-dt:hasStartDate .-> StartDate[["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]]:::literal
    Schedule -. fibo-fnd-dt-fd:hasRecurrenceInterval .-> Recurrence[[fibo-fnd-dt-fd:RecurrenceInterval]]:::literal
    Schedule -. fibo-fnd-dt-oc:hasOccurrence .-> DeliveryOccurrence
```

The amount of waste collected is also recorded through the use of the `CalculationEvent`. Do note that a `ServiceDeliveryEvent` must first occur before the `CalculationEvent`. An occurrence for the`CalculationEvent` event represents the calculation of the amount of waste disposed of at a specific waste disposal facility at a specific time. It consists of the deduction of the truck's unladen weight from the gross weight to get the net waste weight. If necessary, the type of waste can be retrieved from the service associated with this lifecycle. Note that the measurement unit of `tonne` is available in [OntoService's `abox` file](../ontoservice/ontoservice_abox.ttl).

Figure 7: TBox representation of an occurrence of the calculation of the amount of waste disposed

```mermaid
%%{init: {"flowchart": {"defaultRenderer": "elk"}} }%%
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    CalculationEvent[[fibo-fnd-dt-oc:CalculationEvent]] -. cmns-dt:succeeds .-> ServiceDeliveryEvent[[ontoservice:ServiceDeliveryEvent]]
    Calculation[["<h4>fibo-fnd-dt-oc:Calculation</h4><p style='font-size:0.75rem;'>rdfs:comment &quot;string&quot;<br>fibo-fnd-dt-oc:hasEventDate &quot;xsd:dateTime&quot;</p>"]]:::literal -. cmns-dt:succeeds .-> EventOccurrence[[fibo-fbc-pas-fpas:ContractLifecycleEventOccurrence]]
    ServiceExecutionStage[[ontoservice:ServiceExecutionStage]] -. cmns-col:comprises .-> ServiceDeliveryEvent
    ServiceExecutionStage -. cmns-col:comprises .-> CalculationEvent
    EventOccurrence -. fibo-fnd-rel-rel:exemplifies .-> ServiceDeliveryEvent
    CalculationEvent -. cmns-cls:classifies .-> Calculation

    ontowm:WasteDisposalFacility -. fibo-fnd-plc-loc:isLocatedAt .-> Location[[fibo-fnd-plc-loc:PhysicalLocation]]
    Calculation -. fibo-fnd-plc-loc:isLocatedAt .-> Location

    Calculation -. cmns-qtu:hasQuantityValue .-> NetWasteWeight[[ontowm:NetWasteWeight]]

    Calculation -. cmns-qtu:hasExpression .-> Expression[[fibo-fnd-utl-alx:Difference]]
    Expression -. fibo-fnd-utl-alx:hasMinuend .-> GrossWeight[[ontowm:GrossWeight]]
    Expression -. fibo-fnd-utl-alx:hasSubtrahend .-> UnladenWeight[[ontowm:UnladenWeight]]

    NetWasteWeight --> ScalarQuantityValue["<h4>cmns-qtu:ScalarQuantityValue</h4><p style='font-size:0.75rem;'>cmns-qtu:hasNumericValue &quot;xsd:decimal&quot;</p>"]:::literal
    GrossWeight --> ScalarQuantityValue
    UnladenWeight --> ScalarQuantityValue

    NetWasteWeight -. cmns-qtu:hasMeasurementUnit .-> tonne[[ontoservice:tonne]]
    GrossWeight -. cmns-qtu:hasMeasurementUnit .-> tonne
    UnladenWeight -. cmns-qtu:hasMeasurementUnit .-> tonne
```

### 2.2.2 Waste categories

The following waste categories are represented in this ontology. Please find the respective descriptions within the ontology for more details.

1. Ash waste
2. Sludge waste
3. Construction and demolition waste
4. Electronic waste
5. Ferrous metal waste
6. Food waste
7. Fish waste
8. Vegetable waste
9. Glass waste
10. Horticultural waste
11. Tree trunk waste
12. Tree branches waste
13. Leaves waste
14. Non-ferrous metal waste
15. Aluminium waste
16. Stainless steel waste
17. Copper waste
18. Paper or cardboard waste
19. Paper waste
20. Old corrugated cardboard waste
21. Plastics waste
22. Polyethylene terephthalate (PET) waste
23. High-density polyethylene (HDPE) waste
24. Low-density polyethylene (LDPE) waste
25. Polypropylene (PP) waste
26. Polystyrene (PS) waste
27. Scrap tyres waste
28. Textile or leather waste
29. Used slag waste
30. Wood waste
31. Pallet waste
32. Furniture waste

Additionally, there is also a mixed waste category, which allows users to denote the composition of waste if required.

Figure 8: TBox representation of the waste composition for mixed waste

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    MixedWaste[[ontowm:MixedWaste]] -. cmns-col:comprises .-> Waste[[ontowm:Waste]]
```

## 2.3 Assets

This ontology provides representation of assets managed by organisation with waste operations, such as bins and garbage trucks. These assets are also intended to have geospatial and temporal representations, which will typically follow the `geo:Feature` representation as follows:

Figure 9: TBox representation of geospatial and temporal representation of assets

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Feature[[geo:Feature]] -. geo:hasGeometry .-> Geometry[["<h4>geo:Geometry</h4><p style='font-size:0.75rem;'>geo:asWKT &quot;geo:wktLiteral&quot;</p>"]]:::literal
    Geometry -. time:hasTime .-> Instant[[time:Instant]]
    Geometry -. time:hasTime .-> Interval[[time:Interval]]
    Instant --> TemporalEntity[time:TemporalEntity]
    Interval --> TemporalEntity
```

### 2.3.1 Truck

The following truck types are employed in the waste management sector:

1. **Rear End Loader Truck**: A truck designed for municipal waste collection, featuring a rear-loading mechanism with a hydraulic lift to compact waste into the truck body.
2. **Hooklift Truck**: A versatile truck equipped with a hydraulic hooklift system, used for transporting various container types, including heavy industrial and commercial waste containers.
3. **Recycling Truck**: A truck specifically designed and equipped to collect, transport, and sometimes sort recyclable materials from various collection points to recycling facilities.

For more information on the driver of the truck, please see the representation in [OntoProfile](../ontoprofile#221-employment).

Figure 10: TBox representation of a truck for the waste management sector

```mermaid
flowchart BT
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Truck["<h4>vc:Truck</h4><p style='font-size:0.75rem;'>vc:vehicleIdentificationNumber &quot;string&quot;</p>"]:::literal --> Feature[geo:Feature]
    ontowm:HookliftTruck --> Truck
    ontowm:RearEndLoaderTruck --> Truck
    ontowm:RecyclingTruck --> Truck
```

### 2.3.2 Bin

There are six categories of bins:

1. **OTC bin**: A bin for attaching to an OTC truck
2. **3-feet bin**: An open top container bin with a length of 3 feet
3. **5-feet bin**: An open top container bin with a length of 5 feet
4. **7-feet bin**: An open top container bin with a length of 7 feet
5. **Compactor bin**: An open top container bin with the ability to compact waste

Figure 11: TBox representation of a bin

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Bin[["<h4>ontowm:Bin</h4><p style='font-size:0.75rem;'>ontowm:hasBinNumber &quot;string&quot;</p>"]]:::literal --> Feature[[geo:Feature]]
    Bin -. ontowm:hasStatus .-> BinStatus[[ontowm:BinStatus]]
```

Bin statuses are also represented as enums in the corresponding ABox. The available statuses are as follows:

- Filled
- Empty
- Available
- Unavailable
- Decomissioned
