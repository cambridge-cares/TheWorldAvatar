# 1. Ontology for the Profiles of Organizations and People

OntoProfile is designed to represent the profiles of organizations and people, inclusive of their names, roles, and responsibilities. The ontology is primarily an extension of the [Financial Industry Business Ontology (FIBO)](https://spec.edmcouncil.org/fibo/) to accommodate profiles unavailable in the original ontology.

The namespace for the ontology is:

<p align="center"><i>https://www.theworldavatar.com/kg/ontoprofile/</i></p>

## Table of Contents

- [1. Ontology for the Profiles of Organizations and People](#1-ontology-for-the-profiles-of-organizations-and-people)
- [2. Data Model](#2-data-model)
  - [Legend](#legend)
  - [2.1 Profiles](#21-profiles)
    - [2.1.1 Organisations](#211-organisations)
    - [2.1.2 Person](#212-person)
  - [2.2 Situations](#22-situations)
    - [2.2.1 Employment](#221-employment)
    - [2.2.2 Facility usage](#222-facility-usage)

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

| Prefix            | Namespace                                                                                 |
| ----------------- | ----------------------------------------------------------------------------------------- |
| bot               | `https://w3id.org/bot#`                                                                   |
| cmns-col          | `https://www.omg.org/spec/Commons/Collections/`                                           |
| cmns-dt           | `https://www.omg.org/spec/Commons/DatesAndTimes/`                                         |
| cmns-dsg          | `https://www.omg.org/spec/Commons/Designators/`                                           |
| cmns-pts          | `https://www.omg.org/spec/Commons/PartiesAndSituations/`                                  |
| cmns-rlcmp        | `https://www.omg.org/spec/Commons/RolesAndCompositions/`                                  |
| fibo-be-le-lei    | `https://spec.edmcouncil.org/fibo/ontology/BE/LegalEntities/LEIEntities/`                 |
| fibo-fnd-agr-ctr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Agreements/Contracts/`                     |
| fibo-fnd-arr-rep  | `https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/`                   |
| fibo-fnd-aap-ppl  | `https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/`                   |
| fibo-fnd-org-org  | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/Organizations/`              |
| fibo-fnd-pas-pas  | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/`  |
| fibo-fnd-pas-psch | `https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/PaymentsAndSchedules/` |
| fibo-fnd-plc-adr  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/`                         |
| fibo-fnd-plc-fac  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Facilities/`                        |
| fibo-fnd-plc-loc  | `https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/`                         |
| fibo-fnd-pty-pty  | `https://spec.edmcouncil.org/fibo/ontology/FND/Parties/Parties/`                          |
| fibo-fnd-rel-rel  | `https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations`                       |
| fibo-fnd-org-fm   | `https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/FormalOrganizations/`        |
| lcc-cr            | `https://www.omg.org/spec/LCC/Countries/CountryRepresentation/`                           |
| om                | `http://www.ontology-of-units-of-measure.org/resource/om-2/`                              |
| sf                | `http://www.opengis.net/ont/sf#`                                                          |
| geo               | `http://opengis.net/ont/geosparql#`                                                       |
| time              | `http://www.w3.org/2006/time#`                                                            |
| vcard             | `https://www.w3.org/2006/vcard/ns#`                                                       |
| vc                | `https://spec.edmcouncil.org/auto/ontology/VC/VehicleCore/`                               |
| rdfs              | `http://www.w3.org/2000/01/rdf-schema#`                                                   |
| ontobim           | `https://www.theworldavatar.com/kg/ontobim/`                                              |
| ontoprofile       | `https://www.theworldavatar.com/kg/ontoprofile/`                                          |

## 2.1 Profiles

This section describes the profile details about an organisation and/or person, as well as the roles they could play.

### 2.1.1 Organisations

The profiles of organisations generally includes their organisation names, mailing address, as well as their different roles. An organization may also play the following roles represented under the `cmns-pts:PartyRole` concept:

1. `fibo-fnd-org-fm:Employer` - party that provides compensation in exchange for work performed by one or more people up to the standard that the party dictates
2. `fibo-fnd-pas-pas:Client` - party that purchases professional services from another party
3. `fibo-fnd-pas-pas:ServiceProvider` - party that provides and typically provisions professional services.

Figure 1: TBox representation for an organisation's profile

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    PartyRole[[cmns-pts:PartyRole]] -. cmns-rlcmp:isPlayedBy .-> Organisation[[fibo-fnd-org-fm:FormalOrganization]]
    Organisation -. cmns-dsg:hasName .-> OrganizationName[["<h4>fibo-fnd-org-org:OrganizationName</h4><p style='font-size:0.75rem;'>fibo-be-le-lei:hasTransliteratedName &quot;string&quot;</p>"]]:::literal

    Organisation -. fibo-fnd-pty-pty:hasMailingAddress .-> ConventionalStreetAddress[["<h4>fibo-fnd-plc-adr:ConventionalStreetAddress</h4><p style='font-size:0.75rem;'>fibo-fnd-plc-loc:hasCityName &quot;string&quot;<br>fibo-fnd-plc-adr:hasPostalCode &quot;string&quot;</p>"]]:::literal

    ConventionalStreetAddress -. fibo-fnd-plc-loc:hasCountry .-> Country[[lcc-cr:Country]]
    ConventionalStreetAddress -. fibo-fnd-plc-adr:hasStreetAddress .-> StreetAddress[[fibo-fnd-plc-adr:StreetAddress]]

    StreetAddress -. fibo-fnd-plc-adr:hasPrimaryAddressNumber .-> PrimaryAddressNumber[["<h4>fibo-fnd-plc-adr:PrimaryAddressNumber</h4><p style='font-size:0.75rem;'>fibo-fnd-rel-rel:hasTag &quot;string&quot;</p>"]]:::literal
    StreetAddress -. fibo-fnd-plc-adr:hasStreetName .-> StreetName[["<h4>fibo-fnd-plc-adr:StreetName</h4><p style='font-size:0.75rem;'>fibo-fnd-rel-rel:hasTag &quot;string&quot;</p>"]]:::literal
```

### 2.1.2 Person

The profile of a person generally includes their name and may include their contact information. A person may also play the following roles represented under the `cmns-pts:PartyRole` concept:

1. `fibo-fnd-org-fm:Employee` - A person in the service of another under any contract of hire
2. `ontoprofile:EmployedDriver`- An employee who is specifically employed to drive a vehicle
3. `ontoprofile:OperationManager`- An employee who is specifically employed to manage and supervise the operational conditions of a service
4. `ontoprofile:OperationScheduler`- An employee who is specifically employed to manage the operational schedule of a service
5. `ontoprofile:SalesAdmin`- An employee who is specifically employed to communicate with clients about the company's products and/or services
6. `ontoprofile:Student` - A person who has a membership role within an educational institution, typically enrolled in a course of study or program, and is involved in learning or training activities under the supervision of educators

Figure 2: TBox representation for a person's profile

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    PartyRole[[cmns-pts:PartyRole]] -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    Person -. cmns-dsg:hasName .-> PersonName[["<h4>fibo-fnd-aap-ppl:PersonName</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot; <br> fibo-fnd-aap-ppl:hasFullLegalName &quot;string&quot; <br> fibo-fnd-aap-ppl:hasSurname &quot;string&quot;</p>"]]:::literal

    Person -. ontoprofile:hasInitials .-> Initials[["<h4>ontoprofile:Initials</h4><p style='font-size:0.75rem;'>rdfs:label &quot;string&quot;</p>"]]:::literal

    Person -. vcard:hasTelephone .-> Cell[["<h4>vcard:Cell</h4><p style='font-size:0.75rem;'>vcard:hasValue &quot;string&quot;</p>"]]:::literal
```

## 2.2 Situations

This section describes the various situations that organisations or people are involved in.

### 2.2.1 Employment

In this situation, a person can be employed by an organisation, who plays an employer role. It is pertinent to realise that both the employee and employer concepts are established as party roles that is played either by a person or an organisation. Employees also have their associated work responsibilities that they must fulfil, such as their working hours specified by `cmns-dt:hasTimePeriod`.

Figure 3: TBox representation for an employment situation

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Employment[[fibo-fnd-org-fm:Employment]] -. fibo-fnd-org-fm:hasEmployingParty .-> Employer[[fibo-fnd-org-fm:Employer]]
    Employment -. fibo-fnd-org-fm:hasEmployedParty .-> Employee[[fibo-fnd-org-fm:Employee]]
    Employment -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Start Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[End Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    Employee -. fibo-fnd-org-fm:isEmployeeOf .-> Employer
    Employee -. ontoprofile:hasStatus .-> EmployeeStatus[[ontoprofile:EmployeeStatus]]
    Employer -. cmns-rlcmp:isPlayedBy .-> Org[[fibo-fnd-org-fm:FormalOrganization]]
    Employee -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    Org -. cmns-col:hasMember .-> Person

    Employee -. cmns-dt:hasTimePeriod .-> TimePeriod[[cmns-dt:TimePeriod]]
    TimePeriod -. cmns-dt:hasStart .-> StartTime[[Start Time]]
    TimePeriod -. cmns-dt:hasEndTime .-> EndTime[[End Time]]
    StartTime -.-> Time["<h4>cmns-dt:TimeOfDay</h4><p style='font-size:0.75rem;'>cmns-dt:hasTimeValue &quot;xsd:time&quot;</p>"]:::literal
    EndTime -.-> Time
```

Note that the employee status are represented as enums in the corresponding ABox `OntoProfile_abox.ttl`. The available statuses are as follows:

- OnLeave
- OnWork

> Driver's employment

When a person is employed by an organisation as a driver, they may be assigned to a vehicle through the representation of a functional role for a period stipulated in the situation.

Figure 4: TBox representation of driver's employment

```mermaid
flowchart TD
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Situation[[cmns-pts:Situation]] -. cmns-pts:hasSubjectRole .-> Driver[[ontoprofile:EmployedDriver]]
    Situation -. cmns-pts:hasObjectRole .-> VDriver[[ontoprofile:VehicleDriver]]
    Situation -. cmns-pts:holdsDuring .-> DatePeriod[[cmns-dt:DatePeriod]]
    DatePeriod -. cmns-dt:hasStartDate .-> StartDate[[Start Date]]
    DatePeriod -. cmns-dt:hasEndDate .-> EndDate[[End Date]]
    StartDate -.-> Date["<h4>cmns-dt:Date</h4><p style='font-size:0.75rem;'>cmns-dt:hasDateValue &quot;xsd:date&quot;</p>"]:::literal
    EndDate -.-> Date

    Driver --> Employee[[fibo-fnd-org-fm:Employee]]
    Driver -. cmns-rlcmp:isPlayedBy .-> Person[[fibo-fnd-aap-ppl:Person]]
    VDriver --> cmns-rlcmp:FunctionalRole
    VDriver -. fibo-fnd-rel-rel:controls .-> Vehicle[["<h4>vc:Vehicle</h4><p style='font-size:0.75rem;'>vc:vehicleIdentificationNumber &quot;string&quot;</p>"]]:::literal
    Vehicle -. vc:owner .-> Org[[fibo-fnd-org-fm:FormalOrganization]]
    Employee -. fibo-fnd-org-fm:isEmployeeOf .-> Employer
    Employer[[fibo-fnd-org-fm:Employer]] -. cmns-rlcmp:isPlayedBy .-> Org
```

### 2.2.2 Facility usage

This section depicts the usage of facility by organisations and employees. These facilities may operate in a building which is associated with a corresponding address and geolocation. The ontology also enables the inclusion of employees from external organisations.

Figure 5: TBox representation for facility usage

```mermaid
flowchart LR
    %% Styling
    classDef literal fill:none
    classDef node overflow-wrap:break-word,text-wrap:pretty
    linkStyle default overflow-wrap:break-word,text-wrap:pretty;

    %% Contents
    Org[[fibo-fnd-org-fm:FormalOrganization]] -. fibo-fnd-rel-rel:controls .-> Facility
    Employee[[fibo-fnd-org-fm:Employee]] -. ontoprofile:worksAt .-> Facility

    Building[[bot:Building]]-. ontobim:hasFacility .-> Facility[[ontobim:Facility]]
    Building -. fibo-fnd-pty-adr:hasAddress .-> ConventionalStreetAddress[["<h4>fibo-fnd-plc-adr:ConventionalStreetAddress</h4><p style='font-size:0.75rem;'>fibo-fnd-plc-loc:hasCityName &quot;string&quot;<br>fibo-fnd-plc-adr:hasPostalCode &quot;string&quot;</p>"]]:::literal

    ConventionalStreetAddress -. fibo-fnd-plc-loc:hasCountry .-> Country[[lcc-cr:Country]]
    ConventionalStreetAddress -. fibo-fnd-plc-adr:hasStreetAddress .-> StreetAddress[[fibo-fnd-plc-adr:StreetAddress]]

    StreetAddress -. fibo-fnd-plc-adr:hasPrimaryAddressNumber .-> PrimaryAddressNumber[["<h4>fibo-fnd-plc-adr:PrimaryAddressNumber</h4><p style='font-size:0.75rem;'>fibo-fnd-rel-rel:hasTag &quot;string&quot;</p>"]]:::literal
    StreetAddress -. fibo-fnd-plc-adr:hasStreetName .-> StreetName[["<h4>fibo-fnd-plc-adr:StreetName</h4><p style='font-size:0.75rem;'>fibo-fnd-rel-rel:hasTag &quot;string&quot;</p>"]]:::literal
```
