# Ontology for Traffic Incident (OntoTrafficIncident)
## 1. Introduction
The OntoTrafficIncident is developed to represent the concepts and relationship related to traffic incidents instantiated via LTA API datamall. It describes the description, traffic incident type and its time properties. 

## 1.1 Related Agents
This ontology is used in the following agent:
1) [TrafficIncidentAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/TrafficIncidentAgent) 

## 1.2 Legend
| Prefix | URI                                                   |
|--------|-------------------------------------------------------|
| geo    | [http://www.opengis.net/ont/geosparql#](http://www.opengis.net/ont/geosparql#) |
| time   | [http://www.w3.org/2006/time#](http://www.w3.org/2006/time#) |
| tfi    | [https://www.theworldavatar.com/ontotrafficincident/](https://www.theworldavatar.com/ontotrafficincident/) |
| dbr    | [https://dbpedia.org/page/](https://dbpedia.org/page/) |
| sf     | [http://www.opengis.net/ont/sf](http://www.opengis.net/ont/sf) |

## 1.3 Mermaid Diagram
```mermaid
erDiagram
    "tfi:TrafficIncident" {
        tfi-hasTrafficIncidentType string
        tfi-hasDescription string
        rdf-type sf-Point
        geo-asWKT geom
    }
    "time:Interval" {
    }

    "time:Instant" {
        time-hasTRS dbr-Unix_time
        time-numericPosition xsd-decimal
    }

    "tfi:TrafficIncident" ||--o{ "time:Interval" : "tfi:hasTimeInterval"
    "time:Interval" ||--o{ "time:Instant" : "time:hasBeginning"
    "time:Interval" ||--o{ "time:Instant" : "time:hasEnd"