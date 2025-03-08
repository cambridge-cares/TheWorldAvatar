# Pirmasens Amenities App

This repo contains the code for the Pirmasens Amenities App, which is intended to display the amenities in Pirmasens and their information such as operating hours and costs. At the moment, only toilets and Wasgau (markets) are available. It will briefly explain the data requirements and instructions for deployment.

## Table of Contents

- [Pirmasens Amenities App](#pirmasens-amenities-app)
  - [1. Requirement](#1-requirements)
    - [1.1 Ontology](#11-ontology)
    - [1.2 Data sources](#12-data-sources)
    - [1.3 Agents](#13-agents)
  - [2. Deployment](#2-deployment)
    - [2.1 Backend services](#21-backend-services)
      - [2.1.1 Stack Data Uploader](#211-stack-data-uploader)
      - [2.1.2 Feature Info Agent](#212-feature-info-agent)
    - [2.2 Android deployment](#22-android-deployment)
      - [2.2.1 Mapbox token](#221-mapbox-token)
      - [2.2.2 Phone and stack communication](#222-phone-and-stack-communication)
    - [2.3 Web visualization](#23-web-visualization)

## 1. Requirements

### 1.1 Ontology

The app requires the following ontologies:

1. `OntoCityToilets` - a preliminary draft ontology that has yet to be reviewed; Defined in the `./stack-data-uploader-inputs/inputs/data/pirmasens_toilets/tbox/` directory.

The following are related ontologies that need not be imported:

1. [DCAT](https://www.w3.org/TR/vocab-dcat-3/) ontology
2. [Schema](https://schema.org/) ontology
3. [Vcard](http://www.w3.org/2006/vcard/ns#) ontology

### 1.2 Data sources

1. Toilets - Self collected into a `csv`
2. Wasgau (Market) - Self collected into a `csv`

Any data access required should be contacted through someone working on the repository.

### 1.3 Agents

1. Feature Info Agent - Required for retrieving information
2. Routing Agent - Required to map route to the amenity based on current location

## 2. Deployment

### 2.1 Backend Services

The app will require a running [The World Avatar stack](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager) as the backend.

### 2.1.1 Stack Data Uploader

Data specified in [this section](#12-data-sources) should be uploaded using the [Stack Data Uploader](https://github.com/TheWorldAvatar/stack/tree/main/stack-data-uploader). All the relevant configurations and settings are included in the [`./inputs/data/`](./inputs/data/) directory.

Briefly, the app will only require the `Wasgau` and `Toilet` datasets, as well as the associated ontologies and OBDA mappings for the base functionality of this application. Extended configuration are for routing purposes.

### 2.1.2 Feature Info Agent

The stack will also require the [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent) service to retrieve metadata. Please read setting up the [built-in service section](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#built-in-containers) for more details on deploying this. This agent will require the following configuration targeted at the toilet class in `fia-config.json`:

```json
{
  "entries": [
    {
      "id": "Toilet",
      "class": "https://www.theworldavatar.com/kg/ontocitytoilets/Toilet",
      "meta": {
        "queryFile": "fia_toilet.sparql"
      }
    }
  ]
}
```

The relevant SPARQL query is as follows:

```
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX tps: <https://www.theworldavatar.com/kg/ontocitytoilets/>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX schema: <https://schema.org/>

SELECT ?Property ?Value ?Unit
WHERE {
  {
  	SERVICE [ONTOP] {[IRI] ?prop_iri ?Value .}
    ?prop_iri rdfs:label ?Property .
    FILTER(isLiteral(?Value))
  } UNION {
    tps:hasImage rdfs:label ?Property .
    SERVICE [ONTOP] {
      [IRI] tps:hasImage/dcat:distribution/dcat:downloadURL ?Value .
      FILTER(BOUND(?Value) && STRLEN(STR(?Value))>0)
    }
  } UNION {
    SERVICE [ONTOP] {
      [IRI] schema:openingHoursSpecification ?open_hour_spec.
      ?open_hour_spec schema:opens ?opens;
        schema:closes ?closes;
        schema:dayOfWeek ?day.
      BIND(CONCAT(STR(SUBSTR(STR(?day), 20)), " open hours") as ?Property)
      BIND(CONCAT(STR(SUBSTR(STR(?opens), 0,6) ),"-",STR(SUBSTR(STR(?closes), 0,6))) as ?Value)
      FILTER(BOUND(?Value) && STRLEN(STR(?Value))>0)
    }
  } UNION {
    BIND("average rating" as ?Property)
    {
      SELECT (ROUND(AVG(?rating)) AS ?Value)
      WHERE {
        [IRI] tps:hasRating ?ratingNode .
        ?ratingNode tps:ratingValue ?rating .
      }
    }
  } UNION {
    BIND("validity" as ?Property)
    SERVICE [ONTOP] {
      [IRI] schema:openingHoursSpecification ?open_hour_spec.
      ?open_hour_spec schema:dayOfWeek schema:Monday;
      	schema:validThrough ?through;
        schema:validFrom ?from.
      BIND(CONCAT(STR( SUBSTR(STR(?from), 6) )," until ",STR(SUBSTR(STR(?through), 6))) as ?Value)
      FILTER(BOUND(?Value) && STRLEN(STR(?Value))>0)
    }
  } UNION {
    SERVICE [ONTOP] {
    	[IRI] tps:hasFee/tps:price ?Value .
	  }
	  tps:price rdfs:label ?Property
  }
  UNION {
    SERVICE [ONTOP] {
    	[IRI] tps:hasFee/tps:priceCurrency ?Value .
	  }
    tps:priceCurrency rdfs:label ?Property
  }
}
```

### 2.2 Android deployment

This section is under construction.

#### 2.2.1 Mapbox token

Your mapbox token should be in [`settings.gradle`](./gradle.properties) and also [`developer-config.xml`](./core/utils/src/main/res/values/developer-config.xml).

#### 2.2.2 Phone and stack communication

Configure the network setting in [`network_config.xml`](./core/utils/src/main/res/values/network_config.xml) with the following format. 
```xml
    <string name="host_only">123.123.123.123</string>
    <string name="host_with_port">https://DOMAIN_NAME/PATH</string> 
    <!-- or for development use -->
<!--    <string name="host_with_port">http://123.123.123.123:80/path</string>--> 
```

### 2.3 Web Visualization

Users can also set up a web visualisation using [The World Avatar's visualisation platform](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform), and its documentation. For the specific requirements for [the data uploaded](#21-backend-services), users will require the following icons:

1. A home icon marker: `https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Home-icon.svg/1200px-Home-icon.svg.png` or other sources
2. A toilet icon marker: `inputs/icons/toilets.png`
3. A market icon marker: `inputs/icons/wasgau.png`

The configuration for extracting the relevant visualisation layers can be found at `inputs/web/data.json`.
