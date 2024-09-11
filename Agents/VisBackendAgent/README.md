# Vis Backend Agent

The Vis-Backend Agent is a supporting service to The World Avatar's [visualisation platform (ViP)](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform). It is designed to manage all visualisation-related requests from a single point of access to for example, filter map layers or generate dynamic controls. By abstracting the backend implementation details (such as which other agents to call), it provides a unified access point to the data within its specific stack. This design allows the ViP to be deployed on a separate stack while retaining the capability to ingest data from multiple stacks seamlessly.

## Table of Contents

- [Vis Backend Agent](#vis-backend-agent)
  - [1. Agent Deployment](#1-agent-deployment)
    - [1.1 Preparation](#11-preparation)
    - [1.2 Docker Deployment](#12-docker-deployment)
  - [2. Agent Route](#2-agent-route)
    - [2.1 Status ROUTE](#21-status-route-urlvis-backend-agentstatus)
    - [2.2 Form ROUTE](#22-form-route-urlvis-backend-agentformtype)
    - [2.3 Instance ROUTE](#23-instance-route-urlvis-backend-agenttypetype)
  - [3. SHACL Restrictions](#3-shacl-restrictions)

## 1. Agent Deployment

The agent is designed for execution through a Docker container within [The World Avatar's stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). It cannot run as a standalone container, and other deployment workflows are beyond the scope of this document.

### 1.1 Preparation

Before using this agent, follow the steps below to ensure you have everything you need to successfully run the agent.

##### Maven Repository credentials

This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide your credentials in a single-word text files located like this:

```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

##### Environment variables

The agent requires the following environment variables. These variables must be set in their respective docker configuration files for the agent to function as intended.

- `NAMESPACE`: Specifies the SPARQL namespace identifier containing the corresponding instances (default: kb)

##### Files

At present, the agent's only functionality is to generate a form template that filters map layers based on its parameters. When more functionalities are introduced, this section will be further refined.

In generating the form template, users must create and upload [`SHACL` restrictions](#3-shacl-restrictions) into the `namespace` specified in the previous section. Users must also generate a corresponding identifier and target classes in `./resources/application-form.json`. This file must be copied into the Docker container via bind mounts. The target class must also correspond to the object of the `NodeShape sh:targetClass ?object` triple in order to function.

### 1.2 Docker Deployment

**TEST ENVIRONMENT**

- Deploy the agent to execute the unit tests on a standalone container by running the following code in the CLI at the <root> directory.
- The success of all tests must be verified through the Docker logs.

```
docker compose -f "./docker/docker-compose.test.yml" up -d --build
```

**PRODUCTION ENVIRONMENT**

1. Build this agent's image by issuing `docker compose -f './docker/docker-compose.yml' build` within this directory. Do not start the container.
2. Update the environment variables in `./docker/vis-backend-agent.json` if required.
3. Copy the `./docker/vis-backend-agent.json` file into the `inputs/config/services` directory of the stack manager.
4. Ensure the bind mount path is correctly set in the stack configuration for `vis-resources`.
5. Start the stack manager as usual following [these instructions](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

**DEBUGGING ENVIRONMENT**
Follow the same steps as the **PRODUCTION ENVIRONMENT**, but use the `vis-backend-agent-debug.json` file instead in step 3.

If you are developing in VSCode, please add the following `launch.json` to the `.vscode` directory. Once the agent is running with the debug configuration, the developer can attach the debugger on the debug panel in VSCode.

```json
{
  // Use IntelliSense to learn about possible attributes.
  // Hover to view descriptions of existing attributes.
  // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
  "version": "0.2.0",
  "configurations": [
    {
      "type": "java",
      "name": "Debug attach",
      "request": "attach",
      "port": 5007,
      "hostName": "localhost",
      "projectName": "vis-backend-agent"
    }
  ]
}
```

## 2. Agent Route

The agent currently offers the following API route(s):

### 2.1 Status ROUTE: `~url~/vis-backend-agent/status`

This route serves as a health check to confirm that the agent has been successfully initiated and is operating as anticipated. It can be called through a GET request with no parameters, as follows:

```
curl localhost:3838/vis-backend-agent/status
```

If successful, the response will return `Agent is ready to receive requests.`.

### 2.2 Form ROUTE: `~url~/vis-backend-agent/form/{type}`

This route serves as an endpoint to retrieve the corresponding form template for the specified target class type. Users can send a GET request to `~url~/vis-backend-agent/form/{type}`, where `{type}` is the requested identifier that must correspond to a target class in `./resources/application-form.json`.

If successful, the response will return a form template in the following (minimal) JSON-LD format. Please note that the template does not follow any valid ontology rules at the root level, and is merely a schema for the frontend. However, its nested values complies with `SHACL` ontological rules.

```json
{
  "http://www.w3.org/ns/shacl#property": [
    {
      "@id": "PROPERTY IRI",
      "@type": "http://www.w3.org/ns/shacl#PropertyShape",
      "http://www.w3.org/ns/shacl#name": {
        "@value": "form field name"
      },
      "http://www.w3.org/ns/shacl#description": {
        "@value": "description."
      },
      "http://www.w3.org/ns/shacl#group": {
        "@id": "GROUP IRI"
      }
    },
    {
      "@id": "GROUP IRI",
      "@type": "http://www.w3.org/ns/shacl#PropertyGroup",
      "http://www.w3.org/2000/01/rdf-schema#comment": {
        "@value": "Description of group."
      },
      "http://www.w3.org/2000/01/rdf-schema#label": {
        "@value": "property group name"
      },
      "http://www.w3.org/ns/shacl#property": [
        {
          "@id": "PROPERTY IRI",
          "@type": "http://www.w3.org/ns/shacl#PropertyShape",
          "http://www.w3.org/ns/shacl#name": {
            "@value": "form field name"
          },
          "http://www.w3.org/ns/shacl#description": {
            "@value": "description."
          },
          "http://www.w3.org/ns/shacl#group": {
            "@id": "GROUP IRI"
          }
        },
        ...
      ]
    }
  ]
}
```

### 2.3 Instance ROUTE: `~url~/vis-backend-agent/type/{type}`

This route serves as an endpoint to retrieve all available ontology classes and subclasses along with their human readable labels and descriptions associated with the type. Users can send a GET request to `~url~/vis-backend-agent/type/{type}`, where `{type}` is the requested identifier that must correspond to a target class in `./resources/application-form.json`.

If successful, the response will return an array of objects in the following format:

```json
{
  "type": {
    "type": "uri",
    "value": "instance IRI",
    "dataType": "http://www.w3.org/2001/XMLSchema#string",
    "lang": "Optional language field"
  },
  "label": {
    "type": "literal",
    "value": "Label of the class instance",
    "dataType": "http://www.w3.org/2001/XMLSchema#string",
    "lang": "Optional language field"
  },
  "description": {
    "type": "literal",
    "value": "Description for the class instance",
    "dataType": "http://www.w3.org/2001/XMLSchema#string",
    "lang": "Optional language field"
  }
}
```

## 3. SHACL Restrictions

[SHACL](https://www.w3.org/TR/shacl/) is generally a language for validating RDF graphs against a set of conditions. The World Avatar incorporates these restrictions into our workflow to populate form structure and fields. The query to generate the form template is available at `resources/query/construct/form.sparql`. Please read the documentation available on the [Visualisation Platform](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform/doc/form.md) to understand how the form template generated from this agent should look like.

A sample SHACL format in (TTL) is described below:

```
base:NameOfConceptShape
  a sh:NodeShape ;
  sh:targetClass base:Concept ;
  sh:property [
    sh:name "dropdown" ;
    sh:order 1 ;
    sh:description "A sample property showing the structure for creating a new dropdown and its list of selections" ;
    sh:path base:hasDropdownOptions ;
    sh:in base:DropdownOption ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
  ] ;
  sh:property [
    sh:name "text input";
    sh:description "A sample property showing the structure for creating a new text input field" ;
    sh:order 2 ;
    sh:path base:hasInput ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
  ] ;
  sh:property [
    sh:name "number input";
    sh:description "A sample property showing the structure for creating a new numerical input field" ;
    sh:order 3 ;
    sh:path base:hasInput ;
    sh:datatype xsd:decimal ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
  ] ;
  sh:property [
    sh:name "date input";
    sh:description "A sample property showing the structure for creating a new date input field" ;
    sh:order 4 ;
    sh:path base:hasInput ;
    sh:datatype xsd:date ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
  ] ;
  sh:property [
    sh:name "group" ;
    sh:description "A sample property showing the structure for creating a new property group" ;
    sh:order 5 ;
    sh:path base:hasGroup ;
    sh:node base:NestedConceptShape ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
  ] .

base:NestedConceptShape
  a sh:NodeShape ;
  sh:targetClass base:NestedConcept ;
  sh:property [
    sh:name "nested input" ;
    sh:order 1 ;
    sh:description "A sample property showing the structure for creating a nested property input as part of a group" ;
    sh:path base:hasNestedProperty ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
  ] .
```
