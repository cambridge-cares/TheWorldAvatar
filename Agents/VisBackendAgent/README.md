# Vis-Backend Agent

The Vis-Backend Agent is a supporting service to The World Avatar's [visualisation platform (ViP)](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform). It is designed to manage all visualisation-related requests from a single point of access to for example, filter map layers or generate dynamic controls. By abstracting the backend implementation details (such as which other agents to call), it provides a unified access point to the data within its specific stack. This design allows the ViP to be deployed on a separate stack while retaining the capability to ingest data from multiple stacks seamlessly.

## Instructions

Before using this agent, follow the steps below to ensure you have everything you need to successfully run the agent.

### 1. Agent Deployment

The agent is designed for execution through a Docker container within a stack. It cannot run as a standalone container, and other deployment workflows are beyond the scope of this document.

#### 1.1 Preparation

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

#### 1.2 Docker Deployment

**TEST ENVIRONMENT**

- Deploy the agent to execute the unit tests by running the following code in the CLI at the <root> directory.
- The success of all tests must be verified through the Docker logs.

```
docker compose -f "./docker/docker-compose.test.yml" up -d --build
```

**PRODUCTION ENVIRONMENT**

1. Build this agent's image by issuing `docker compose -f './docker/docker-compose.yml' build` within this folder. Do not start the container.
2. Copy the `vis-backend-agent.json` file from the `docker` folder into the `inputs/config/services` folder of the stack manager, adjusting the absolute path of the bind mount as required.

- Please review the [different routes](#2-agent-route) to understand the purpose of these bind mounts. See [sample bind mounts](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#bind-mounts) for the configuration syntax.

3. Start the stack manager as usual following [these instructions](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

**DEBUGGING ENVIRONMENT**
Follow the same steps as the **PRODUCTION ENVIRONMENT**, but use the `vis-backend-agent-debug.json` file instead in step 2.

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

### 2. Agent Route

The agent currently offers two API route:

#### 2.1 Status ROUTE: `~url~/vis-backend-agent/status`

This route serves as a health check to confirm that the agent has been successfully initiated and is operating as anticipated. It can be called through a GET request with no parameters, as follows:

```
curl localhost:3838/vis-backend-agent/status
```

If successful, the response will return `"Agent is ready to receive requests."`.

#### 2.2 UI ROUTE: `~url~/vis-backend-agent/ui`

This route retrieves the UI configuration for the target resource on the stack through a GET request with the following parameters. This configuration is delivered to the ViP to establish the necessary user interfaces.

1. `target`: Target resource name

A sample request for curl syntax (in one line) is as follows:

```
curl localhost:3838/vis-backend-agent/ui?target=data
```

If the agent ran successfully, a JSON response containing the UI component configuration would be returned as follows. Please see the respective UI element for their specific configurations that will be returned.

```json
{
  "type": "filter",
  "config" : [{
    "label" : "Parameter name",
    "type" : "input",
    "options" : [ {"Specific Option JSON Object"}, ... ]
  }]
}
```

### 3. UI Configuration Files

The Vis-Backend agent offers configurations for various user interfaces, which are dependent on `json` files at the `<root>/resources/config` directory. These files consist of a `controls.json` and at least one `resource.json`. The `controls.json` serve as a coordinator that associates the target resource to their designated configuration files. Each resource group holds a key for the resource name and must have the following parameters:

- `url`: Location of target `resource.json` file
- `type`: Type of user interface. See below for more details

At the moment, the agent has the following user interfaces. Please read their corresponding sections to understand the `resource.json` format for the target resource configuration. Note that users can set up multiple target resources to generate the user interfaces they require.

- Filter Panel (`type: filter`): users are able to interact with search parameters to filter the map elements

A sample `controls.json` is:

```json
{
  // This is the resource name key that must be sent and accessed via the target parameter in the HTTP request
  "resource": {
    "url": "resource.json",
    "type": "filter"
  }
}
```

#### 3.1 Filter Panel

The filter panel's `resource.json` contains an array of UI elements that must be displayed within the panel. Examples of these elements include the dropdown or input elements. In the `json` file, each UI element must contain the following parameters:

1. `type`: type of UI component. Valid inputs include `input`, `dropdown`.
2. `label`: parameter name.
3. `query`: holds query-related configuration that is dependent on the type of UI component.

A sample file is as follows:

```json
[
  {UI ELEMENT GROUP},
  {UI ELEMENT GROUP}
]
```

> Input element

Input elements are displayed as a group with a group name (indicated by the root `label` key) and individual inputs. These inputs are set up through the array associated with the `query` parameter. Each input will require the following parameters:

- `label`: The public facing display name of the input
- `filter`: Name of the filter which will be replaced in the filter agent
- `type`: The input type - all or number. Only digits are allowed for number type

An example for one input group is as follows:

```json
{
  // Group
  "type": "input",
  "label": "Area [m2]",
  "query": [
    // First input element
    {
      "label": "Min",
      "filter": "minarea",
      "type": "number"
    },
    // Second input element
    {
      "label": "Max",
      "filter": "maxarea",
      "type": "number"
    }
  ]
}
```

A sample response group is as follows:

```json
{
  // Group
  "type": "input",
  "label": "Area [m2]",
  "options": [
    // First input element
    {
      "label": "Min",
      "filter": "minarea",
      "type": "number"
    },
    // Second input element
    {
      "label": "Max",
      "filter": "maxarea",
      "type": "number"
    }
  ]
}
```

> Dropdown element

Dropdown elements display a set of filter options for users to select. They have an additional `required` property for indicating if one option must always be selected. The agent will then retrieve the set of options through the SPARQL queries listed via the `query` parameter. Specifically, the `query` parameter must have the following information:

- `namespace`: Namespace for executing the query
- `file`: Location of the `.sparql` file containing the required SPARQL query

An example is as follows:

```json
{
  "type": "dropdown",
  "label": "Building Use",
  "required": false, // Optional to select a value in dropdown
  "query": [
    {
      "namespace": "buildings",
      "file": "use.sparql"
    }
  ]
}
```

A sample response group is as follows:

```json
{
  // Group
  "type": "dropdown",
  "label": "Building Use",
  "isRequired": false, // Optional to select a value in dropdown
  "options": [
    // First option element
    { "group": "BuildingUse", "value": "Residential" },
    // Second option element
    { "group": "BuildingUse", "value": "Commercial" }
  ]
}
```
