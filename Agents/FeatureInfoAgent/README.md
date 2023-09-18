# Feature Info Agent

<img align="right" width="250" height="250" src="./docs/fia-logo.svg">

This Feature Info Agent (FIA) acts as a single access point for [TWA Visualisations](https://github.com/cambridge-cares/TheWorldAvatar/wiki/TWA-Visualisations) to query for both meta and time series data of an individual feature (i.e. a single geographical location) before display within the side panel of the visualisation.

Please see the [CHANGELOG](./CHANGELOG.md) file for details on recent changes; the latest available image of the FIA can be determine by viewing its [GitHub package page](https://github.com/cambridge-cares/TheWorldAvatar/pkgs/container/feature-info-agent).

## Overview

The FIA is a relatively simple HTTP agent built using the TWA agent framework. It's goal is to take in the IRI of a single feature then use it to query the knowledge graphs for metadata, and the relational databases for time series data before formatting and returning it as a JSON object.

At the time of writing, automatic discovery of data is not feasible, as such the developer deploying an instance of the FIA is responsible for writing SPARQL queries to both return the raw metadata as well as the measurement IRIs of time series data (so that these can then be looked up in the relational databases to actually get the time series data).

These SPARQL queries are written on a class-by-class (T-Box) basis; this should mean that, for example, all IRIs that are A-Box instances of the `https://theworldavatar.io/Building` T-Box class will reuse the same SPARQL query as they _should_ have data in the same format.

## Restrictions

At the time of writing, the FIA has a few restrictions that all deploying developers should be aware of. These are as follows:

- The FIA can only be run within a [TWA Stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).
- The FIA can only report meta and time data that is contained within the same stack as the agent itself.
- The FIA can only return time series data on series that uses the Instant class.
  - This is due to a limitation with the underlying TimeseriesClient.

### Class discovery

In addition to the above restrictions, the FIA uses a hardcoded series of SPARQL queries to ask the KG was classes the received A-Box IRI belongs to. For the FIA to detect a class, then use it to find and run the correct metadata/time series query, the data must be able to fulfil the below query.

If the below query returns no results, the FIA will not function; developers may need to update their triples/mapping until the query does return.

```
prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT distinct ?class WHERE {
    [IRI] rdf:type ?class
}
```

```
prefix owl: <http://www.w3.org/2002/07/owl#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

select distinct ?class { 
    SERVICE [ONTOP] { [IRI] a ?x . }
    ?x rdfs:subClassOf* ?class .
    ?class rdf:type owl:Class .
}
```

## Requirements

For the FIA to function, a number of configuration steps need to take place before deployment, these are detailed in the subsections below. It is also necessary for users to have good knowledge of Docker, JSON, and to be familiar with management of the TWA Stack system.

### Configuration

Follow the below configuration steps within the `fia-queries` subdirectory of the TWA stack manager's data directory. Volumes that are used by containers running with the TWA Stack are populated by named subdirectories within the stack manager's [data directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data). For more details, read the [TWA Stack Manager documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

- Create a JSON configuration file named `fia-config.json`.
  - This configuration file should be a JSON object containing the following parameters:
    - `queries`: This is a **required** array of objects defining a mapping between (T-Box) class names and the names of files containing pre-written SPARQL queries. Each object needs to contain the following string parameters:
      - `class`: Full IRI of the class.
      - `metaFile`: Name of the file (inc. extension) that contains the query to run when gathering metadata. This file should also be located within the `queries` directory.
      - `timeFile`: Optional, name of the file (inc. extension) that contains the query to run when gathering timeseries measurement details. This file should also be located within the `queries` directory.
      - `timeLimit`: Optional, this is an integer parameter that defaults to 24. When set, timeseries data from the last N hours will be pulled (or all data if the value is set to below 0).
      - `databaseName`: Optional, but **required** if setting a timeFile. It should match the PostgreSQL database name that contains your timeseries data.

The `metaFile` should contain a single SPARQL query that is used to return metadata from the Knowledge Graph in the required format (listed below).

The `timeFiles` should contain a single SPARQL query that is used to return the measurement IRIs used to identify the connected time series objects (not the actual time series data). Note that this query should also return results in a set format (detailed below).

Within the [examples](./examples/) directory, a mock configuration file has been provided along with a number of example SPARQL query files.

### Expected query formats

To properly parse the metadata and time series queries, the agent requires the results from queries to fulfil a set format. For each type of query a number of placeholder tokens can be added that will be populated by the agent just before execution. These are:

- `[IRI]`: The IRI of the feature (A-Box) of interest, i.e. the feature selected within the TWA-VF (the IRI will be injected by the agent).
- `[ONTOP]`: The internal URL of the Ontop service within the stack (the URL will be injected by the agent).

**Metadata queries**:<br/>
Queries for metadata should not concern themselves with data relating to time series. Queries here need to return a table with two (or optionally three) columns. The first column should be named `Property` and contains the name of the parameter we're reporting, the second should be `Value` and contain the value. The optional third column is `Unit`; any other columns are currently ignored.

Queries that generate multiple rows with the same property name are supported, their values will be combined into a single JSON array by the agent.

<p align="center">
   <img src="./docs/meta-query-example.jpg" alt="Example result of a metadata query" width="50%"/>
</p>
<p align="center">
   <em>Results of a valid SPARQL query for metadata.</em>
</p>

**Queries for time series:**<br/>
Queries for time series need to return the measurement/forecast IRIs (i.e. the IRIs which are connected via `ts:hasTimeSeries`) to the actual time series instances stored in the relational database. Those IRIs will be used to grab the actual values from PostgreSQL as well as parameters associated with each measurement/forecast. Required columns are `Series` containing the time series' IRI, `Name` containing a user facing name for this entry, and `Unit` containing the unit (which can be blank);any other columns are currently ignored

<p align="center">
    <img src="./docs/time-query-example.jpg" alt="Example result of a time series query" width="75%"/>
</p>
<p align="center">
   <em>Results of a valid SPARQL query for time series IRIs.</em>
</p>

## Requests

The following HTTP request routes are available for the agent:

- `/get`
  - Run algorithm to gather metadata and time series.
  - Requires the `iri` parameter.
  - Supports optional `endpoint` parameter to direct KG queries rather than federating.

- `/status`
  - Reports the agent's current status.

- `/refresh`
  - Forces the agent to re-scan for available Blazegraph endpoints.

## Enabling the FIA in a stack

The FIA container is an optional built-in service in the stack; to enable it you need to create/modify the configuration file for that stack. An example of the changes required are described in the stack-manager readme file [here](../../Deploy/stacks/dynamic/stack-manager/README.md#adding-the-feature-info-agent). After spinning up the stack the agent should be accessible via the `/feature-info-agent` route.

Note that the version of the FIA run by the stack is determined by the stack manager itself; to use a custom (or newer) version, developers will need ensure the newer FIA image is built (either locally or uploaded to GitHub), then provide a custom service configuration (ideally a near-copy of the stack's default configuration for the FIA, found [here](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-clients/src/main/resources/com/cmclinnovations/stack/services/built-ins/feature-info-agent.json)) within the stack manager's `inputs/config/services` directory.

## Automated actions

The FIA is currently set up with two automated GitHub actions:

- **Test the FeatureInfoAgent:**
  - Only runs when files within the agent have changed AND on commits that are part of a non-draft PR to the main branch.
  - Tests the FIA by running its unit tests and compiling a Docker image (which is NOT pushed at this stage).
  
- **Push the FeatureInfoAgent:**
  - Only runs when files within the agent are changed AND on commits to the main branch (i.e. after a PR is approved and merged).
  - Builds the FIA's Docker image (inc. running the unit tests again) AND pushes it to the TWA GitHub image registry.

## Examples

A number of example metadata and time series queries, along with an example FIA configuration file, can be found within the [FIA Examples](./examples/README.md) document.

## Troubleshooting

For troubleshooting and FAQs, please see the [FIA Troubleshooting](./docs/troubleshooting.md) document.

## Development

The FIA is a simple HTTP agent written using the existing TWA agent framework. The core functionality of the agent is split across 4 classes; the central `FeatureInfoAgent` class that acts as the receiver and transmitter for HTTP requests, and classes that actually run logic (which should be self-explanatory): `ClassHandler`, `MetaHandler`, and `TimeHandler`.

The algorithm used to find, format, and return data after a request is received is detailed in the diagram below (although you can also read the in-code documentation for more details).

Building the Docker image for the FIA is automatically triggered under certain conditions (see above), but developers can also build a local copy using the provided `build.sh` script after supplying the required `repo_username.txt` and `repo_password.txt` files within the `credentials` directory.

Mermaid diagrams are available for the [start up routine](./docs/mermaid-startup.md) and the [main retrieval algorithm](./docs/mermaid-get-request.md).