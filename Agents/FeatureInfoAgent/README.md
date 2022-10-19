# Feature Info Agent

This agent acts as a single access point for visualisations, allowing them to query for data on an individual feature (i.e. location). When a HTTP request is received, this agent uses the IRI within the request to query the relevant knowledge graphs and/or relational databases to return relevant feature metadata that the visualisation can then display.

**Note: This version of the agent is a VERY rough implementation and should not be used outside of very specific use cases. It currently relies on hardcoded SPARQL queries, and will not work out-of-the box for any old data sets. Future improvements will make this more generic, at which point the agent could be used more widely.**

# Requirements

Display of meta and timeseries data within visualisations is supported by the DTVF, however this functionality is still under development and requires data to be hosted within a [stack instance](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), with an instance of this agent running, and the geospatial data to meet certain standards. If these conditions are not met, any metadata baked directly into the geospatial data is shown instead; it's recommended to use this approach for now.

## Agent Setup

Before deploying this agent to your stack instance, a few stages of setup must be completed.

1. Ensure that your data has been marked up with RDF classes.
2. Write a query in a `*.sparql` file that gathers the desired metadata and returns it within a table format that contains rows of two/three columns: `label` for the property name, `value` for its value, and optionally a `unit` column with the unit string.
3. Add the query file to the `code/WEB-INF/queries` directory.
4. Update the code within the `Lookups` class to map your class IRI to the name of the newly added query file (see line 53 for an example).
5. Build the agent and deploy to the stack.

## Geospatial data

For the FeatureInfoAgent to work in tandem with visualisations, the geospatial data provided to the visualisaton (via local files or Geoserver), needs to meet certain requirements. The data needs to contain `name`, `iri`, and `endpoint` fields for each individual location.

  * The `name` field needs to contain the human readable name of the feature.
  * The `iri` field needs to contain the full IRI of the feature as represented in the knowledge graph.
  * The `endpoint` field needs to contain the URL of the Blazegraph namespace containing data on the feature. Note that this can be absolute or relative to the FeatureInfoAgent's location.
