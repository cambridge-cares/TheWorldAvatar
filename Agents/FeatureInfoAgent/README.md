# Feature Info Agent

This agent acts as a single access point for visualisations, allowing them to query for data on an individual feature (i.e. location). When a HTTP request is received, this agent uses the IRI within the request to query the relevant knowledge graphs and/or relational databases to return relevant feature metadata that the visualisation can then display.

**Note: This version of the agent is a VERY rough implementation and should not be used outside of very specific use cases. It currently relies on hardcoded SPARQL queries, and will not work out-of-the box for any old data sets. Future improvements will make this more generic, at which point the agent could be used more widely.**


## Request Format

Requests to the FeatureInfo Agent should contain a JSON string with the following format:

```
{
	"iri": "FEATURE-IRI",
	"endpoint": "KNOWLEDGE-GRAPH-ENDPOINT"
}
```