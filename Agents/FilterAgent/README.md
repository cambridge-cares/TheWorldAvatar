# Filter Agent
This agent executes a configurable query and returns a list of iris. 
The purpose of this is to provide an endpoint to be used by filtering within the visualization but might have other applications.

## Deployment
This agent requires to be spun up inside a stack. 
Read the [stack manager documentation](../../Deploy/stacks/dynamic/stack-manager/README.md) for instructions on how to do this, the service config can be found [here](./filter-agent.json). 
Something similar to the follow stack config will be needed.

```json
{
    "services": {
        "includes": [
            "filter-agent"
        ]
    },
    "volumes": {
        "filter-queries": "my-filter-queries"
    }
}
```

Here, `my-filter-queries` is a directory containing `.sparql` template queries that will be used for the filtering. An example of such is the following.

```sparql
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?iri WHERE {
  ?type rdfs:label ?typelabel .
  FILTER(CONTAINS(?typelabel, "[class]"))
  ?class rdfs:subClassOf* ?type .
  SERVICE <ontop> {
    ?iri a ?class .
    ?iri rdfs:label ?asset_name .
    FILTER(CONTAINS(?asset_name, "[name]"))
  }
}
```

Here `<ontop>` will automatically be replaced by the relevant internal Ontop URL similar to the [Feature Info Agent](../FeatureInfoAgent/). 
The placeholders `[class]` and `[name]` will be replaced on request. 

Once spun up this agent will have an external endpoint at `localhost:<STACK PORT NUMBER>/filter-agent`

## Use
The following are the configurable parameters of the request to localhost:<STACK PORT NUMBER>/filter-agent/filter.
- `namespace`: The Blazegraph namespace where the query should be sent.
- `query`: Query to be used as a template. 
For example if `nhs_query.sparql` is in your directory this can be set to `nhs_query`.
- `subs`: Set of substitution containing string:value pairs that will be replaced in the query. 
Example can be found [here](./example_input.json). This will naturally need to be URL encoded.

For example the following is an example request.

```http://localhost:3838/filter-agent/filter?namespace=nhs&query=nhs_query&subs=%7B%22class%22%3A%22GP%22%2C%22name%22%3A%22North%22%7D```