# Filter Agent
This agent executes a configurable query and returns a list of iris. 
The purpose of this is to provide an endpoint to be used by filtering within the visualization but might have other applications.

## Deployment
This agent must be spun up as part of a stack deployment workflow.
The Filter Agent is a built-in stack service
A stack-manager config similar to the following will deploy this agent:

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

Spin up a stack as per the [documented workflow](../../Deploy/stacks/dynamic/stack-manager/README.md). 
The filter agent should be spun up along with the entire stack. 
There is no need to build or deploy the filter agent and its image separately.

Here, `my-filter-queries` is a directory containing `.sparql` template queries that will be used for the filtering. An example of such is the following.

```sparql
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?iri WHERE {
  SERVICE <http://nhs-blazegraph:8080/blazegraph/namespace/nhs/sparql> {
    ?type rdfs:label ?typelabel .
    FILTER(CONTAINS(?typelabel, "[class]"))
    ?class rdfs:subClassOf* ?type .
    SERVICE <ontop> {
      ?iri a ?class .
      ?iri rdfs:label ?asset_name .
      FILTER(CONTAINS(?asset_name, "[name]"))
    }
  }
}
```

Here `<ontop>` will automatically be replaced by the relevant internal Ontop URL similar to the [Feature Info Agent](../FeatureInfoAgent/). 
The placeholders `[class]` and `[name]` will be replaced on request. 

Once spun up this agent will have an external endpoint at `localhost:<STACK PORT NUMBER>/filter-agent`

## Use
The following are the configurable parameters of the request to localhost:<STACK PORT NUMBER>/filter-agent/filter.
- `namespace`: The Blazegraph namespace where the query should be sent.
This defaults to the value specified by the `DEFAULT_NAMESPACE` environment variable and is set to `kb` in the default stack config.
- `query`: Query to be used as a template. 
For example if `nhs_query.sparql` is in your directory this can be set to `nhs_query`.
This defaults to the value specified by the `DEFAULT_QUERY` environment variable and is set to `query` in the default stack config.
- `subs`: Set of substitution containing string:value pairs that will be replaced in the query. 
Example can be found [here](./example_input.json). 
This will naturally need to be URL encoded.

For example the following is an example request.

```http://localhost:3838/filter-agent/filter?namespace=nhs&query=nhs_query&subs=%7B%22class%22%3A%22GP%22%2C%22name%22%3A%22North%22%7D```