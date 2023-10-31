# SpaceFinder

## Usage
Merge this directory with [stack directory](../dynamic/) and replace `README`s in data directories with files detailed. 
Then spin up a stack called `pirmasens` on port `3838`.
Need the usual secrets for the default stack as well as the Mapbox tokens.

## Filtering
If not filtering on particular variable put in as `"'null'"`.
For filtering the requests should resemble the following
- Filter on area only http://localhost:3838/filter-agent/filter?subs=%7B%22area%22%3A%225000%22%2C%22zonetype%22%3A%22%27Industry%27%2C%20%27Automeile%27%22%7D
- Filter on zone type only http://localhost:3838/filter-agent/filter?subs=%7B%22area%22%3A%22%27null%27%22%2C%22zonetype%22%3A%22%27Industry%27%22%7D
- Filter on both area and zone type http://localhost:3838/filter-agent/filter?subs=%7B%22area%22%3A%225000%22%2C%22zonetype%22%3A%22%27Industry%27%22%7D

To get all zone types can run the following query in blazegraph
```sparql
PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX plt:     <https://www.theworldavatar.com/kg/ontoplot/>
PREFIX zone:    <https://www.theworldavatar.com/kg/ontozoning/>

SELECT DISTINCT ?ZoneType WHERE {
    SERVICE <http://psdt-ontop:8080/sparql/> {
        ?zone zone:hasPlot [a plt:Plot];
            zone:hasZoneType [rdfs:label ?ZoneType] .
    }
} 
```