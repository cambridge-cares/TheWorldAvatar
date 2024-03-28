## Add Ratings

Ratings should be added with a SPARQL insert query in Blazegraph like:

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX tps: <https://www.theworldavatar.com/kg/ontocitytoilets/>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX schema: <https://schema.org/>

INSERT
{
   <https://www.theworldavatar.com/kg/ontocitytoilets/poi_8ps> tps:hasRating [ a tps:Rating;
                                                                               tps:ratingValue 2;
                                                                               tps:ratingTimestamp "2024-03-27T12:35:56Z"^^xsd:dateTime ;] .
}WHERE{}
```

The rating value can be 1 (bad), 2 (neutral), and 3 (good).
