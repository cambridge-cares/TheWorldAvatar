# Linked Data Fragments Server <img src="http://linkeddatafragments.org/images/logo.svg" width="100" align="right" alt="" />
On today's Web, Linked Data is published in different ways,
which include [data dumps](http://downloads.dbpedia.org/3.9/en/),
[subject pages](http://dbpedia.org/page/Linked_data),
and [results of SPARQL queries](http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query=CONSTRUCT+%7B+%3Fp+a+dbpedia-owl%3AArtist+%7D%0D%0AWHERE+%7B+%3Fp+a+dbpedia-owl%3AArtist+%7D&format=text%2Fturtle).
We call each such part a [**Linked Data Fragment**](http://linkeddatafragments.org/).

The issue with the current Linked Data Fragments
is that they are either so powerful that their servers suffer from low availability rates
([as is the case with SPARQL](http://sw.deri.org/~aidanh/docs/epmonitorISWC.pdf)),
or either don't allow efficient querying.

Instead, this server offers **[Triple Pattern Fragments](http://www.hydra-cg.com/spec/latest/triple-pattern-fragments/)**.
Each Triple Pattern Fragment offers:

- **data** that corresponds to a _triple pattern_
  _([example](http://data.linkeddatafragments.org/dbpedia?subject=&predicate=rdf%3Atype&object=dbpedia-owl%3ARestaurant))_.
- **metadata** that consists of the (approximate) total triple count
  _([example](http://data.linkeddatafragments.org/dbpedia?subject=&predicate=rdf%3Atype&object=))_.
- **controls** that lead to all other fragments of the same dataset
  _([example](http://data.linkeddatafragments.org/dbpedia?subject=&predicate=&object=%22John%22%40en))_.

This is a **Java** implementation based on Jena. 

## Build
Execute the following command to create a WAR and JAR file:
```
$ mvn install
```
## Deploy stand alone
The server can run with Jetty from a single jar as follows:

    java -jar ldf-server.jar [config.json]

The `config.json` parameters is optional and is default the `config-example.json` file in the same directory as `ldf-server.jar`.

## Deploy on an application server
Use an application server such as [Tomcat](http://tomcat.apache.org/) to deploy the WAR file.

Create an `config.json` configuration file with the data sources (analogous to the example file) and add the following init parameter to `web.xml`:

    <init-param>
      <param-name>configFile</param-name>
      <param-value>path/to/config/file</param-value>
    </init-param>
  
If no parameter is set, it looks for a default `config-example.json` in the folder of the deployed WAR file.

## Status
This software is still under development. It currently supports:
- HDT & Jena TDB data sources
- HTML, Turtle, NTriples, JsonLD, RDF/XML output

A [more complete server](https://github.com/LinkedDataFragments/Server.js/) has been implemented for the Node.js platform.
