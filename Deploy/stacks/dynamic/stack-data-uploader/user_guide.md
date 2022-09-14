# Simple User Guide

A quick guide to upload data.

## Prerequisits

These are the same as listed in [The Stack Manager](./README.md#prerequisites).

You should also initialise the stack by following the instructions in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

## Where to put Data

Download the data you want to upload. Supported file types include

- `tabular` e.g. CSV, JSON
- `vector` e.g. GeoJSON, Shapefile
- `raster` e.g. PNG, JPEG, GIF

Create a directory inside [./inputs/data/](./inputs/data/). You can have multiple files inside subdirectories if you would like. 

## Making a Configuration File

Make a JSON file in [inputs/config/](./inputs/config/) with the following structure
```
{
    "name": "<name you wish to give data>",
    "database": "<database name>",
    "workspace": "<workspace name>",
    "datasetDirectory": "<directory inside ./inputs/data/ where data is stored>",
    "dataSubsets": [
        {
            "type": "<data type as above e.g. if GeoJSON put vector here>",
            "name": "<name you wish to give data subset (might be same as name above if only one)>",
            "skip": false
            "subdirectory": "<if you have data inside subdircetories you can specify here>"
        }
    ]
}
```

Replace all in the `<>` naturally. You can have multiple `dataSubsets` inside the `[]`. Examples can be found in [../example_datasets/inputs/](../example_datasets/inputs/). There are many more options that we can add here but this is the most basic version of such a configuration file.

## Upload and Viewing

Run `./stack.sh start <STACK NAME>` in the [stack-data-uploader](./) directory. Check that data is uploaded using the links and instructions to PostgreSQL or Geoserver in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

## Creating OnTop Mappings

Create a `.obda` file in the directory where the data is stored. It can be layed out in the following way. For more information on how to write the `target` look into [Turtle](https://www.w3.org/TR/turtle/) and SQL for the `source`.

```
[PrefixDeclaration]
ex:     http://example.org/
owl:    http://www.w3.org/2002/07/owl#
rdf:    http://www.w3.org/1999/02/22-rdf-syntax-ns#
xml:    http://www.w3.org/XML/1998/namespace
xsd:    http://www.w3.org/2001/XMLSchema#
foaf:   http://xmlns.com/foaf/0.1/
obda:   https://w3id.org/obda/vocabulary#
rdfs:   http://www.w3.org/2000/01/rdf-schema#
geo:    http://www.opengis.net/ont/geosparql#

[MappingDeclaration] @collection [[
mappingId	<map name 1>
target		example:Something/{thing_id} a example:Something, 
source      SELECT DISTINCT id as owner
            FROM "table_1"

mappingId	<map name 2>
target		ukpn:PowerLine/{line_name} a ukpn:PowerLine ;
                credo:hasName "{line_name}"^^xsd:string .
source      SELECT DISTINCT REPLACE(REPLACE(routename, ' ', '_'), '/', '#slash#') as line_name 
            FROM "33kv-poles-towers"          FROM "33kv-overhead-lines"
```

## Fixes Found

- If you have multilines in a Shapefiles need to add `"ogr2ogrOptions": {"otherOptions": {"-nlt": ["MULTILINESTRING"] }}` to the data subset in the config file.
