# Data Generation for QA_ICL

## Table of Contents

- [Data Generation for QA\_ICL](#data-generation-for-qa_icl)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Terminologies](#terminologies)
    - [Lexicon](#lexicon)
    - [Schema Elements](#schema-elements)
    - [Data Request Generation Examples](#data-request-generation-examples)
      - [SparqlDataRequestForm Schema](#sparqldatarequestform-schema)
      - [FuncDataRequestForm Schema](#funcdatarequestform-schema)
      - [DataRequest Schema](#datarequest-schema)
      - [Nlq2DataReqExample Schema](#nlq2datareqexample-schema)
    - [Physical Quantity Recognition Examples](#physical-quantity-recognition-examples)
  - [Installation](#installation)
    - [Prerequisites](#prerequisites)
    - [Steps](#steps)
  - [Usage](#usage)
    - [Lexicon Generation](#lexicon-generation)
    - [KG Schema Extraction](#kg-schema-extraction)
    - [CSV-to-JSON Conversion of Data Request Generation Examples](#csv-to-json-conversion-of-data-request-generation-examples)
    - [Augmentation of Data Request Generation](#augmentation-of-data-request-generation)
    - [CSV-to-JSON Conversion of Physical Quantity Detection Examples](#csv-to-json-conversion-of-physical-quantity-detection-examples)
  
## Overview

This directory contains Python and shell scripts to prepare three kinds of data required by Marie's backend: lexicons, graph schemas, and examples of data request generation.

- Lexicons: help with entity linking
- Graph schemas: help with predicting SPARQL queries that look for information needed by users
- Examples of data request generation: provide demonstrations for how user queries ought to be mapped to structured data requests.
- Examples of physical quantity recognition: provide demonstrations for how to extract mentions of physical quantities from input texts.
 
Specifically, the scripts in this directory provide the following functionalities:

- Generate lexicons from ABoxes or TBoxes for all instances of an particular `rdf:type`.

- Extract schema elements from ABoxes or TBoxes.

- Convert the format of labelled examples, including examples for data request generation and physical quantity recognition, from CSV to JSON.
  
  When labelled examples are hand-crafted by human developers, it is more convenient to do so in a spreadsheet than crafting a JSON file from scratch. The spreadsheet can then be exported into the CSV format and converted to JSON so that it can be loaded into the backend application.

- Augment data request generation examples with synthetic entries.

  One of the factors that influence the success of in-context learning with demonstration retrieval is the diversity of in-context demonstrations. Using an LLM, an initially small set of examples can be augmented to enhance its size and diversity.


## Terminologies

### Lexicon

Within the context of this project, a lexicon of a class, a predicate, or an entity is defined as a collection of its text representations, which include its canonical label and surface forms. 

See JSON schema for lexicons below.

```{json}
{
  "$id": "/schemas/lexicon",
  "title": "Lexicon",
  "type": "object",
  "properties": {
    "iri": {
      "description": "IRI (Internationalized Resource Identifier) within an RDF graph",
      "type": "string"
    },
    "cls": {
      "description": "Class tag, not necessarily the same as `rdf:type`",
      "type": "string"
    },
    "label": {
      "description": "Canonical label",
      "type": "string"
    },
    "surface_forms": {
      "description": "",
      "type": "array",
      "items": {
        "type": "string"
      }
    }
  },
  "required": ["iri", "cls", "label"]
}
```

### Schema Elements

The `simplified_schema` directory adopts the terminology used for heterogeneous graphs `G=(V, E, τ, φ)`, where G is the entire graph, V is the set of all vertices (nodes) in the graph, E is the set of all edges in the graph, τ is a function that assigns types to nodes, and φ is a function that assigns types to edges. The following relationships can be derived:

- For each node v in the set of vertices V (i.e. `v ∈ V`), `τ(v)` represents the node type of v.
- For each edge (u, v) in the set of edges E (i.e. `(u, v)∈ E`),
  - Its edge type is `φ(u, v)`, which is equivalent to 'predicate' or 'property' in RDF terms;
  - Its relation type is tuple `(τ(u), φ(u, v), τ(v))`, representing the type of the source node, the edge type, and the type of the target node, respectively.

The simplified data schema captures only node types, edge types, and relation types, and disregards other schema information such as RDFS class hierarchy.

Schema definition for node type and edge type data:

```{json}
{
  "$id": "/schemas/graph_item_type",
  "title": "GraphItemType",
  "type": "object",
  "properties": {
   "iri": {
      "description": "IRI (Internationalized Resource Identifier) within an RDF graph",
      "type": "string"
   },
   "label": {
      "description": "`rdfs:label` attribute",
      "type": "string"
   },
   "comment": {
    "description": "`rdfs:comment` attribute",
      "type": "string"
   }
  },
  "required": ["iri"]
}
```

Schema definition for relation type data:

```{json}
{
  "$id": "/schemas/relation_type",
  "title": "RelationType",
  "type": "object",
  "properties": {
    "s": {
      "description": "Head node type",
      "type": "string"
    },
    "p": {
      "description": "Edge type",
      "type": "string"
    },
    "o": {
      "description": "Tail node type",
      "type": "string"
    }
  },
  "required": ["s", "p", "o"]
}
```

### Data Request Generation Examples

A question can be answered so long as appropriate data is supplied. The retrieval of pertinent data is carried out by executing structured data requests that correspond to the input questions. By prompting LLMs with pairs of natural language queries and their corresponding data requests, LLMs can perform in-context learning to automatically generate data requests for unseen input questions.

Data requests come in two variants: SPARQL query and function call. To represent these concepts, we use a hierarchy of interconnected JSON schemas. Below is an overview of these schemas and their relationships:

1. Base schemas for data request forms
   - `SparqlDataRequestForm`: Defines the structure for SPARQL-based data requests.
   - `FuncDataRequestForm`: Defines the structure for function call-based data requests.
2. Composite schemas
   - `DataRequest`: Incorporates either a `SparqlDataRequestForm` or a `FuncDataRequestForm`, along with additional metadata.
   - `Nlq2DataReqExample`: Represents a complete example, pairing a natural language query with its corresponding DataRequest.

Here's a visual representation of the schema relationships:

```
Nlq2DataReqExample
└── DataRequest
    ├── SparqlDataRequestForm
    └── FuncDataRequestForm
```

Now, let's examine each schema in detail:

#### SparqlDataRequestForm Schema

This schema defines the structure for SPARQL-based data requests.

```{json}
{
  "$id": "/schemas/sparql_data_request_form",
  "title": "SparqlDataRequestForm",
  "type": "object",
  "properties": {
    "type": {
      "const": "sparql"
    },
    "namespace": {
      "description": "Namespace that uniquely identifies a SPARQL endpoint",
      "type": "string"
    },
    "query": {
      "description": "SPARQL query",
      "type": "string"
    },
    "res_map": {
      "description": "Configuration that determines the transformation of a SPARQL SELECT response to a document collection",
      "type": "object",
      "additionalProperties": {
        "type": "object",
        "properties": {
          "pkey": {
            "description": "If true, group bindings with the same value of this key; if false, aggregate the values into a list",
            "type": "boolean"
          },
          "cls": {
            "description": "Class tag",
            "type": "string"
          } 
        }
      }
    }
  },
  "required": [
    "type",
    "namespace",
    "query"
  ]
}
```

#### FuncDataRequestForm Schema

This schema defines the structure for function call-based data requests.

```{json}
{
  "$id": "/schemas/func_data_request_form",
  "title": "FuncDataRequestForm",
  "type": "object",
  "properties": {
    "type": {
      "const": "func"
    },
    "name": {
      "description": "Function name",
      "type": "string"
    },
  },
  "required": ["type", "name"]
}
```

#### DataRequest Schema

This schema incorporates either a `SparqlDataRequestForm` or a `FuncDataRequestForm`, along with additional metadata.

```{json}
{
  "$id": "/schemas/data_request",
  "title": "DataRequest",
  "properties": {
    "var2cls": {
      "description": "Mappings of variables to their class labels",
      "type": "object",
      "additionalProperties": {
        "type": "string"
      }
    },
    "entity_bindings": {
      "description": "",
      "type": "object",
      "additionalProperties": { 
        "type": "array",
        "items": {
          "anyOf": [
            { 
              "description": "Surface form",
              "type": "string" 
            },
            { 
              "description": "Key-value pairs of specific entity identifiers",
              "type": "object",
              "additionalProperties": {
                "type": "string"
              }
            }
          ]
        }
      }
    },
    "const_bindings": {
      "type": "object",
      "additionalProperties": true
    },
    "req_form": {
      "oneOf": [
        { "$ref": "/schemas/sparql_data_request" },
        { "$ref": "/schemas/func_data_request" }
      ]
    }
  }
}
```

#### Nlq2DataReqExample Schema

This schema represents a complete example, pairing a natural language query with its corresponding `DataRequest`.

```{json}
{
  "$id": "/schemas/nlq2datareq_example",
  "title": "Nlq2DataReqExample",
  "type": "object",
  "properties": {
    "nlq": {
      "description": "Natural language question",
      "type": "string"
    },
    "data_req": {
      "description": "Data request corresponding to the given input query",
      "$ref": "/schemas/data_request"
    }
  },
  "required": ["nlq", "data_req"]
}
```

### Physical Quantity Recognition Examples

For science and engineering, the units of physical quantities must be handled properly to ensure sound reasoning. In the context of a QA system, any physical quantities mentioned by the user must align with the unit system of the machine system. To this end, physical quantities in user inputs need to be extracted and parsed into their constituent magnitudes and units for further processing. Using the in-context learning method, an LLM can be instructed to recognise physical quantities based on labelled examples.

JSON schema for examples of physical quantity recognition:

```{json}
{
  "$id": "/schemas/qtRecog_example",
  "title": "QtRecogExample",
  "type": "object",
  "properties": {
    "text": {
      "description": "Natural language text",
      "type": "string"
    },
    "prediction": {
      "type": "object",
      "properties": {
        "template": {
          "description": "The natural language text with all physical quantities replaces with \{\}",
          "type": "string"
        },
        "quantities": {
          "description": "Detected physical quantities",
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "description": "Physical quantity",
              "type": {
                "description": "Physical quantity type e.g. molecular_weight, boiling_point",
                "type": "string"
              },
              "value": {
                "description": "Magnitude of the physical quantity",
                "type": "number"
              },
              "unit": {
                "description": "Unit of the physical quantity e.g. degC",
                "type": "string"
              }
            }
            "required": ["value"]
          }
        }
      },
      "required": ["template", "quantities"]
    },
    "required": ["text"]
  }
}
```


## Installation

### Prerequisites

- python>=3.8
- (optional but recommended) [conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html)

### Steps

1. Create a Virtual Environment (if using conda):
   ```{bash}
   conda create --name data-env python==3.8
   source activate data-env
   ```
2. Install Dependencies: Navigate to the `data_generation` directory and install the required Python packages:
   ```{bash}
   pip install -r requirements.txt
   ```


## Usage

Most scripts in this directory are in Python. The command line arguments supported by a script can be enquired by running it with the `-h` flag, e.g. `python lexicion/lexicon_from_label.py -h`. Below, we give an overview of the available scripts.

### Lexicon Generation

- Generate a lexicon for all entities of a base class using only `rdfs:label` data: [lexicon/lexicon_from_label.py](lexicon/lexicon_from_label.py).

- Generate a lexicon for all classes that inherit from a base class using only `rdfs:label` data: [lexicon/lexicon_from_owl.py](lexicon/lexicon_from_owl.py).

- Synthesise a lexicon with LLM:
  - Optionally, retrieve all literal data associated with entities of a class first: [lexicon/retrieve_literal_data.py](lexicon/retrieve_literal_data.py).
  - Then, run [lexicon/lexicon_from_llm.py](lexicon/lexicon_from_llm.py).

- Generate lexicons specific to an entity type:
  - Entities of `purl:Element` type: [lexicon/Element_lexicon.py](lexicon/Element_lexicon.py).

### KG Schema Extraction

- Extract edge type info from an ABox exposed via a SPARQL endpoint: [simplified_schema/extract_edgetypes_from_tbox.py](simplified_schema/extract_edgetypes_from_tbox.py).
- Extract relation type info from OWL files: [simplified_schema/extract_relations_from_tbox.py](simplified_schema/extract_relations_from_tbox.py).
- Extract relation type info from an ABox exposed via a SPARQL endpoint: [simplified_schema/extract_relations_from_abox.py](simplified_schema/extract_relations_from_abox.py)

### CSV-to-JSON Conversion of Data Request Generation Examples

[nlq2datareq_examples/csv2json.py](nlq2datareq_examples/csv2json.py)

### Augmentation of Data Request Generation

- With an OpenAI-compatible API: [nlq2datareq_examples/make_examples.py](nlq2datareq_examples/make_examples.py).
- With a locally hosted Mixtral 8x7B model: [nlq2datareq_examples/make_examples.sh](nlq2datareq_examples/make_examples.sh).

### CSV-to-JSON Conversion of Physical Quantity Detection Examples

[qtRecog_examples/csv2json.py](qtRecog_examples/csv2json.py)
