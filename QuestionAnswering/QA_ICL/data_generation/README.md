# Data Generation for TWA Question-Answering Applications using In-Context Learening

## Introduction

This directory contains scripts to generate three kinds of data: lexicon, simplified graph schema, and examples of data request generation.

### Lexicon

Within the context of this project, a lexicon of a class, a predicate, or an entity is defined as a collection of its text representations, which include its canonical label and surface forms. 

In the main application, lexicons are used to perform:

- entity linking (including fuzzy search over surface forms and vector similarity search)
- lookup of canonical labels (useful when `rdfs:label` information is not easy to locate due to the distribution of data across different servers and namespaces)

See [JSON schema for lexicons](#data-schema-of-lexicons).

### Simplified graph schema

This project adopts the terminology used for heterogenous graphs `G=(V, E, τ, φ)`:

- For each node `v ∈ V`, its node type is `τ(v)`.
- For each edge `(u, v) ∈ E`,
  - its edge type is `φ(u, v)`;
  - its relation type is tuple `(τ(u), φ(u, v), τ(v))`.

The simplified data schema captures only node types, edge types, and relation types, and disregard other schema information such as RDFS class hierarchy. 

See [JSON schema for simplified graph schema](#data-schema-of-simplified-graph-schema).

### Examples of data request generation from natural language questions

A question can be answered so long as appropriate data is supplied. The retrieval of pertinent data is carried out by executing structured data requests that correspond to the input questions. By prompting LLMs with pairs of natural language queries and their corresponding data requests, LLMs can perform in-context learning to automatically generate data requests for unseen input questions.

Data requests come in two variants: SPARQL query and function call. 

See [JSON schema for examples of data request generation](#data-schema-of-data-request-generation-examples).

## Installation

### Prerequisites
- `python>=3.8`
- (optional but recommended) [conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html)

### Steps

0. If a virtual environment manager is used, create a new python environment and activate it. In the case of conda:
   ```{bash}
   conda create --name data-env python==3.8
   source activate data-env
   ```
1. Install dependendencies.
   ```{bash}
   pip install -r requirements.txt
   ```

## Usage

For all scripts in this directory, run `python <path_to_script> -h` to view the command line argument options.

### Generation of lexicons

- Generate lexicon for all entities of a base class using only `rdfs:label` data: `lexicon/lexicon_from_label.py`.
- Generate lexicon for all classes that inherit from a base class using only `rdfs:label` data: `lexicon/lexicon_from_owl.py`.
- Synthesise lexicon with LLM:
  - Optionally, retrieve all literal data associated with entities of a class first: `lexicon/retrieve_literal_data.py`.
  - Then, run `lexicon/lexicon_from_llm.py`.
- Generate lexicon with scripts specific to entity types:
  - Entities of `purl:Element` type: `lexicon/Element_lexicon.py`.
  - Entities of `disp:Ship` type: `lexicon/Ship_lexicon.py`.

### Generation of simplified graph schema

- Generate simplified graph schema from OWL files: `simplified_schema/extract_schema_from_tbox.py`.
- Generate simplified grpah schema from a SPARQL endpoint: `simplified_schema/extract_schema_from_abox.py`

### Augmentation of data request examples

Augmentation refers to the process of artificially synthesising new data from existing data. 

- With an OpenAI-compatible API: `make_examples.py`.
- With a locally hosted Mixtral 8x7B model: `make_examples.sh`.

## Schema definitions

### Data schema of lexicons

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

### Data schema of simplified graph schema

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
   "annotations": {
      "description": "Information such as `rdfs:label` and `rdfs:comment`",
      "type": "object"
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

### Data schema of data request generation examples

Schema definition of SPARQL data requests:

```{json}
{
  "$id": "/schemas/sparql_data_request",
  "title": "SparqlDataRequest",
  "type": "object",
  "properties": {
    "type": {
      "const": "sparql"
    },
    "namespace": {
      "description": "Namespace that uniquely identifies a SPARQL endpoint",
      "type": "string"
    },
    "bindings": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "var": {
            "description": "Variable name",
            "type": "string"
          },
          "cls": {
            "description": "Class tag",
            "type": "string"
          },
          "values": {
            "type": "array",
            "items": {
              "type": "object",
              "properties": {
                "text": {
                  "description": "Surface form",
                  "type": "string"
                },
                "identifier": {
                  "description": "Key-value pairs of specific entity identifiers",
                  "type": "object"
                }
              }
            }
          }
        }
      }
    },
    "query": {
      "description": "SPARQL query",
      "type": "string"
    },
    "nodes_to_augment": {
      "description": "Additional class-specific data will be retrieved for these nodes after the execution of the main SPARQL query",
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "var": {
            "description": "Variable name",
            "type": "string"
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

Schema definition for function call data requests:

```{json}
{
  "$id": "/schemas/func_data_request",
  "title": "FuncDataRequest",
  "type": "object",
  "properties": {
    "type": {
      "const": "func"
    },
    "name": {
      "description": "Function name",
      "type": "string"
    },
    "args": {
      "description": "Arguments to pass into the function call",
      "type": "object"
    }
  },
  "required": ["type", "name"]
}
```

Schema definition of data requests:

```{json}
{
  "$id": "/schemas/data_request",
  "title": "DataRequest",
  "oneOf": [
    { "$ref": "/schemas/sparql_data_request" },
    { "$ref": "/schemas/func_data_request" }
  ]
}
```

Schema definition of data request generation examples:

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