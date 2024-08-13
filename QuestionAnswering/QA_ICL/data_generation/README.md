# Data generation for QA applications using in-context learening

## Introduction

This directory contains scripts to generate three kinds of data: lexicon, simplified graph schema, and examples of data request generation.

### Lexicon

Within the context of this project, a lexicon of a class, a predicate, or an entity is defined as a collection of its text representations, which include its canonical label and surface forms. 

In the main application, lexicons are used to perform:

- entity linking (including fuzzy search over surface forms and vector similarity search)
- lookup of canonical labels (useful when `rdfs:label` information is not easy to locate due to the distribution of data across different servers and namespaces)

See [JSON schema for lexicons](#data-schema-of-lexicons).

### KG schema extracts

This directory adopts the terminology used for heterogenous graphs `G=(V, E, τ, φ)`:

- For each node `v ∈ V`, its node type is `τ(v)`.
- For each edge `(u, v) ∈ E`,
  - its edge type is `φ(u, v)`, which is equivalent to 'predicate' or 'property' in RDF terms;
  - its relation type is tuple `(τ(u), φ(u, v), τ(v))`.

The simplified data schema captures only node types, edge types, and relation types, and disregard other schema information such as RDFS class hierarchy. 

See [JSON schema for KG schema extracts](#data-schema-of-kg-schema-extracts).

### Examples of data request generation from natural language questions

A question can be answered so long as appropriate data is supplied. The retrieval of pertinent data is carried out by executing structured data requests that correspond to the input questions. By prompting LLMs with pairs of natural language queries and their corresponding data requests, LLMs can perform in-context learning to automatically generate data requests for unseen input questions.

Data requests come in two variants: SPARQL query and function call. 

See [JSON schema for examples of data request generation](#data-schema-of-data-request-generation-examples).

## Installation

### Prerequisites
- python>=3.8
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

- Generate lexicon for all entities of a base class using only `rdfs:label` data: [lexicon/lexicon_from_label.py](lexicon/lexicon_from_label.py).
- Generate lexicon for all classes that inherit from a base class using only `rdfs:label` data: [lexicon/lexicon_from_owl.py](lexicon/lexicon_from_owl.py).
- Synthesise lexicon with LLM:
  - Optionally, retrieve all literal data associated with entities of a class first: [lexicon/retrieve_literal_data.py](lexicon/retrieve_literal_data.py).
  - Then, run [lexicon/lexicon_from_llm.py](lexicon/lexicon_from_llm.py).
- Generate lexicon with scripts specific to entity types:
  - Entities of `purl:Element` type: [lexicon/Element_lexicon.py](lexicon/Element_lexicon.py).
  - Entities of `disp:Ship` type: [lexicon/Ship_lexicon.py](lexicon/Ship_lexicon.py).

### Generation of KG schema extracts

- Generate an extract of edge types from an ABox exposed via a SPARQL endpoint: [simplified_schema/extract_edgetypes_from_tbox.py](simplified_schema/extract_edgetypes_from_abox.py).
- Generate an extract of relation types from OWL files: [simplified_schema/extract_relations_from_tbox.py](simplified_schema/extract_schema_from_tbox.py).
- Generate an extract of relation types from an ABox exposed via a SPARQL endpoint: [simplified_schema/extract_relations_from_abox.py](simplified_schema/extract_schema_from_abox.py)

### Augmentation of data request examples

Augmentation refers to the process of artificially synthesising new data from existing data. 

- With an OpenAI-compatible API: [make_examples.py](make_examples.py).
- With a locally hosted Mixtral 8x7B model: [make_examples.sh](make_examples.sh).

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

### Data schema of KG schema extracts

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

### Data schema of data request generation examples

Schema definition of SPARQL data request forms:

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

Schema definition for function call data request forms:

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

Schema definition of data requests:

```{json}
{
  "$id": "/schemas/data_request",
  "title": "DataRequest",
  "properties": {
    "entity_bindings": {
      "type": "object",
      "additionalProperties": { 
        "type": "object",
        "properties": {
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
  },
  "required": ["req_form"]
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