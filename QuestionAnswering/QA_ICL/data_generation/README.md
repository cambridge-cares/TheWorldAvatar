# Data Generation for TWA Question-Answering Applications using In-Context Learening

## Introduction

This directory contains scripts to generate three classes of data: lexicon, simplified data schema, and examples of data request generation.

### Lexicon

Within the context of this project, a lexicon of an entity is defined as a collection of its text representations, which include its canonical label and surface forms. Its schema definition is given below.

```{json}
{
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

In the main application, lexicons are used to perform:

- entity linking (including fuzzy search over surface forms and vector similarity search)
- lookup of canonical labels (useful when `rdfs:label` information is not easy to locate due to the distribution of data across servers and namespaces)

### Simplified Data Schema

This project adopts the terminology used for heterogenous graphs `G=(V, E, τ, φ)`:

- for each node `v ∈ V`, its node type is `τ(v)`
- for each edge `(u, v) ∈ E`,
  - its edge type is `φ(u, v)`
  - its relation type is tuple `(τ(u), φ(u, v), τ(v))`.

The simplified data schema captures only node types, edge types, and relation types, and disregard other schema information such as inheritance. 

Schema definition for node type and edge type data:

```{json}
{
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

### Data Request Example

Data requests come in two variants: SPARQL query and function call. 

```
{
  "title": "DataRequest",
  "type": "object",
  "oneOf": [
    {
      "title": "SparqlDataRequest",
      "properties": {
        "action_type": {
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
        "action_type",
        "namespace",
        "query"
      ]
    },
    {
      "title": "FuncDataRequest",
      "properties": {
        "action_type": {
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
      "required": ["action_type", "name"]
    }
  ]
}
```

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

For all scripts in this directory, run `python {path_to_script} -h` to view the command line argument options.

### Lexicon generation

- Generate lexicon with only `rdfs:label` data: `make_lexicon/retrieve_label.py`.
- Synthesise lexicon with LLM:
  - Optionally, retrieve all literal data associated with entities of a class first: `make_lexicon/retrieve_literal_data.py`.
  - Then, run `make_lexicon/from_llm.py`.
- Generate lexicon with scripts specific to entity types:
  - Entities of `purl:Element` type: `make_lexicon/Element.py`.
  - Entities of `disp:Ship` type: `make_lexicon/Ship.py`.