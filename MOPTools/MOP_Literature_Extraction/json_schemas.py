def step_schema(dynamic_prompt):
    """
    Generates a JSON schema for a chemical synthesis procedure based on the steps provided
    in the dynamic_prompt dictionary.
    
    Parameters:
    dynamic_prompt (dict): A dictionary indicating which steps are present in the synthesis.
    (Based on adaptive_schema())
    
    Returns:
    dict: A JSON schema that structures the synthesis steps.
    """
    # Predefine empty dictionaries for each possible synthesis step
    add               = {}
    heat_chill        = {}
    filt              = {}
    crystal           = {}
    stir              = {}
    soni              = {}
    evap              = {}
    dry               = {}
    dissolve          = {}
    separate          = {}
    transfer          = {}
    # Fill the respective dictionary if a specific step is used
    if dynamic_prompt["Add"] == True:
        add.update({"type": "object",
            "properties": {
                "Add": {
                    "type": "object",
                    "properties": {
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "addedChemical":{ "type": "array",
                                        "items":{"type":"object",
                                                "properties":{
                                                    "chemicalName": { "type": "array",
                                                    "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}},
                                                    "chemicalAmount": {"type": "string", "description": "Added amount of the chemcial used in this step."}
                                                },
                                                "required": ["chemicalName", "chemicalAmount"],
                        "additionalProperties": False
                    }, 
                        "description": "If a mixture of species is added make multiple entries each with chemcial names and chemical amount."},
                        "stepNumber": {"type": "integer"},
                        "stir": {"type": "boolean", "description": "true if stired while adding, false otherwise."},
                        "isLayered": {"type": "boolean", "description": "true if added component is layered on top of content in target vessel."},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "duration": {"type": "string", "description": "Time the addition takes. E.g. Added over 5 minutes."},
                        "targetPH": {"type": "number", "description": "If the step involves acidification note target Ph."},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."}
                    },
                    "required": ["usedVesselName", "usedVesselType", "addedChemical", "stepNumber",  "atmosphere", "duration", "stir", "targetPH", "isLayered", "comment"],
                    "additionalProperties": False
                }
            },
            "required": ["Add"],
            "additionalProperties": False
            })
    # Repeat for other steps (e.g., HeatChill, Dry, Filter, etc.)
    if dynamic_prompt["HeatChill"]:
        heat_chill.update({"type": "object",
            "properties": {
                "HeatChill": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string", "description": "Time the vessel is heated or cooled."},
                        "usedDevice": {"type": "string", "description": "Equipment used for heating or cooling."},
                        "targetTemperature": {"type": "string", "description": "Temperature the vessel is heated to."},
                        "heatingCoolingRate": {"type": "string", "description": "Temperature gradient that is applied to heat the vessel. For constant fill in 0 and for reflux state reflux."},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."},
                        "underVacuum": {"type": "boolean", "description": "If the heating is performed under reduced pressure or vacuum."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "sealedVessel": {"type": "boolean", "description": "true if the vessel is sealed. "},
                        "stir": {"type": "boolean", "description": "true if mixture is stirred while heating. "},
                        "stepNumber": {"type": "integer"},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                    },
                    "required": ["duration", "usedDevice", "targetTemperature", "heatingCoolingRate", "underVacuum", "usedVesselName", "usedVesselType", "sealedVessel", "stepNumber", "comment", "atmosphere", "stir"],
                    "additionalProperties": False
                }
            },
            "required": ["HeatChill"],
            "additionalProperties": False})
    if dynamic_prompt["Dry"]:
        dry.update({
            "type": "object",
            "properties": {
                "Dry": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string", "description": "Time the chemical is dried."},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "pressure": {"type": "string", "description": "Pressure applied for drying, often: reduced Pressue, Vacum, etc. "},
                        "temperature": {"type": "string", "description": "Temperature applied for drying."},
                        "stepNumber": {"type": "integer"},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "dryingAgent":{ "type": "array", "description": "Chemical used to support drying.",
                                        "items":{"type":"object",
                                                "properties":{
                                                    "chemicalName": { "type": "array",
                                                    "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}}
                                                },
                                                "required": ["chemicalName"],
                        "additionalProperties": False
                    }}, 
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."}
                    },
                    "required": ["duration", "usedVesselName", "usedVesselType", "stepNumber", "atmosphere", "pressure", "temperature", "comment", "dryingAgent"],
                    "additionalProperties": False
                }
            },
            "required": ["Dry"],
            "additionalProperties": False
        })
    if dynamic_prompt["Filter"]:
        filt.update({"type": "object",
            "properties": {
                "Filter": {
                    "type": "object",
                    "properties": {
                        "washingSolvent":{ "type": "array",
                                        "items":{"type":"object",
                                                "properties":{
                                                    "chemicalName": { "type": "array",
                                                    "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}},
                                                    "chemicalAmount": {"type": "string", "description": "Added amount of the chemcial used in this step."}},
                                                "required": ["chemicalName", "chemicalAmount"],
                        "additionalProperties": False}, 
                        "description": "If a mixture of species is added make multiple entries each with chemcial names and chemical amount."},
                        "vacuumFiltration": {"type": "boolean", "description": "True for vacuum filtration. "},
                        "numberOfFiltrations": {"type": "integer", "description": "Number of filtrations"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "stepNumber": {"type": "integer"},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                    },
                    "required": ["washingSolvent", "numberOfFiltrations", "usedVesselName", "usedVesselType", "stepNumber", "comment", "atmosphere","vacuumFiltration"],
                    "additionalProperties": False
                }
            },
            "required": ["Filter"],
            "additionalProperties": False
        })

    if dynamic_prompt["Sonicate"]:
        soni.update({"type": "object",
            "properties": {
                "Sonicate": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "stepNumber": {"type": "integer"},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                    },
                    "required": ["duration", "usedVesselName", "usedVesselType", "stepNumber", "atmosphere"],
                    "additionalProperties": False
                }
            },
            "required": ["Sonicate"],
            "additionalProperties": False
        })
    if dynamic_prompt["Stir"]:
        stir.update({
            "type": "object",
            "properties": {
                "Stir": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "stepNumber": {"type": "integer"},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "temperature": {"type": "string", "description": "Temperature at which it is stirred."},
                        "wait": {"type": "boolean", "description": "True if stirringrate = 0."},
                    },
                    "required": ["usedVesselName", "usedVesselType", "duration", "stepNumber", "atmosphere", "temperature", "wait"],
                    "additionalProperties": False
                }
            },
            "required": ["Stir"],
            "additionalProperties": False
        })
    if dynamic_prompt["Crystallization"]:
        crystal.update({
            "type": "object",
            "properties": {
                "Crystallization": {
                    "type": "object",
                    "properties": {
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "targetTemperature": {"type": "string"},
                        "stepNumber": {"type": "integer"},
                        "duration": {"type": "string"},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."}
                    },
                    "required": ["usedVesselName", "usedVesselType", "targetTemperature", "duration", "comment", "atmosphere", "stepNumber"],
                    "additionalProperties": False
                }
            },
            "required": ["Crystallization"],
            "additionalProperties": False
        })
    if dynamic_prompt["Evaporate"]:
        evap.update({"type": "object",
            "properties": {
                "Evaporate": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "pressure": {"type": "string"},
                        "temperature": {"type": "string"},
                        "stepNumber": {"type": "integer"},
                        "rotaryEvaporator": {"type": "boolean", "description": "True if rotary evaporator is used."},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "removedSpecies":{ "type": "array",
                                        "items":{"type":"object",
                                                "properties":{
                                                    "chemicalName": { "type": "array",
                                                    "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}}},
                                                "required": ["chemicalName"],
                        "additionalProperties": False}, 
                        "description": "Species that is removed by evaporation."},
                        "targetVolume": {"type": "string", "description": "Volume to which mixture is evaporated."},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."}
                    },
                    "required": ["duration", "usedVesselName", "usedVesselType", "atmosphere", "stepNumber", "pressure", "temperature", "removedSpecies", "rotaryEvaporator", "targetVolume",  "comment"],
                    "additionalProperties": False
                }
            },
            "required": ["Evaporate"],
            "additionalProperties": False
        })
    if dynamic_prompt["Dissolve"]:
        dissolve.update({"type": "object",
            "properties": {
                "Dissolve": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "solvent":{ "type": "array",
                                        "items":{"type":"object",
                                                "properties":{
                                                    "chemicalName": { "type": "array",
                                                    "items":{"type":"string", "description": "Name of the chemical as given in the prompt. Make sure to not include multiple species."}},
                                                    "chemicalAmount": {"type": "string", "description": "Amount of the chemcial used in this step."}},
                                                "required": ["chemicalName", "chemicalAmount"],
                        "additionalProperties": False}, 
                        "description": "If a mixture of species is used make multiple entries each with chemcial names and chemical amount."},
                        "stepNumber": {"type": "integer"},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."}
                    },
                    "required": ["duration", "usedVesselName", "usedVesselType", "stepNumber", "solvent", "atmosphere", "comment"],
                    "additionalProperties": False
                }
            },
            "required": ["Dissolve"],
            "additionalProperties": False
        })
    if dynamic_prompt["Separate"]:
        separate.update({"type": "object",
            "properties": {
                "Separate": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "solvent":{ "type": "array",
                                        "items":{"type":"object",
                                                "properties":{
                                                    "chemicalName": { "type": "array",
                                                    "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}},
                                                    "chemicalAmount": {"type": "string", "description": "Amount of the chemcial used in this step."}},
                                                "required": ["chemicalName", "chemicalAmount"],
                        "additionalProperties": False}, 
                        "description": "If a mixture of species is used make multiple entries each with chemcial names and chemical amount."},
                        "stepNumber": {"type": "integer"},
                        "separationType": {"type": "string", "description": "Separation type that is performed.",
                        "enum": ["extraction", "washing", "column", "centrifuge"]},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."}
                    },
                    "required": ["duration", "usedVesselName", "usedVesselType", "atmosphere", "stepNumber", "solvent", "separationType", "comment"],
                    "additionalProperties": False
                }
            },
            "required": ["Separate"],
            "additionalProperties": False
        })
    if dynamic_prompt["Transfer"]:
        transfer.update({"type": "object",
            "properties": {
                "Transfer": {
                    "type": "object",
                    "properties": {
                        "duration": {"type": "string"},
                        "usedVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "usedVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "targetVesselName": {"type": "string", "description": "Generic vessel name, e.g. vessel 1."},
                        "targetVesselType": {"type": "string", "description": "One of 7 vessel types.",
                        "enum": ["Teflon-lined stainless-steel vessel", "glass vial", "quartz tube", "round bottom flask", "glass scintillation vial", "pyrex tube", "schlenk flask"]},
                        "stepNumber": {"type": "integer"},
                        "isLayered": {"type": "boolean", "description": "true if transfered component is layered on top of content in target vessel."},
                        "transferedAmount": {"type": "string", "description": "volume or mass that is transfered if given."},
                        "comment": {"type": "string", "description": "Information that does not fit any other entry."},
                        "atmosphere": {"type": "string", "description": "indicates if step is conducted under N2 or Ar atmosphere.", 
                                        "enum": ["N2", "Ar", "Air", "N/A"]},
                    },
                    "required": ["duration", "usedVesselName", "atmosphere", "usedVesselType", "targetVesselName", "isLayered", "targetVesselType", "stepNumber", "transferedAmount", "comment"],
                    "additionalProperties": False
                }
            },
            "required": ["Transfer"],
            "additionalProperties": False
        })
    # Schema dictionary containing all defined step schemas
    step_schema_dict        = {
                      "type": "json_schema",
                      "json_schema": {
                          "name": "synthesis",
                          "schema": {
                              "type": "object",
                              "properties": {
                                  "Synthesis": {
                                      "type": "array",
                                      "items": {
                                          "type": "object",
                                          "properties": {
                                              "productNames": { "type": "array",
                                                      "items":{"type":"string"}},
                                              "productCCDCNumber": {"type": "string"},
                                              "steps": {
                                                  "type": "array",
                                                  "items": {
                                                      "type": "object",
                                                      "anyOf": [  
                                                          add,
                                                          heat_chill,
                                                          filt,
                                                          crystal,
                                                          stir,
                                                          soni,
                                                          evap,
                                                          dry,
                                                          dissolve, 
                                                          transfer, 
                                                          separate
                                                      ]
                                                  }
                                              }
                                          },
                                          "required": ["productNames", "productCCDCNumber", "steps"],
                                          "additionalProperties": False
                                      }
                                  }
                              },
                              "required": ["Synthesis"],
                              "additionalProperties": False
                          },
                          "strict": True
                      }
                  }
    return step_schema_dict

def adaptive_schema():
    """
    Generates a JSON schema defining the structure for a chemical synthesis process.
    Lists all possible synthesis step types used in the synthesis to pass it to the
    llm and get a boolean list with the step types used in a synthesis to generate 
    tailored schemas based on the currently processed synthesis procedure.
    The schema ensures that only specific boolean properties related to synthesis
    steps are allowed, and all properties are required. (OpenAi requirements)

    Returns:
        dict: A dictionary representing the JSON schema.
    """
    schema          = {
                      "type": "json_schema",                            # Specifies the type of schema
                      "json_schema": {
                          "name": "chemicalSynthesis",                  # Name of the schema
                          "schema": {
                              "type": "object",                         # Defines the schema as an object type
                              "properties": {                           # Defines the allowed properties and their types
                                "Add": {"type": "boolean"},
                                "HeatChill": {"type": "boolean"},
                                "Dry": {"type": "boolean"},
                                "Evaporate": {"type": "boolean"},
                                "Filter": {"type": "boolean"},
                                "Sonicate": {"type": "boolean"},
                                "Stir": {"type": "boolean"},
                                "Crystallization": {"type": "boolean"},
                                "Dissolve": {"type": "boolean"},
                                "Separate": {"type": "boolean"},
                                "Transfer": {"type": "boolean"}
                              },
                              "required": [                             # Specifies the required properties
                                  "Add", "HeatChill", "Separate", "Transfer",
                                  "Dry", "Evaporate", "Crystallization",
                                  "Filter", "Sonicate", "Stir", "Dissolve"
                              ],
                              "additionalProperties": False             # Disallows any properties not explicitly defined
                          },
                      "strict": True                                    # Ensures strict adherence to the schema
                      }
                  }
    return schema                                                       # Returns the defined schema

def chemical_schema():
    """
    Defines a JSON schema for chemical synthesis procedures.
    This schema ensures structured data representation for synthesis procedures,
    including input chemicals, output chemicals.
    
    Returns:
        dict: JSON schema defining the structure of chemical in a synthesis procedure.
    """
    schema          = {
                    "type": "json_schema",                                                          # Specifies that this schema follows JSON Schema standards
                    "json_schema": {            
                      "name": "chemicalSynthesis",                                                  # Name of the schema
                      "schema": {           
                        "type": "object",                                                           # Root element must be an object
                        "properties": {         
                          "synthesisProcedures": {                                                  # Main key containing synthesis procedure details
                            "type": "array",                                                        # List of synthesis procedures
                            "items": {          
                              "type": "object",                                                     # Each procedure is an object
                              "properties": {
                                "procedureName": { "type": "string" },                              # Name of the procedure
                                "steps": {
                                  "type": "array",                                      
                                  "items": {
                                    "type": "object",                                               # Each step is an object
                                    "properties": {
                                      "inputChemicals": {                                           # Input chemicals
                                        "type": "array",
                                        "items": {
                                          "type": "object",                                         # Each input chemical is an object
                                          "properties": {
                                            "chemical":{ "type": "array",                           # Details of chemicals used
                                                      "items":{"type":"object",
                                                      "properties":{
                                                        "chemicalFormula": {"type": "string"},      # Input Chemical formula
                                                        "chemicalName": { "type": "array",          # List of input chemical names
                                                        "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}},
                                                        "chemicalAmount": {"type": "string", "description": "Amount of the chemcial used in this step."}},
                                                        "required": ["chemicalName", "chemicalAmount", "chemicalFormula"],
                                                      "additionalProperties": False}, 
                                                      "description": "If a mixture of species is used make multiple entries each with chemcial names and chemical amount."},
                                            "supplierName": {"type": "string"},                     # Supplier information for input chemicals
                                            "purity": { "type": "string" }
                                          },
                                          "required": ["chemical", "supplierName", "purity"],       # Required fields for input chemicals
                                          "additionalProperties": False                             # No extra properties allowed
                                        }
                                      },
                                      "outputChemical": {                                           # Output chemical(s)
                                        "type": "array",
                                        "items": {
                                          "type": "object",
                                          "properties": {
                                            "chemicalFormula": { "type": "string" },                # Formula of the output chemical
                                            "names": { "type": "array",                             # List of output chemical names
                                                      "items":{"type":"string"}},
                                            "yield": { "type": "string" },                          # Yield of the reaction
                                            "CCDCNumber": { "type": "string" }                      # CCDC number (if applicable)
                                          },
                                          "required": ["chemicalFormula", "names", "yield", "CCDCNumber"],
                                          "additionalProperties": False
                                        }
                                      }
                                    },
                                    "required": ["inputChemicals", "outputChemical"],               # Required fields for each step
                                    "additionalProperties": False                                   # No extra properties allowed in step
                                  }
                                }
                              },
                              "required": ["procedureName", "steps"],                               # Required fields for each procedure
                              "additionalProperties": False                                         # No extra properties allowed in procedure
                            }
                          }
                        },
                        "required": ["synthesisProcedures"],                                        # The root object must have synthesis procedures
                        "additionalProperties": False                                               # No extra properties allowed at the root level
                      },
                      "strict": True                                                                # Enforces strict validation
                    }
                  }
    return schema
def cbu_schema():
    """
    Returns a JSON schema defining the structure for chemical synthesis data.
    
    The schema enforces a strict structure to ensure consistency in stored data.
    """
    schema      ={
                    "type": "json_schema",                                      # Indicates that this is a JSON schema definition
                    "json_schema": {
                        "name": "chemicalSynthesis",                            # Name of the schema
                        "schema": {
                        "type": "object",                                       # Root element is an object
                        "properties": {
                        "synthesisProcedures": {                                # Key representing synthesis procedures
                        "type": "array",                                        # Must be an array
                        "items": {
                            "type": "object",                                   # Each item in the array is an object
                            "properties": {
                                "mopCCDCNumber": {"type": "string"},            # Unique identifier (better than MOP name)
                                "cbuFormula1": {"type": "string"},              # First chemical building unit formula
                                "cbuSpeciesNames1": {
                                    "type": "array",                            # List of species names
                                    "items": {"type": "string"}                 # Each species name is a string
                                },
                                "cbuFormula2": {"type": "string"},              # Second chemical building unit formula
                                "cbuSpeciesNames2": { 
                                    "type": "array",                            # List of species names
                                    "items": {"type": "string"}                 # Each species name is a string
                                }
                            },
                            "required": [                                       # Fields that must be present in each object
                                "mopCCDCNumber", "cbuFormula1", 
                                "cbuSpeciesNames1", "cbuFormula2", 
                                "cbuSpeciesNames2"
                            ],
                            "additionalProperties": False                       # Prevents unspecified properties
                        }}},
                    "required": ["synthesisProcedures"],                        # Top-level required field
                    "additionalProperties": False},                             # Disallows extra fields at the root level
                    "strict": True                                              # Ensures strict validation
                    }
                }
    return schema
def characterisation_schema():
    """
    Defines a JSON schema for characterisation devices and their associated characterisation data.
    
    Returns:
        dict: A dictionary representing the JSON schema for characterisation devices.
    """
    schema      = {    
                      "type": "json_schema",
                      "json_schema": {
                          "name": "characterisationDevices",
                          "schema": {
                      "type": "object",
                      "properties": {
                        "Devices": {
                          "type": "array",
                          "items": {
                            "type": "object",
                            "properties": {
                            # Device schema for HNMR (Hydrogen Nuclear Magnetic Resonance)
                              "HNMRDevice": {
                                "type": "object",
                                "properties": {
                                  "deviceName": { "type": "string" },
                                  "frequency": { "type": "string" }
                                },
                                "required": ["deviceName", "frequency", "solventNames"],
                                "additionalProperties": False
                              },
                              # Device schema for Elemental Analysis
                              "ElementalAnalysisDevice": {
                                "type": "object",
                                "properties": {
                                  "deviceName": { "type": "string" }
                                },
                                "required": ["deviceName"],
                                "additionalProperties": False
                              },
                              # Device schema for Infrared Spectroscopy
                              "InfraredSpectroscopyDevice": {
                                "type": "object",
                                "properties": {
                                  "deviceName": { "type": "string" }
                                },
                                "required": ["deviceName", "solventNames"],
                                "additionalProperties": False
                              },
                                # Characterisation data for products
                                  "Characterisation": {
                                      "type": "array",
                                      "items": {
                                          "type": "object",
                                          "properties": {
                                              "productNames": { "type": "array",
                                                      "items":{"type":"string"}},
                                              "productCCDCNumber": { "type": "string" },
                                              # HNMR characterisation details
                                              "HNMR": {
                                                  "type": "object",
                                                  "properties": {
                                                      "shifts": { "type": "string" },
                                                      "solvent": { "type": "string" },
                                                      "temperature": { "type": "string" }
                                                  },
                                                  "required": ["shifts", "solvent", "temperature"],
                                                  "additionalProperties": False
                                              },
                                              # Elemental Analysis characterisation details
                                              "ElementalAnalysis": {
                                                  "type": "object",
                                                  "properties": {
                                                      "weightPercentageCalculated": { "type": "string" },
                                                      "weightPercentageExperimental": { "type": "string" },
                                                      "chemicalFormula": { "type": "string" }
                                                  },
                                                  "required": ["weightPercentageCalculated", "weightPercentageExperimental", "chemicalFormula", "measurementDevice"],
                                                  "additionalProperties": False
                                              },
                                              # Infrared Spectroscopy characterisation details
                                              "InfraredSpectroscopy": {
                                                  "type": "object",
                                                  "properties": {
                                                      "material": { "type": "string" },
                                                      "bands": { "type": "string" }
                                                  },
                                                  "required": ["material", "bands"],
                                                  "additionalProperties": False
                                              }
                                          },
                                          "required": ["productNames", "productCCDCNumber", "HNMR", "ElementalAnalysis", "InfraredSpectroscopy"],
                                          "additionalProperties": False
                                      }
                                  }
                             
                            },
                            "required": ["HNMRDevice", "ElementalAnalysisDevice", "InfraredSpectroscopyDevice", "Characterisation"],
                            "additionalProperties": False
                          }
                        }
                      },
                      "required": ["Devices"],
                      "additionalProperties": False
                    }}

                  }
    return schema


