def step_schema(dynamic_prompt):
    # predeffine empty dictionairies
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
    # fill empty dicitonairy if step type is used
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
    # integrate the above schema parts in the overall schema
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
    schema          = {
                      "type": "json_schema",
                      "json_schema": {
                          "name": "chemicalSynthesis",
                          "schema": {
                              "type": "object",
                              "properties": {
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
                              "required": [
                                  "Add", "HeatChill", "Separate", "Transfer",
                                  "Dry", "Evaporate", "Crystallization",
                                  "Filter", "Sonicate", "Stir", "Dissolve"
                              ],
                              "additionalProperties": False  
                          },
                      "strict": True
                      }
                  }
    return schema
def chemical_schema():
    schema          = {
                    "type": "json_schema",
                    "json_schema": {
                      "name": "chemicalSynthesis",
                      "schema": {
                        "type": "object",
                        "properties": {
                          "synthesisProcedures": {
                            "type": "array",
                            "items": {
                              "type": "object",
                              "properties": {
                                "procedureName": { "type": "string" },
                                "steps": {
                                  "type": "array",
                                  "items": {
                                    "type": "object",
                                    "properties": {
                                      "inputChemicals": {
                                        "type": "array",
                                        "items": {
                                          "type": "object",
                                          "properties": {
                                            "chemical":{ "type": "array",
                                                      "items":{"type":"object",
                                                      "properties":{
                                                        "chemicalFormula": {"type": "string"},
                                                        "chemicalName": { "type": "array",
                                                        "items":{"type":"string", "description": "Name of the chemical as given in the prompt"}},
                                                        "chemicalAmount": {"type": "string", "description": "Amount of the chemcial used in this step."}},
                                                        "required": ["chemicalName", "chemicalAmount", "chemicalFormula"],
                                                      "additionalProperties": False}, 
                                                      "description": "If a mixture of species is used make multiple entries each with chemcial names and chemical amount."},
                                            "supplierName": {"type": "string"},
                                            "purity": { "type": "string" }
                                          },
                                          "required": ["chemical", "supplierName", "purity"],
                                          "additionalProperties": False
                                        }
                                      },
                                      "outputChemical": {
                                        "type": "array",
                                        "items": {
                                          "type": "object",
                                          "properties": {
                                            "chemicalFormula": { "type": "string" },
                                            "names": { "type": "array",
                                                      "items":{"type":"string"}},
                                            "yield": { "type": "string" },
                                            "CCDCNumber": { "type": "string" }
                                          },
                                          "required": ["chemicalFormula", "names", "yield", "CCDCNumber"],
                                          "additionalProperties": False
                                        }
                                      }
                                    },
                                    "required": ["inputChemicals", "outputChemical"],
                                    "additionalProperties": False
                                  }
                                }
                              },
                              "required": ["procedureName", "steps"],
                              "additionalProperties": False
                            }
                          }
                        },
                        "required": ["synthesisProcedures"],
                        "additionalProperties": False
                      },
                      "strict": True
                    }
                  }
    return schema
def cbu_schema():
    schema      ={
                    "type": "json_schema",
                    "json_schema": {
                        "name": "chemicalSynthesis",
                        "schema": {
                        "type": "object",
                        "properties": {
                        "synthesisProcedures": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "mopCCDCNumber": {"type": "string"},
                                "cbuFormula1": {"type": "string"},
                                "cbuSpeciesNames1": {
                                    "type": "array",
                                    "items": {"type": "string"}
                                },
                                "cbuFormula2": {"type": "string"},
                                "cbuSpeciesNames2": { 
                                    "type": "array",
                                    "items": {"type": "string"}
                                }
                            },
                            "required": [
                                "mopCCDCNumber", "cbuFormula1", 
                                "cbuSpeciesNames1", "cbuFormula2", 
                                "cbuSpeciesNames2"
                            ],
                            "additionalProperties": False  
                        }}},
                    "required": ["synthesisProcedures"],
                    "additionalProperties": False},
                    "strict": True
                    }
                }
    return schema
def characterisation_schema():
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
                              "HNMRDevice": {
                                "type": "object",
                                "properties": {
                                  "deviceName": { "type": "string" },
                                  "frequency": { "type": "string" }
                                },
                                "required": ["deviceName", "frequency", "solventNames"],
                                "additionalProperties": False
                              },
                              "ElementalAnalysisDevice": {
                                "type": "object",
                                "properties": {
                                  "deviceName": { "type": "string" }
                                },
                                "required": ["deviceName"],
                                "additionalProperties": False
                              },
                              "InfraredSpectroscopyDevice": {
                                "type": "object",
                                "properties": {
                                  "deviceName": { "type": "string" }
                                },
                                "required": ["deviceName", "solventNames"],
                                "additionalProperties": False
                              },

                                  "Characterisation": {
                                      "type": "array",
                                      "items": {
                                          "type": "object",
                                          "properties": {
                                              "productNames": { "type": "array",
                                                      "items":{"type":"string"}},
                                              "productCCDCNumber": { "type": "string" },
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


