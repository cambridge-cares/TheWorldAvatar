import os
import sys
import re
import fitz  # PyMuPDF
import json
from openai import OpenAI
import tiktoken
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
from processing.rework_ontomops import update_kg as KG

class PdfConverter:
    """
    A class to handle the conversion of PDF files to text files.

    Attributes:
    input_pdf_path (str):       The path to the input PDF file.
    output_folder_path (str):   The path to the output folder where the text file will be saved.
    """
    
    def __init__(self, input_pdf_path: str, output_folder_path: str):
        self.input_pdf_path         = os.path.abspath(input_pdf_path)
        self.output_folder_path     = os.path.abspath(output_folder_path)
        self.validate_paths()

    def validate_paths(self):
        """
        Validates the provided paths.

        Raises:
        FileNotFoundError:      If the input PDF file does not exist.
        NotADirectoryError:     If the output folder path does not exist.
        """
        if not os.path.isfile(self.input_pdf_path):
            raise FileNotFoundError(f"The input PDF file does not exist: {self.input_pdf_path}")
        if not os.path.isdir(self.output_folder_path):
            raise NotADirectoryError(f"The output folder does not exist: {self.output_folder_path}")

    def get_output_txt_path(self) -> str:
        """
        Constructs the output text file path.

        Returns:
        str:                    The path to the output text file.
        """
        base_name                   = os.path.basename(self.input_pdf_path)
        txt_file_name               = os.path.splitext(base_name)[0] + ".txt"
        return os.path.join(self.output_folder_path, txt_file_name)

    def convert_pdf_to_text(self) -> str:
        """
        Converts the PDF file to a text file.

        Returns:
        str:                    The path to the output text file.
        """
        output_txt_path     = self.get_output_txt_path()
        text                = self.extract_text_from_pdf()
        self.save_text_to_file(text, output_txt_path)
        return text, output_txt_path

    def extract_text_from_pdf(self) -> str:
        """
        Extracts text from the PDF file.

        Returns:
        str:                    The extracted text.
        """
        text = ""
        pdf_document    = fitz.open(self.input_pdf_path)
        for page_num in range(len(pdf_document)):
            page        = pdf_document.load_page(page_num)
            text       += page.get_text()
        return text

    def save_text_to_file(self, text: str, output_path: str):
        """
        Saves the extracted text to a file.

        Parameters:
        text (str):             The extracted text to save.
        output_path (str):      The path to the output text file.
        """
        with open(output_path, "w", encoding="utf-8") as txt_file:
            txt_file.write(text)

    def read_text_file(self, file_path: str) -> str:
        """
        Reads a text file and returns its contents as a string.

        Parameters:
        file_path (str): The path to the text file.

        Returns:
        str: The contents of the text file.

        Raises:
        FileNotFoundError: If the file does not exist.
        IOError: If an error occurs while reading the file.
        """
        if not os.path.isfile(file_path):
            raise FileNotFoundError(f"The file does not exist: {file_path}")

        try:
            with open(file_path, "r", encoding="utf-8") as file:
                content = file.read()
        except IOError as e:
            raise IOError(f"An error occurred while reading the file: {file_path}. Error: {e}")

        return content
            


class ChatGPTAPI:
    """
    A class to interact with the OpenAI API using the provided API key.

    Attributes:
    api_key (str):              The API key for OpenAI.
    client (OpenAI):            The OpenAI client instance.
    """
    
    def __init__(self):
        self.api_key    = self.read_api_key_from_file()
        self.client     = OpenAI(api_key=self.api_key)

    def send_request(self, pdf_text: str, prompt: str, model_name: str, json_out: int, dynamic_prompt) -> str:
        """
        Sends a request to the OpenAI API with the given text and prompt.

        Parameters:
        pdf_text (str):         The text extracted from the PDF.
        prompt (str):           The prompt to send to the OpenAI API.
        model_name (str):       The OpenAI model name.

        Returns:
        str:                    The response from the OpenAI API.
        """
        
        match  json_out:
            case 12:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]

              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format={
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
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Add": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "addedChemicalName": {"type": "string"},
                                                                          "addedChemicalAmount": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "addedDropwise": {"type": "boolean"},
                                                                          "atmosphere": {"type": "string"},
                                                                          "additionTime": {"type": "string"}
                                                                      },
                                                                      "required": ["usedVesselName", "usedVesselType", "addedChemicalName", "addedChemicalAmount", "stepNumber", "addedDropwise", "atmosphere", "additionTime"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Add"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "HeatChill": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "heatCoolingTime": {"type": "string"},
                                                                          "usedDevice": {"type": "string"},
                                                                          "targetTemperature": {"type": "string"},
                                                                          "heatingCoolingRate": {"type": "string"},
                                                                          "heatingCoolingRateComment": {"type": "string"},
                                                                          "underVacuum": {"type": "boolean"},
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "sealedVessel": {"type": "boolean"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "atmosphere": {"type": "string"}
                                                                      },
                                                                      "required": ["heatCoolingTime", "usedDevice", "targetTemperature", "heatingCoolingRate", "underVacuum", "usedVesselName", "usedVesselType", "sealedVessel", "stepNumber", "heatingCoolingRateComment", "atmosphere"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["HeatChill"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Filter": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "washingSolventName": {"type": "string"},
                                                                          "washingSolventAmount": {"type": "string"},
                                                                          "repetitions": {"type": "integer"},
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "filterComment": {"type": "string"},
                                                                          "atmosphere": {"type": "string"}
                                                                      },
                                                                      "required": ["washingSolventName", "washingSolventAmount", "repetitions", "usedVesselName", "usedVesselType", "stepNumber", "filterComment", "atmosphere"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Filter"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Crystallization": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "targetTemperature": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "crystallizationTime": {"type": "string"},
                                                                          "crystallizationComment": {"type": "string"}
                                                                      },
                                                                      "required": ["usedVesselName", "usedVesselType", "targetTemperature", "crystallizationTime", "crystallizationComment", "stepNumber"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Crystallization"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Stir": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "stirringTime": {"type": "string"},
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "atmosphere": {"type": "string"}
                                                                      },
                                                                      "required": ["usedVesselName", "usedVesselType", "stirringTime", "stepNumber", "atmosphere"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Stir"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Sonicate": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "sonicationTime": {"type": "string"},
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"}
                                                                      },
                                                                      "required": ["sonicationTime", "usedVesselName", "usedVesselType", "stepNumber"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Sonicate"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Evaporate": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "evaporationTime": {"type": "string"},
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "pressure": {"type": "string"},
                                                                          "temperature": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "remainingVolume": {"type": "string"}
                                                                      },
                                                                      "required": ["evaporationTime", "usedVesselName", "remainingVolume", "usedVesselType", "stepNumber", "pressure", "temperature"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Evaporate"],
                                                              "additionalProperties": False
                                                          },
                                                          {
                                                              "type": "object",
                                                              "properties": {
                                                                  "Dry": {
                                                                      "type": "object",
                                                                      "properties": {
                                                                          "dryingTime": {"type": "string"},
                                                                          "usedVesselName": {"type": "string"},
                                                                          "usedVesselType": {"type": "string"},
                                                                          "pressure": {"type": "string"},
                                                                          "temperature": {"type": "string"},
                                                                          "stepNumber": {"type": "integer"},
                                                                          "vacuumType": {"type": "string"},
                                                                      },
                                                                      "required": ["dryingTime", "usedVesselName", "usedVesselType", "stepNumber", "pressure", "temperature"],
                                                                      "additionalProperties": False
                                                                  }
                                                              },
                                                              "required": ["Dry"],
                                                              "additionalProperties": False
                                                          }
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
                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )

            case 17:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
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
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]

              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format={
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
                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )


            case 11:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]

              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format={
                      "type": "json_schema",
                      "json_schema": {
                          "name": "characterisation",
                          "schema": {
                              "type": "object",
                              "properties": {
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
                                                      "chemicalFormula": { "type": "string" },
                                                      "measurementDevice": { "type": "string" }
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
                                          "required": ["productName", "productCCDCNumber", "HNMR", "ElementalAnalysis", "InfraredSpectroscopy"],
                                          "additionalProperties": False
                                      }
                                  }
                              },
                              "required": ["Characterisation"],
                              "additionalProperties": False
                          },
                          "strict": True
                      }
                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )
            case 13:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]
              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format={    
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

                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )
            case 14:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]
              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format=   {
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
                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )
            case 15:
              full_prompt       = f"{prompt}"
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]
              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format={
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
                                  "mopFormula": {"type": "string"},
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
                                  "mopFormula", "cbuFormula1", 
                                  "cbuSpeciesNames1", "cbuFormula2", 
                                  "cbuSpeciesNames2"
                              ],
                              "additionalProperties": False  
                          }}},
                        "required": ["synthesisProcedures"],
                        "additionalProperties": False},
                      "strict": True
                      }
                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )
            case 18:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
              messages    =[
              {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
              {"role": "user", "content": full_prompt} 
              ]
              response = self.client.chat.completions.create(
                  model=model_name,
                  response_format={
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
                  },
                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )
            case _:
              full_prompt       = f"{prompt}\n\n{pdf_text}"
              print("default prompt")
              messages    =[
              {"role": "system","content": "You will be provided with synthesis text and your task is to extract text based on the instruction."},
              {"role": "user", "content": full_prompt} ]
              response        = self.client.chat.completions.create(
                                  model=model_name,
                                  messages=messages,
                  temperature=0.2,  # Adds controlled randomness
                  top_p=0.1  # Limits the selection of probability mass
              )
        return response.choices[0].message.content

    @staticmethod
    def count_tokens_and_calculate_cost(input_text: str, model_name: str, cost_per_token: float) -> dict:
        """
        Counts the number of tokens generated for the input text using a specified OpenAI model and calculates the cost.

        Parameters:
        input_text (str):       The input text to be tokenized.
        model_name (str):       The OpenAI model name.
        cost_per_token (float): The cost per token in dollars.

        Returns:
        dict:                   A dictionary containing the number of tokens and the estimated cost.
        """
        # Get the appropriate encoding for the specified model
        encoding        = tiktoken.encoding_for_model(model_name)
        # Encode the input text to get the tokens
        num_tokens      = len(encoding.encode(input_text))
        # Calculate the estimated cost
        estimated_cost  = num_tokens * cost_per_token
        # Return the number of tokens and the estimated cost
        print(f"Estimated costs for given input: {estimated_cost} for the provided: {num_tokens} tokens.")
        return {"num_tokens": num_tokens, "estimated_cost": estimated_cost}
    
    def read_api_key_from_file(self) -> str:
        """
        Reads the API key from a file named 'api_key.txt' located in a 'secrets' directory.

        Returns:
        str: The API key read from the file.

        Raises:
        FileNotFoundError: If the 'api_key.txt' file does not exist.
        """
        secrets_dir = os.path.join(os.getcwd(), "secrets")
        api_key_file_path = os.path.join(secrets_dir, "api_key.txt")
        
        if not os.path.isfile(api_key_file_path):
            raise FileNotFoundError(f"The API key file does not exist: {api_key_file_path}")
        
        with open(api_key_file_path, "r", encoding="utf-8") as file:
            api_key = file.read().strip()
            print(api_key)
        
        return api_key


def process_pdf_with_openai(input_pdf_path: str, output_folder_path: str, prompt: str, iteration: int = 0) -> str:
    """
    Processes a PDF file by extracting text and sending it to the OpenAI API recursively.

    Parameters:
    input_pdf_path (str):           The path to the input PDF file.
    output_folder_path (str):       The path to the output folder for saving the text file.
    prompt (str):                   The prompt to send to the OpenAI API.
    iteration (int):                The current iteration number (default is 0).

    Returns:
    str: The response from the OpenAI API.
    """
    print("Current recursion depth: ",iteration)
    if iteration >= 2:  # Limit the recursion depth to avoid infinite recursion
        return ""

    # Convert PDF to text
    converter               = PdfConverter(input_pdf_path, output_folder_path)
    text, output_txt_path   = converter.convert_pdf_to_text()

    # Send the PDF text to ChatGPT
    chatgpt_api             = ChatGPTAPI()
    cost                    = 5e-6
    model_name              = "gpt-4o-2024-05-13"
    chatgpt_api.count_tokens_and_calculate_cost(text, model_name, cost)
    response                = chatgpt_api.send_request(text, prompt, model_name)

    # Save the response
    response_output_path    = os.path.join(output_folder_path, f"response_iteration_{iteration}.txt")
    converter.save_text_to_file(response, response_output_path)
    return process_pdf_with_openai(response_output_path, output_folder_path, new_prompt, iteration + 1)

def get_chemicals(doi:str) -> dict:
    # initialize KG class
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
    # make file path dependent on script location
    a_box_updates_config                = KG.config_a_box_updates(os.path.join(script_dir,"../OntoMOPConnection.env"))
    # instantiate class
    updater = KG.UpdateKG(
        query_endpoint                  = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint                 = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                         = a_box_updates_config.KG_USERNAME,
        kg_password                     = a_box_updates_config.KG_PASSWORD
    )
    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]
    where_mops                          = f"""  ?MOPIRI         om:hasProvenance            ?ProvenanceIRI                  ;
                                                                om:hasCCDCNumber            ?CCDCNum                        ;
                                                                om:hasMOPFormula            ?Formula                        .
                                            ?ProvenanceIRI	    om:hasReferenceDOI          "{doi}"                . """
    select_mops                         = """?Formula ?CCDCNum """
    mop_cbu                             = updater.query_triple(where_mops, select_mops)
    return mop_cbu
def get_literature(doi:str) -> dict:
    # initialize KG class
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
    # make file path dependent on script location
    a_box_updates_config                = KG.config_a_box_updates(os.path.join(script_dir,"../OntoMOPConnection.env"))
    # instantiate class
    updater = KG.UpdateKG(
        query_endpoint                  = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint                 = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                         = a_box_updates_config.KG_USERNAME,
        kg_password                     = a_box_updates_config.KG_PASSWORD
    )
    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]
    where_mops                          = f"""  ?MOPIRI         om:hasProvenance            ?ProvenanceIRI                  ;
                                                                om:hasCCDCNumber            ?CCDCNum                        ;
                                                                om:hasMOPFormula            ?Formula                        .
                                            ?ProvenanceIRI	    om:hasReferenceDOI          "{doi}"                . """
    select_mops                         = """?Formula ?CCDCNum """
    mop_cbu                             = updater.query_triple(where_mops, select_mops)
    return mop_cbu

def chemicals(doi:str) -> dict:
    # initialize KG class
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
    # make file path dependent on script location
    a_box_updates_config                = KG.config_a_box_updates(os.path.join(script_dir,"../OntoSynthesisConnection.env"))
    # instantiate class
    updater = KG.UpdateKG(
        query_endpoint                  = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint                 = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                         = a_box_updates_config.KG_USERNAME,
        kg_password                     = a_box_updates_config.KG_PASSWORD
    )
    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]

    query                               = f"""
    PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

    SELECT (GROUP_CONCAT(DISTINCT ?label; SEPARATOR=", ") AS ?labels) 
    WHERE {{	
        ?Species <http://www.w3.org/2000/01/rdf-schema/label> ?label .
        ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?InputChemical osyn:referencesMaterial ?Material .   
      	?ChemicalSynthesis osyn:hasChemicalInput ?InputChemical .
        ?ChemicalSynthesis osyn:retrievedFrom ?doc .
        ?doc <http://purl.org/ontology/bibo/doi> "{doi}" .
    }}
group by ?Species
                                              """        
    species_labels                        = updater.sparql_client.performQuery(query) 
    print("species labels: ", species_labels)
    species_list                          = []
    for species in species_labels:
      species_list.append(species["labels"])
    return species_list
    # go thorugh all the papers and save them:
    batch_download              = False
    if batch_download:
        for literature_doi in literature_dois:
            print(literature_doi["DOI"])
            if literature_doi["DOI"]=="Not in OntoMOPs KG":
                continue
            GetPaper.GetPapers.get_paper(literature_doi["DOI"])
def extract_bracket_substrings(input_string):
    # Use regex to find all substrings within square brackets
    substrings = re.findall(r'\[[^\]]*\]', input_string)
    
    if len(substrings) >= 2:
        substring1 = substrings[0]
        substring2 = substrings[1]
        return substring1, substring2
    else:
        return None, None
def input_for_cbu(doi:str) -> dict:
    # initialize KG class
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
    # make file path dependent on script location
    a_box_updates_config                = KG.config_a_box_updates(os.path.join(script_dir,"../OntoSynthesisConnection.env"))
    # instantiate class
    updater = KG.UpdateKG(
        query_endpoint                  = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint                 = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                         = a_box_updates_config.KG_USERNAME,
        kg_password                     = a_box_updates_config.KG_PASSWORD
    )
    mops                                = get_literature(doi)
    species_list                        = []
    cbu_list                            = []
    mop_list                            = []

    for mop in mops:
      print("mops: ", mop)
      cbu_list.append({extract_bracket_substrings(mop["Formula"])})
      query                               = f"""
      PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX bibo: <http://purl.org/ontology/bibo/>
      PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>	


      SELECT distinct 
            (GROUP_CONCAT(DISTINCT ?label; separator=", ") AS ?allLabels) 
      WHERE {{
      ?ChemicalSynthesis 	osyn:retrievedFrom 			?doc 			.
        ?doc				bibo:doi					?provenance 	.
      ?doc <http://purl.org/ontology/bibo/doi> 		?doi 			.
        ?transform osyn:isDescribedBy 	?ChemicalSynthesis ;
                  osyn:hasChemicalOutput ?output 			. 
        ?output 	osyn:isRepresentedBy 	?MOP	.
        ?MOP 		mop:hasCCDCNumber		"{mop["CCDCNum"]}" .
        ?ChemicalSynthesis osyn:hasChemicalInput ?InputChemical.
        
        
        ?InputChemical osyn:referencesMaterial ?Material 	.
        ?Species skos:altLabel ?label .
        ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?PhaseComponent	<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty>	?PhaseComponentConc	.
        ?PhaseComponentConc om:hasValue		?concval					.
        ?concval	om:hasNumericalValue ?concnum			;
                  <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure>	?cunit					.
        ?cunit	rdfs:label		?concunit				.
        }}
        GROUP BY ?Species  ?doi ?ChemicalSynthesis """        
      cbu_labels                        = updater.sparql_client.performQuery(query) 
      species_list.append(cbu_labels)
      mop_list.append(mop["CCDCNum"])

    print(cbu_list, species_list)
    return mop_list, cbu_list, species_list

def query_mop_names(doi:str):
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
    # make file path dependent on script location
    a_box_updates_config                = KG.config_a_box_updates(os.path.join(script_dir,"../OntoSynthesisConnection.env"))
    # instantiate class
    updater = KG.UpdateKG(
        query_endpoint                  = a_box_updates_config.SPARQL_QUERY_ENDPOINT,
        update_endpoint                 = a_box_updates_config.SPARQL_UPDATE_ENDPOINT,
        kg_user                         = a_box_updates_config.KG_USERNAME,
        kg_password                     = a_box_updates_config.KG_PASSWORD)
    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]
    query                               = f"""
    PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
            PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
            PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX mops: <https://www.theworldavatar.com/kg/ontomops/>
    PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
    SELECT ?lab
    WHERE {{
      
      ?chemicalTransformation   osyn:hasChemicalOutput 	      ?chemicalOutput 	  ;
                                osyn:isDescribedBy		        ?chemicalSynthesis 	.
      ?chemicalOutput	          skos:altLabel                    ?lab             .
      ?chemicalSynthesis	      osyn:retrievedFrom 		        ?document			      . 
      ?document 			          <http://purl.org/ontology/bibo/doi>   "{doi}"		  .
          }}
                                              """        
    species_labels                        = updater.sparql_client.performQuery(query) 
    print("species labels: ", species_labels)
    species_list                          = []
    for species in species_labels:
      species_list.append(species["lab"])
    return species_list
    
def process_papers():
    script_dir              = os.path.dirname(os.path.abspath(__file__))
    #processor               = XYZFileProcessor(os.path.abspath(os.path.join(script_dir, "../../Data/papers_with_si")), os.path.join(script_dir, "../../Data/"))
    #processor.process_files_in_directory(processor.transform_xyz_string)
    #processor.process_files_in_directory(processor.prepend_line_count)

def append_si_to_paper(directory):
  # List all files in the directory
  files = os.listdir(directory)
  
  for file_name in files:
      # Check if the file is a .txt file and does not already end with '_si.txt'
      if file_name.endswith('.txt') and not file_name.endswith('_si.txt'):
          base_name = file_name.rsplit('.txt', 1)[0]
          si_file_name = f"{base_name}_si.txt"
          
          # Check if the corresponding _si.txt file exists
          si_file_path = os.path.join(directory, si_file_name)
          if si_file_name in files:
              original_file_path = os.path.join(directory, file_name)
              
              # Append the content of the _si.txt file to the original file
              with open(original_file_path, 'a', encoding='utf-8') as original_file, \
                    open(si_file_path, 'r', encoding='utf-8') as si_file:
                  original_file.write("\n\n")  # Ensure a newline before appending
                  original_file.write(si_file.read())
              
              # Delete the _si.txt file after appending its content
              os.remove(si_file_path)
              print(f"Appended {si_file_name} to {file_name} and deleted {si_file_name}")


# Example usage:
if __name__ == "__main__":
    print("main")