import os
import json
from openai import OpenAI
import utils
import parameters as para
import json_schemas as jschem
import llm_prompts as llmp
import secret_parameter as spara

class ChatGPTAPI:
    """
    A class to interact with the OpenAI API using the provided API key.

    Attributes:
    api_key (str):              The API key for OpenAI.
    client (OpenAI):            The OpenAI client instance.
    """
    
    def __init__(self):
        self.api_key    = spara.API_KEY
        self.client     = OpenAI(api_key=self.api_key)
    def make_llm_call(self, message):
        response = self.client.chat.completions.create(
        model=para.MODEL_NAME,
        messages=message,
        temperature=0.2,  # Adds controlled randomness
        top_p=0.1  # Limits the selection of probability mass
              )
        return response.choices[0].message.content
    def make_llm_call_structured(self, prompt, schema):
        message     =[
        {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
        {"role": "user", "content": prompt} ]   
        response    = self.client.chat.completions.create(
        model=para.MODEL_NAME,
        response_format=schema,
        messages=message,
        temperature=0.2,  # Adds controlled randomness
        top_p=0.1  # Limits the selection of probability mass
              )
        return response.choices[0].message.content


def extract_chemicals(file_path, output_dir):
    # get doi
    doi                                 = utils.doi_from_path(file_path)
    # read in synthesis text 
    with open(file_path, "r", encoding="utf-8") as file:
      file_content                      = file.read()
    prompt                              = llmp.chemical_prompt(doi)
    full_prompt                         = f"{prompt}\n\n{file_content}"
    chatgpt_api                         = ChatGPTAPI()
    schema                              = jschem.chemical_schema()
    response                            = chatgpt_api.make_llm_call_structured(full_prompt, schema)
    # regenerate file name from doi
    name                = doi.replace("/", "_")
    # write to file
    with open(output_dir+"/"+name+".json", "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 

def extract_steps(file_path, output_dir):
    doi                                 = utils.doi_from_path(file_path) 
    # read the file content        
    with open(file_path, "r", encoding="utf-8") as file:
            file_content                = file.read()
    # get schema to extract expected step types
    schema_adaptive                     = jschem.adaptive_schema()
    # get the prompt string
    prompt_step_types                   = llmp.step_types_prompt()
    # combine prompt and synthesis text:
    adaptive_prompt                     = f"{prompt_step_types}\n\n{file_content}"
    # make api call
    chatgpt_api                         = ChatGPTAPI()
    response                            = chatgpt_api.make_llm_call_structured(adaptive_prompt, schema_adaptive)
    # load json file from llm as input
    dynamic_prompt                      = json.loads(response)
    # adjust prompt based on LLM output
    prompt                              = llmp.step_prompt(doi, dynamic_prompt)
    # add the synthesis text to prompt
    full_prompt                         = f"{prompt}\n\n{file_content}"
    # get schema
    schema                              = jschem.step_schema(dynamic_prompt)
    # make llm call
    response_steps                      = chatgpt_api.make_llm_call_structured(full_prompt, schema)
    # regenerate file name from doi
    name                                = doi.replace("/", "_")
    # write to file
    with open(output_dir+"/"+name+".json", "w", encoding="utf-8") as txt_file:
        txt_file.write(response_steps) 

def extract_pre_steps(file_path, output_dir):
    # get doi
    doi                                 = utils.doi_from_path(file_path)        
    with open(file_path, "r", encoding="utf-8") as file:
            file_content                = file.read()
    chatgpt_api                         = ChatGPTAPI()
    # use no dynamic prompt
    prompt                              = llmp.pre_steps_prompt()
    full_prompt                         = f"{prompt}\n\n{file_content}"
    messages    =[
      {"role": "system","content": "You will be provided with synthesis text and your task is to extract text based on the instruction."},
      {"role": "user", "content": full_prompt} ]
    response                            = chatgpt_api.make_llm_call(messages)    
    # regenerate file name from doi
    name                = doi.replace("/", "_")
    # write to file
    with open(output_dir+"/"+name+".txt", "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 

def extract_procedure(file_path, output_dir):
    # get doi
    doi                         = utils.doi_from_path(file_path)            
    with open(file_path, "r", encoding="utf-8") as file:
            file_content        = file.read()
    prompt                      = llmp.procedure_prompt(doi)
    full_prompt                 = f"{prompt}\n\n{file_content}"
    messages    =[
      {"role": "system","content": "You will be provided with synthesis text and your task is to extract text based on the instruction."},
      {"role": "user", "content": full_prompt} ]
    chatgpt_api                 = ChatGPTAPI()
    response                    = chatgpt_api.make_llm_call(messages)
    # write to file
    # regenerate file name from doi
    name                = doi.replace("/", "_")
    with open(output_dir+"/"+name+".txt", "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 
        
def extract_cbu(file_path, output_dir):
    # get doi
    doi                         = utils.doi_from_path(file_path)            
    with open(file_path, "r", encoding="utf-8") as file:
            file_content        = file.read()
    prompt, successful          = llmp.cbu_prompt(doi)
    if not successful:
        return
    chatgpt_api                 = ChatGPTAPI()
    schema                      = jschem.cbu_schema()
    response                    = chatgpt_api.make_llm_call_structured(prompt, schema)
    # regenerate file name from doi
    name                = doi.replace("/", "_")
    # write to file
    with open(output_dir+"/"+name+".json", "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 

def extract_characterization(file_path, output_dir):
    # get doi
    doi                         = utils.doi_from_path(file_path)          
    with open(file_path, "r", encoding="utf-8") as file:
      file_content        = file.read()
    chatgpt_api             = ChatGPTAPI()
    prompt                  = llmp.characterisation_prompt(doi)
    full_prompt             = f"{prompt}\n\n{file_content}"
    schema                  = jschem.characterisation_schema
    response                = chatgpt_api.make_llm_call_structured(full_prompt, schema)
    # regenerate file name from doi
    name                = doi.replace("/", "_")
    # write to file
    with open(output_dir+"/"+name+".json", "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 
