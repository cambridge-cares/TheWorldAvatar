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

    def send_request(self, pdf_text: str, prompt: str, model_name: str, json_out: bool) -> str:
        """
        Sends a request to the OpenAI API with the given text and prompt.

        Parameters:
        pdf_text (str):         The text extracted from the PDF.
        prompt (str):           The prompt to send to the OpenAI API.
        model_name (str):       The OpenAI model name.

        Returns:
        str:                    The response from the OpenAI API.
        """
        full_prompt       = f"{prompt}\n\n{pdf_text}"
        if json_out:
          messages    =[
          {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into json format."},
          {"role": "user", "content": full_prompt} 
          ]

          response = self.client.chat.completions.create(
              model=model_name,
              response_format={
                  "type": "json_schema",
                  "json_schema": {
                      "name" : "synthesis",
                      "schema":{
                      "type": "object",
                      "properties": {
                          "Synthesis": {
                              "type": "array",
                              "items": {
                                  "type": "object",
                                  "properties": {
                                      "productName": { "type": "string" },
                                      "productCCDCNumber": { "type": "string" },
                                      "steps":{
                                      "Add": {
                                          "type": "object",
                                          "properties": {
                                              "usedVessel": { "type": "string" },
                                              "addedChemicalName": { "type": "string" },
                                              "addedChemicalAmount": { "type": "string" },
                                              "stepNumber": { "type": "integer" }
                                          },
                                          "required": ["usedVessel", "addedChemicalName", "addedChemicalAmount", "stepNumber"],
                                          "additionalProperties": False
                                      },
                                      "HeatChill": {
                                          "type": "object",
                                          "properties": {
                                              "heatCoolingTime": { "type": "string" },
                                              "usedDevice": { "type": "string" },
                                              "targetTemperature": { "type": "string" },
                                              "heatingCoolingRate": { "type": "string" },
                                              "underVacuum": { "type": "boolean" },
                                              "usedVessel": { "type": "string" },
                                              "sealedVessel": { "type": "boolean" },
                                              "stepNumber": { "type": "integer" }
                                          },
                                          "required": ["heatCoolingTime", "usedDevice", "targetTemperature", "heatingCoolingRate", "underVacuum", "usedVessel", "sealedVessel", "stepNumber"],
                                          "additionalProperties": False
                                      },
                                      "Filter": {
                                          "type": "object",
                                          "properties": {
                                              "washingSolventName": { "type": "string" },
                                              "washingSolventAmount": { "type": "string" },
                                              "repetitions": { "type": "integer" },
                                              "usedVessel": { "type": "string" },
                                              "stepNumber": { "type": "integer" }
                                          },
                                          "required": ["washingSolventName", "washingSolventAmount", "repetitions", "usedVessel", "stepNumber"],
                                          "additionalProperties": False
                                      },
                                      "Stir": {
                                          "type": "object",
                                          "properties": {
                                              "stirringTime": { "type": "string" },
                                              "usedVessel": { "type": "string" },
                                              "stepNumber": { "type": "integer" }
                                          },
                                          "required": ["stirringTime", "usedVessel", "stepNumber"],
                                          "additionalProperties": False
                                      },
                                      "Sonicate": {
                                          "type": "object",
                                          "properties": {
                                              "sonicationTime": { "type": "string" },
                                              "usedVessel": { "type": "string" },
                                              "stepNumber": { "type": "integer" }
                                          },
                                          "required": ["sonicationTime", "usedVessel", "stepNumber"],
                                          "additionalProperties": False
                                      }}
                                  },
                                  "required": ["productName", "productCCDCNumber", "Add", "HeatChill", "Filter", "Stir", "Sonicate"],
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
              messages=messages
          )

        else:
          messages    =[
          {"role": "system","content": "You will be provided with unstructured data, and your task is to parse it into csv format."},
          {"role": "user", "content": full_prompt} ]
          response        = self.client.chat.completions.create(
                              model=model_name,
                              messages=messages
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

    SELECT ?Species (GROUP_CONCAT(?label; SEPARATOR=", ") AS ?labels) (COUNT(?label) AS ?labelCount)
    WHERE {{	
        ?Species skos:altLabel ?label .
        ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?InputChemical osyn:referencesMaterial ?Material .   
        ?ChemicalSynthesis osyn:retrievedFrom ?doc .
        ?doc <http://purl.org/ontology/bibo/doi> "{doi}" .
    }}
    GROUP BY ?Species
    HAVING (?labelCount > 2)
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
def process_papers():
    script_dir              = os.path.dirname(os.path.abspath(__file__))
    #processor               = XYZFileProcessor(os.path.abspath(os.path.join(script_dir, "../../Data/papers_with_si")), os.path.join(script_dir, "../../Data/"))
    #processor.process_files_in_directory(processor.transform_xyz_string)
    #processor.process_files_in_directory(processor.prepend_line_count)
def remove_section(text, section_title):
    # Define a regex pattern to match the section title and its content
    pattern = re.compile(rf'{section_title}.*?(?=\n[A-Z])', re.DOTALL)
    # Remove the section from the text
    cleaned_text = re.sub(pattern, '', text)
    return cleaned_text

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
    input_pdf_path              = os.path.join(os.getcwd(), "../../Data/pdf_files/Synthetic Supercontainers Exhibit Distinct Solution versus Solid State Guest-Binding Behavior.pdf")
    output_folder_path          = os.path.join(os.getcwd(), "../../Data/txt_files/")
    chatgpt_answer_path         = "../../Data/gpt_txt/"
    prompt                      = """Please extract the synthesis procedure for all the metal-organic polyhedra (MOPs) from the provided text.
                                        Provide a word-for-word copy of each paragraph related to the synthesis procedures.
                                        Only include the text from the input and do not add any additional information or commentary.

                                        Here is the text: """
    
    prompt_syn                  = """   Provide a word-for-word copy of each paragraph related to synthesis procedures.
                                        Answer the question as truthfully as possible using the provided context. 
                                        Only include the text from the input and do not add any additional information or commentary.

                                        Here is the text: """
                                        
    prompt_syn_2                = """   Provide a word-for-word copy of each paragraph related to synthesis procedures.
                                        Please make sure to capture all the synthesis-related information. Occurrences of "°" should be written as deg.
                                        Answer the question as truthfully as possible using the provided context. 
                                        Only include the text from the input and do not add any additional information or commentary.

                                        Here is the text: """
    
    prompt_syn_3                = """   Provide a word-for-word copy of each paragraph related to synthesis procedures.
                                        Please make sure to capture the synthesis related information in the experimental section.
                                        Answer the question as truthfully as possible using the provided context. 
                                        Only include the text from the input and do not add any additional information or commentary.
                                        Occurrences of "°" should be written as deg.

                                        Here is the text: """

    # Convert PDF to text
    converter               = PdfConverter(input_pdf_path, output_folder_path)
    #text, output_txt_path   = converter.convert_pdf_to_text()
    input_text_path         = "../../Data/txt_with_si/10.1002_asia.201701647.txt"
    # Extract the base name of the file (e.g., '10.1021_acs.inorgchem.8b01130.pdf')
    base_name               = os.path.basename(input_text_path)
    name, _                 = os.path.splitext(base_name)
    text                    = converter.read_text_file(input_text_path)
    doi                     = name.replace("_", "/")
    mop_cbu                 = get_literature(doi)
    print(mop_cbu)
    prompt2                 = """For every compound, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Synthesis text: """
    prompt2_2               = f"""For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Synthesis text: """
    prompt3                 = f"""For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                    for each procedure the first step is a list of **Reagents and Materials** and the last step is **Characterization**. 
                                    Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                    or you are unsure, use "N/A".

                                     
       
                                    Synthesis text: """
    prompt3_2               = f"""For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                    for each procedure the first step is a list of **Reagents and Materials** and the last step is **Characterization**. 
                                    Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                    or you are unsure, use "N/A". Please make sure to assign each synthesis to the correct MOP from the following list: {mop_cbu}.
                                    If it is not possible to assign the synthesis to a MOP, list the name in the paper. 
                                     
                                      
                                    Synthesis text: """
    prompt4                 = f"""  For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                    for each procedure the first step is a list of **Reagents and Materials** and the last step is **Characterization**. 
                                    Make sure to include all listed materials with the used quantities if available and to include the characterisation methods and results.
                                    Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                    or you are unsure, use "N/A".
    
                                    
                                    
                                Synthesis text: """
    prompt8                 = f"""  For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                    for each procedure a list of "Reagents and Materials" and "Characterization" is listed separately if possible. 
                                    Make sure to include all listed materials with the used quantities if available and to include the characterisation methods and results.
                                    Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                    or you are unsure, use "N/A". Group the steps into the following XDL steps: Add, Separate, Heat or Chill, wash solid, filter, transfer and add the group as title.
                                    
                                    Synthesis text: """
    prompt9                 = f"""  For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions.  Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                    for each procedure a list of "Reagents and Materials" and "Characterization" is listed separately if possible. 
                                    Make sure to include all listed materials with the used quantities if available.
                                    Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                    or you are unsure, use "N/A".
                                    
                                    Synthesis text: """
    prompt10                 = f"""  For the following MOPs: {mop_cbu}, please rewrite the provided synthesis procedures into separate,
                                    clear, and self-contained step-by-step instructions. Ensure that each synthesis procedure is entirely
                                    independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                    Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                    for each procedure a list of "Reagents and Materials" and "Characterization" is listed separately if possible. 
                                    Make sure to include all listed materials with the used quantities if available and to include the characterisation methods and results.
                                    Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                    or you are unsure, use "N/A".
                                    
                                    Synthesis text: """

    prompt5                 = """Could you summarize the following synthesis procedure in a json document? Answer the question as truthfully as possible using the provided context.
                                    If any information is not provided or you are unsure, use "N/A". Synthesis steps:"""
    prompt6                 = f"""Could you go through the following synthesis description and assign each step to one of the following XDL synthesis steps: Add, Separate, Heat or Chill, wash solid, filter, transfer?"""
    prompt7                 = """Could you summarize the following synthesis procedure in a jsonld document? Answer the question as truthfully as possible using the provided context.
                                    If any information is not provided or you are unsure, use "N/A". Please use the following Ontology as schema:
                                    {
  "@context": {
    "OntoSyn": "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl#",
    "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
    "owl": "http://www.w3.org/2002/07/owl#",
    "dc": "http://purl.org/dc/elements/1.1/",
    "xsd": "http://www.w3.org/2001/XMLSchema#"
  },
  "@id": "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl",
  "@type": "owl:Ontology",
  "dc:date": "18 June 2024",
  "rdfs:comment": "An ontology developed for representing chemical synthesis procedures.",
  "owl:versionInfo": "1.0",
  "owl:Class": [
    {
      "@id": "OntoSyn:ChemicalSynthesis",
      "rdfs:label": "Chemical Synthesis",
      "rdfs:comment": "A class representing a chemical synthesis procedure."
    },
    {
      "@id": "OntoSyn:Reagent",
      "rdfs:label": "Reagent",
      "rdfs:comment": "A class representing a chemical reagent."
    },
    {
      "@id": "OntoSyn:Solvent",
      "rdfs:label": "Solvent",
      "rdfs:comment": "A class representing a solvent used in the synthesis."
    },
    {
      "@id": "OntoSyn:Equipment",
      "rdfs:label": "Equipment",
      "rdfs:comment": "A class representing equipment used in the synthesis."
    },
    {
      "@id": "OntoSyn:Product",
      "rdfs:label": "Product",
      "rdfs:comment": "A class representing the product of the synthesis."
    },
    {
      "@id": "OntoSyn:SynthesisProcedure",
      "rdfs:label": "SynthesisProcedure",
      "rdfs:comment": "A class representing a step in the synthesis procedure."
    },
    {
      "@id": "OntoSyn:Add",
      "rdfs:label": "Add",
      "rdfs:comment": "A class representing the addition of a reagent to a vessel."
    },
    {
      "@id": "OntoSyn:Separate",
      "rdfs:label": "Separate",
      "rdfs:comment": "A class representing the separation of components in a synthetic reaction."
    },
    {
      "@id": "OntoSyn:Vessel",
      "rdfs:label": "Vessel",
      "rdfs:comment": "A class representing a vessel used in the reaction."
    },
    {
      "@id": "OntoSyn:HeatChill",
      "rdfs:label": "HeatChill",
      "rdfs:comment": "A class representing the process of heating or chilling in a synthetic reaction."
    },
    {
      "@id": "OntoSyn:Filter",
      "rdfs:label": "Filter",
      "rdfs:comment": "A class representing the filtration process in a synthetic reaction."
    },
    {
      "@id": "OntoSyn:WashSolid",
      "rdfs:label": "WashSolid",
      "rdfs:comment": "A class representing the process of washing solid materials in a synthetic reaction."
    },
    {
      "@id": "OntoSyn:Stir",
      "rdfs:label": "Stir",
      "rdfs:comment": "A class representing the process of stirring in a synthetic reaction."
    }
  ],
  "owl:DatatypeProperty": [
    {
      "@id": "OntoSyn:name",
      "rdfs:label": "Name",
      "rdfs:comment": "The name of the synthesis or entity.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "xsd:string"
      }
    },
    {
      "@id": "OntoSyn:description",
      "rdfs:label": "Description",
      "rdfs:comment": "A detailed description of the synthesis or entity.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "xsd:string"
      }
    },
    {
      "@id": "OntoSyn:quantity",
      "rdfs:label": "Quantity",
      "rdfs:comment": "The quantity of a reagent or material.",
      "rdfs:domain": {
        "@id": "OntoSyn:Reagent"
      },
      "rdfs:range": {
        "@id": "xsd:decimal"
      }
    },
    {
      "@id": "OntoSyn:unit",
      "rdfs:label": "Unit",
      "rdfs:comment": "The unit of measurement for the quantity.",
      "rdfs:domain": {
        "@id": "OntoSyn:Reagent"
      },
      "rdfs:range": {
        "@id": "xsd:string"
      }
    },
    {
      "@id": "OntoSyn:steps",
      "rdfs:label": "Steps",
      "rdfs:comment": "The step-by-step procedure of the synthesis.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "xsd:string"
      }
    },
    {
      "@id": "OntoSyn:conditions",
      "rdfs:label": "Conditions",
      "rdfs:comment": "The conditions under which the synthesis is performed (temperature, time, pressure, etc.).",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      }
    },
    {
      "@id": "OntoSyn:amount",
      "rdfs:label": "Amount",
      "rdfs:comment": "The amount of reagent added.",
      "rdfs:domain": {
        "@id": "OntoSyn:Add"
      },
      "rdfs:range": {
        "@id": "xsd:decimal"
      }
    }
  ],
  "owl:ObjectProperty": [
    {
      "@id": "OntoSyn:reagents",
      "rdfs:label": "Reagents",
      "rdfs:comment": "The reagents used in the synthesis.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Reagent"
      }
    },
    {
      "@id": "OntoSyn:solvents",
      "rdfs:label": "Solvents",
      "rdfs:comment": "The solvents used in the synthesis.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Solvent"
      }
    },
    {
      "@id": "OntoSyn:product",
      "rdfs:label": "Solvents",
      "rdfs:comment": "The solvents used in the synthesis.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Product"
      }
    },
    {
      "@id": "OntoSyn:hasSynthesisProcedure",
      "rdfs:label": "SynthesisProcedure",
      "rdfs:comment": "Step in the Synthesis",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "OntoSyn:SynthesisProcedure"
      }
    },
    {
      "@id": "OntoSyn:equipment",
      "rdfs:label": "Equipment",
      "rdfs:comment": "The equipment used in the synthesis.",
      "rdfs:domain": {
        "@id": "OntoSyn:ChemicalSynthesis"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Equipment"
      }
    },
    {
      "@id": "OntoSyn:hasReactionVessel",
      "rdfs:label": "hasReactionVessel",
      "rdfs:comment": "The vessel to which the reagent is added.",
      "rdfs:domain": {
        "@id": "OntoSyn:Add"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Vessel"
      }
    },
    {
      "@id": "OntoSyn:usedVessel",
      "rdfs:label": "Used Vessel",
      "rdfs:comment": "The vessel used in the heat or chill process.",
      "rdfs:domain": {
        "@id": "OntoSyn:HeatChill"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Vessel"
      }
    },
    {
      "@id": "OntoSyn:reagent",
      "rdfs:label": "Reagent",
      "rdfs:comment": "The reagent involved in the filtration process.",
      "rdfs:domain": {
        "@id": "OntoSyn:Filter"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Reagent"
      }
    },
    {
      "@id": "OntoSyn:reactionVessel",
      "rdfs:label": "reactionVessel",
      "rdfs:comment": "The vessel used in the stirring process.",
      "rdfs:domain": {
        "@id": "OntoSyn:Stir"
      },
      "rdfs:range": {
        "@id": "OntoSyn:Vessel"
      }
    }
  ]
}

                                    Synthesis steps: """
    # Send the PDF text to ChatGPT
    chatgpt_api             = ChatGPTAPI()
    cost                    = 5e-6
    model_name              = "gpt-4o-2024-05-13"
    chatgpt_api.count_tokens_and_calculate_cost(text, model_name, cost)
    response                = chatgpt_api.send_request(text, prompt_syn_3, model_name)

    # Save the response
    response_output_path    = os.path.join(output_folder_path, f"{name}_prompt1_4.txt")
    converter.save_text_to_file(response, response_output_path)


