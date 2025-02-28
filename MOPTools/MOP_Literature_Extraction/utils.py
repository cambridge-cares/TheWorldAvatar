import datetime
import subprocess
import re
import os
import secret_parameter as spara
import fitz  # PyMuPDF
import json
from twa.kg_operations import PySparqlClient
from pyderivationagent.conf import config_generic, Config

class AboxUpdateConfig(Config):
    """
    This is a config class for the UpdateKG class.
    It is a subclass of Config and can be extended to provide custom configurations for developed agents.
    It has the following fields:
      - SPARQL_QUERY_ENDPOINT:  The SPARQL endpoint to be used for querying the knowledge graph.
      - SPARQL_UPDATE_ENDPOINT: The SPARQL endpoint to be used for updating the knowledge graph.
      - KG_USERNAME:            The username to access the SPARQL endpoint.
      - KG_PASSWORD:            The password to access the SPARQL endpoint.
    """
    SPARQL_QUERY_ENDPOINT:      str
    SPARQL_UPDATE_ENDPOINT:     str
    KG_USERNAME:                str
    KG_PASSWORD:                str

def read_json_file(file_path:str):
    """
    Reads a JSON file and returns the data as a dictionary.

    Args:
    file_path (str): The path to the JSON file.

    Returns:
    dict: The data parsed from the JSON file.
    """
    with open(file_path, 'r') as file:
        data            = json.load(file)
    return data

def doi_from_path(input_path: str):
    print("path:", input_path)
    # Handle paths that end with '_si.txt' and other cases
    if input_path.endswith('_si.txt'):
        # Remove '_si.txt' from the path to make it consistent with other cases
        input_path  = input_path.replace('_si.txt', ".txt")
    # Extract the filename from the path
    filename        = os.path.basename(input_path)
    # Split the filename into parts using '_'
    parts           = filename.split('_')
    # Extract the second part (without the extension) using os.path.splitext
    number2         = os.path.splitext(parts[-1])[0]
    # Combine the parts in the desired format                                
    print("doi computed:", f"{parts[-2]}/{number2}")
    return f"{parts[-2]}/{number2}"

def generate_backup(filename):
    # Get the current date in YYYY-MM-DD format
    current_date = datetime.datetime.now().strftime("%Y-%m-%d")

    # Define the output file name using the current date
    output_file = f"OntoSynbackup_{current_date}_{filename}.ttl"

    # Define the curl command to be executed
    curl_command = [
        "curl",
        "-X", "POST",
        "--url", spara.QUERY_URL,
        "--data-urlencode", "query=CONSTRUCT { ?s ?p ?o } where { ?s ?p ?o }",
        "--header", "Accept: application/x-turtle"
    ]

    # Open the output file and execute the curl command, saving the output
    with open(output_file, "w") as file:
        result = subprocess.run(curl_command, stdout=file)

    # Print a message indicating where the output was saved
    if result.returncode == 0:
        print(f"Backup saved to {output_file}")
    else:
        print(f"Failed to create backup. Return code: {result.returncode}")

def extract_bracket_substrings(input_string):
    # Use regex to find all substrings within square brackets
    substrings = re.findall(r'\[[^\]]*\]', input_string)
    
    if len(substrings) >= 2:
        substring1 = substrings[0]
        substring2 = substrings[1]
        return substring1, substring2
    else:
        return None, None
def config_a_box_updates(env_file: str = None) -> AboxUpdateConfig:
    """Return configurations from either environment variables or env_file."""
    print(env_file)
    return config_generic(AboxUpdateConfig, env_file)

def get_client(name):
    a_box_updates_config                        = config_a_box_updates(f"secrets/{name}.env")
    print("read in: ", a_box_updates_config)
    return                                        PySparqlClient(
        query_endpoint                          = a_box_updates_config.SPARQL_QUERY_ENDPOINT            ,
        update_endpoint                         = a_box_updates_config.SPARQL_UPDATE_ENDPOINT           ,
        kg_user                                 = a_box_updates_config.KG_USERNAME                      ,
        kg_password                             = a_box_updates_config.KG_PASSWORD                      ,
        fs_url                                  = ""                                                    ,
        fs_user                                 = ""                                                    ,
        fs_pwd                                  = ""        )

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
# ------------------------------------
# utility classes
# ------------------------------------
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
            

