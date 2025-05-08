import datetime
import subprocess
import re
import os
import secrets.secret_parameter as spara
import fitz  # PyMuPDF
import json
from twa.kg_operations import PySparqlClient
from pyderivationagent.conf import config_generic, Config

class AboxUpdateConfig(Config):
    """
    Configuration class for managing SPARQL-based knowledge graph updates.

    This class extends the `Config` class and provides configuration settings 
    for the `UpdateKG` class, which is responsible for querying and updating 
    a knowledge graph (KG) using SPARQL endpoints.

    Attributes:
        SPARQL_QUERY_ENDPOINT (str): 
            The SPARQL endpoint URL used for querying the knowledge graph.
            Example: "http://example.com/sparql/query"
        
        SPARQL_UPDATE_ENDPOINT (str): 
            The SPARQL endpoint URL used for performing updates on the knowledge graph.
            Example: "http://example.com/sparql/update"
        
        KG_USERNAME (str): 
            The username required to authenticate with the SPARQL endpoints.
            This is used for secured knowledge graph instances requiring authentication.
        
        KG_PASSWORD (str): 
            The password associated with the given username for authentication 
            with the SPARQL endpoints.
            Ensure this is stored securely and not exposed in plain text.

    Notes:
        - This class is designed to be extended if additional configuration 
          fields are needed for specific agents interacting with the knowledge graph.
        - Proper security practices should be followed when handling credentials.
    """

    SPARQL_QUERY_ENDPOINT:      str         # URL for SPARQL query operations
    SPARQL_UPDATE_ENDPOINT:     str         # URL for SPARQL update operations
    KG_USERNAME:                str         # Username for authentication
    KG_PASSWORD:                str         # Password for authentication (handle securely)

def read_json_file(file_path:str):
    """
    Reads a JSON file and returns its content as a dictionary.

    This function opens a JSON file, reads its content, and parses it into a Python dictionary.

    Args:
        file_path (str): The absolute or relative path to the JSON file.

    Returns:
        dict: A dictionary representation of the JSON file content.

    Raises:
        FileNotFoundError: If the specified file does not exist.
        json.JSONDecodeError: If the file is not a valid JSON format.
        IOError: If an error occurs while reading the file.

    Example:
        >>> config = read_json_file("config.json")
        >>> print(config)
        {'key1': 'value1', 'key2': 'value2'}
    
    Notes:
        - Ensure the file exists before calling this function.
        - The file should be in a valid JSON format to avoid decoding errors.
    """
    try:
        with open(file_path, 'r') as file:
            data            = json.load(file)
        return data
    except FileNotFoundError as e:
        print(f"Error: File not found - {file_path}")
        raise e  # Reraise the exception to be handled by the calling function
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON format in file - {file_path}")
        raise e  # Reraise JSON decoding errors
    except IOError as e:
        print(f"Error: Failed to read file - {file_path}")
        raise e  # Handle other I/O errors

def doi_from_path(input_path: str):
    """
    Extracts and formats the DOI (Digital Object Identifier) from a given file path.

    This function processes the input file path, removes specific suffixes (if applicable), 
    and extracts the DOI from the filename using an underscore-based split.

    Args:
        input_path (str): The absolute or relative path of the file.

    Returns:
        str: The extracted DOI in the format "<prefix>/<suffix>".

    Raises:
        IndexError: If the file name does not contain expected DOI components.

    Example:
        >>> doi_from_path("path/to/10.1021_acs.inorgchem.8b01130.txt")
        '10.1021/acs.inorgchem.8b01130'
    
    Notes:
        - Assumes the filename follows the pattern: `<prefix>_<suffix>.txt`
        - Special handling for files ending with `_si.txt` to maintain consistency.
    """
    print("path:", input_path)
    # Handle special case where the filename ends with '_si.txt'
    if input_path.endswith('_si.txt'):
        # Replace '_si.txt' with '.txt' to ensure uniform processing
        input_path  = input_path.replace('_si.txt', ".txt")
    # Extract the filename from the given file path
    filename        = os.path.basename(input_path)
    # Split the filename using underscores ('_') as separators
    parts           = filename.split('_')
    try:
        # Extract the second part (without file extension) to form the DOI
        number2 = os.path.splitext(parts[-1])[0]

        # Construct the DOI in the required format
        doi = f"{parts[-2]}/{number2}"

        print("DOI computed:", doi)

        return doi

    except IndexError:
        print(f"Error: Unable to extract DOI from file path '{input_path}'. Ensure the filename follows the expected pattern.")
        raise

def generate_backup(filename):
    """
    Generates a backup of the OntoSyn knowledge graph by executing a SPARQL CONSTRUCT query 
    and saving the output in Turtle format.

    The function creates a filename using the current date and executes a `curl` command 
    to retrieve and store the data from the SPARQL endpoint.

    Args:
        filename (str): The base filename to be appended to the backup file.

    Returns:
        None

    Notes:
        - The backup file is named using the format: "OntoSynbackup_YYYY-MM-DD_<filename>.ttl"
        - The `curl` command retrieves RDF data using a SPARQL CONSTRUCT query.
        - The backup is saved in Turtle format (`.ttl`).
        - If the `curl` command fails, an error message is displayed.

    Example:
        >>> generate_backup("kg_dump")
        Backup saved to OntoSynbackup_2025-03-04_kg_dump.ttl
    """
    # Get the current date in YYYY-MM-DD format to include in the backup filename
    current_date = datetime.datetime.now().strftime("%Y-%m-%d")

    # Construct the backup file name with the current date and input filename
    output_file = f"OntoSynbackup_{current_date}_{filename}.ttl"

    # Define the curl command to retrieve the SPARQL query results in Turtle format
    curl_command = [
        "curl",                                                                 
        "-X", "POST",                                                           # HTTP POST request
        "--url", spara.QUERY_URL,                                               # SPARQL endpoint URL
        "--data-urlencode", "query=CONSTRUCT { ?s ?p ?o } where { ?s ?p ?o }",  # SPARQL query
        "--header", "Accept: application/x-turtle"                              # Request Turtle format
    ]

    # Execute the curl command and save the output to the backup file
    with open(output_file, "w") as file:
        result = subprocess.run(curl_command, stdout=file)

    # Print success or failure message based on the curl command execution result
    if result.returncode == 0:
        print(f"Backup saved to {output_file}")
    else:
        print(f"Failed to create backup. Return code: {result.returncode}")

def extract_bracket_substrings(input_string: str) -> tuple:
    """
    Extracts substrings enclosed within square brackets from a given input string.

    This function uses regular expressions to find all occurrences of text enclosed in 
    square brackets ('[...]'). If at least two bracketed substrings are found, it returns 
    the first two. Otherwise, it returns (None, None).

    Args:
        input_string (str): The input string containing bracketed substrings.

    Returns:
        tuple: A tuple containing two extracted substrings if at least two are found, 
               otherwise returns (None, None).

    Example:
        >>> extract_bracket_substrings("Some text [value1] and more [value2].")
        ('[value1]', '[value2]')

        >>> extract_bracket_substrings("Only one [value].")
        (None, None)
    """
    # Use regex to find all substrings enclosed within square brackets
    substrings = re.findall(r'\[[^\]]*\]', input_string)
    
    # If at least two substrings are found, return the first two
    if len(substrings) >= 2:
        substring1 = substrings[0]
        substring2 = substrings[1]
        return substring1, substring2
    else:
        # If less than two substrings are found, return None values
        return None, None
def config_a_box_updates(env_file: str = None) -> AboxUpdateConfig:
    """
    Loads and returns the configuration settings for ABox updates.

    This function retrieves configuration values either from environment variables 
    or from a specified `.env` file (if provided). It utilizes the `config_generic` 
    function to populate an instance of `AboxUpdateConfig`.

    Args:
        env_file (str, optional): The path to a `.env` file containing configuration 
                                  variables. If not provided, environment variables 
                                  will be used instead. Defaults to None.

    Returns:
        AboxUpdateConfig: An instance of `AboxUpdateConfig` populated with the retrieved 
                          configuration settings.

    Example Usage:
        >>> config = config_a_box_updates("config.env")
        >>> print(config.SPARQL_QUERY_ENDPOINT)

    Notes:
        - If `env_file` is provided, the function will attempt to load configuration 
          values from the specified file.
        - If `env_file` is `None`, configuration values will be pulled directly from 
          system environment variables.
    """
    # Print the provided environment file path for debugging
    print(env_file)
    # Load the configuration using `config_generic`, passing the AboxUpdateConfig class
    return config_generic(AboxUpdateConfig, env_file)

def get_client(name):
    """
    Initializes and returns a PySparqlClient instance for querying and updating a knowledge graph.

    This function loads ABox update configurations from an environment file specific to the given 
    `name`, then initializes a `PySparqlClient` with the retrieved settings.

    Args:
        name (str): The name of the environment configuration file (without extension).
                    The function looks for a file in the format `secrets/{name}.env`.

    Returns:
        PySparqlClient: An instance of `PySparqlClient` configured with the retrieved 
                        SPARQL endpoints and authentication credentials.

    Example Usage:
        >>> client = get_client("OntoSynthesisConnection")
        >>> results = client.query("SELECT ?s WHERE { ?s ?p ?o }")

    Notes:
        - The function expects an `.env` file in the `secrets/` directory with the following variables:
            - `SPARQL_QUERY_ENDPOINT`: SPARQL endpoint for querying the knowledge graph.
            - `SPARQL_UPDATE_ENDPOINT`: SPARQL endpoint for updating the knowledge graph.
            - `KG_USERNAME`: Username for authentication.
            - `KG_PASSWORD`: Password for authentication.
        - File system credentials (`fs_url`, `fs_user`, `fs_pwd`) are set to empty strings.
    """
    # Load ABox update configurations from the corresponding secrets file
    a_box_updates_config                        = config_a_box_updates(f"secrets/{name}.env")
    # Print the retrieved configuration for debugging purposes
    print("read in: ", a_box_updates_config)
    # Initialize and return a PySparqlClient instance with the retrieved settings
    return                                        PySparqlClient(
        query_endpoint                          = a_box_updates_config.SPARQL_QUERY_ENDPOINT            ,   # SPARQL query endpoint
        update_endpoint                         = a_box_updates_config.SPARQL_UPDATE_ENDPOINT           ,   # SPARQL update endpoint
        kg_user                                 = a_box_updates_config.KG_USERNAME                      ,   # Knowledge graph username
        kg_password                             = a_box_updates_config.KG_PASSWORD                      ,   # Knowledge graph password
        fs_url                                  = ""                                                    ,   # File system URL (unused, defaulting to empty)
        fs_user                                 = ""                                                    ,   # File system username (unused, defaulting to empty)
        fs_pwd                                  = ""        )                                               # File system password (unused, defaulting to empty)

def append_si_to_paper(directory):
    """
    Appends the content of supplementary information (SI) text files to their corresponding 
    main paper text files and deletes the SI files afterward.

    This function searches for `.txt` files in the specified directory and checks if a corresponding 
    `_si.txt` file exists. If found, the content of `_si.txt` is appended to the original `.txt` file, 
    and the `_si.txt` file is deleted to keep the directory clean.

    Args:
        directory (str): The path to the directory containing the text files.

    Returns:
        None: The function modifies files in place and does not return a value.

    Example Usage:
        >>> append_si_to_paper("/path/to/text/files")

    Notes:
        - The function ensures a **newline separation** (`\n\n`) before appending the SI content.
        - **Only** `.txt` files that **do not already end with `_si.txt`** are considered for appending.
        - After appending, the `_si.txt` file is **deleted** to prevent duplication.
    """
    # List all files in the specified directory
    files = os.listdir(directory)
    for file_name in files:
      # Check if the file is a .txt file and does NOT already end with '_si.txt'
      if file_name.endswith('.txt') and not file_name.endswith('_si.txt'):
          # Extract the base name (remove '.txt' extension)
          base_name = file_name.rsplit('.txt', 1)[0]
          # Construct the expected SI file name
          si_file_name = f"{base_name}_si.txt"
          
          # Check if the corresponding SI file exists in the directory
          si_file_path = os.path.join(directory, si_file_name)
          if si_file_name in files:
              # Construct the original file's full path
              original_file_path = os.path.join(directory, file_name)
              
              # Append the content of the _si.txt file to the original file
              with open(original_file_path, 'a', encoding='utf-8') as original_file, \
                    open(si_file_path, 'r', encoding='utf-8') as si_file:
                  # Ensure a newline separation before appending SI content
                  original_file.write("\n\n")  
                  # Append the content of the SI file to the original file
                  original_file.write(si_file.read())
              
              # Delete the SI file after appending its content to keep the directory clean
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
            

