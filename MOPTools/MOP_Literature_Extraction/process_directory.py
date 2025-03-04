import os
import glob
import fitz  # PyMuPDF
import tiktoken
import subprocess
import datetime
import parameters as para
import llm_extraction as llm
import upload as up

def process_files_in_directory(func, input_dir, output_dir):
    """
    Processes each file in the specified input directory using a given function 
    and saves the processed output to the designated output directory.

    Parameters:
    func (function): A function that processes a single file, taking (file_path, output_dir) as arguments.
    input_dir (str): The directory containing the input files to be processed.
    output_dir (str): The directory where the processed files will be saved.

    Returns:
    None: The function does not return a value but applies `func` to each valid file in `input_dir`.
    """
    # Iterate over all files in the input directory
    for file_path in glob.glob(os.path.join(input_dir, '*')):
        # Check if the path corresponds to a file (ignoring directories)
        if os.path.isfile(file_path):
            print("current file: ", file_path)
            # Apply the provided function to process the file
            transformed_text        = func(file_path, output_dir)

def transform_xyz_string(file_path:str, output_dir:str) -> None:
    """
    Transforms a wrongly formatted XYZ file where atomic data is stored in a single line 
    into a properly structured multi-line format.

    Parameters:
    file_path (str): The path to the input .xyz file containing atomic data in a single line.
    output_dir (str): The directory where the corrected XYZ file will be saved.

    Returns:
    None: The function writes the transformed data to a new file in the specified output directory.
    """
    # Open and read the contents of the input file
    with open(file_path, 'r') as file:
        text                = file.read()
    # Split the input string into individual lines based on the pattern
    elements                = text.split()
    
    # Group elements in sets of 4 (atomic symbol, X, Y, Z coordinates) and format them as lines
    atom_lines              = [f"{elements[i]} {elements[i+1]} {elements[i+2]} {elements[i+3]}" 
                for i in range(0, len(elements), 4)]
    # Construct the corrected XYZ file content
    output_text             = f"\n".join(atom_lines)
    # Retrieve the original file name and construct the output file path
    file_name               = os.path.basename(file_path)
    output_path             = os.path.join(output_dir, file_name)
    # Write the transformed XYZ data to the output file
    with open(output_path, 'w') as output_file:
        output_file.write(output_text)
        print(output_text)

def prepend_line_count(file_path:str, output_dir:str):
    """
    Reads a file from the given file_path, prepends its line count followed by two newlines
    to its contents, and saves the modified content to a new file in the output directory.
    Used to fix wrongly generated .xyz files
    
    Args:
    - file_path  (str)  : Path to the file to be processed.
    - output_dir (str)  :  Directory where the updated files will be saved to.
    """
    # Read the file and get its contents
    with open(file_path, 'r') as file:
        lines           = file.readlines()
    line_count          = len(lines)  # Get the number of lines in the file

    # Prepare the output file path
    file_name           = os.path.basename(file_path)
    output_path         = os.path.join(output_dir, file_name)

    # Write to the output file
    with open(output_path, 'w') as file:
        file.write(f"{line_count}\n\n")  # Prepend line count and two newlines
        file.writelines(lines)          # Write the original content back

    print(f"Successfully processed {file_path}: {line_count} lines")

def extract_text_from_pdf(file_path:str, output_dir:str):
    """
    Extracts text from a PDF file while excluding headers and footers.
    
    Parameters:
    file_path (str): The path to the input PDF file.
    output_dir (str): The directory where the extracted text file will be saved.

    Returns:
    None: The extracted text is written to a new .txt file in the specified output directory.
    """
    # Extract the base name of the file (e.g., '10.1021_acs.inorgchem.8b01130.pdf')
    base_name       = os.path.basename(file_path)
    # Split the base name into the filename and extension (e.g., ('10.1021_acs.inorgchem.8b01130', '.pdf'))
    name, _         = os.path.splitext(base_name)
    # Initialize an empty string to store extracted text
    text = ""
    # Open the PDF document
    pdf_document    = fitz.open(file_path)
    # Iterate through each page, excluding the last one as it may not contain useful synthesis information
    for page_num in range(len(pdf_document)-1):
        page        = pdf_document.load_page(page_num)  # Load the current page
        # Get the dimensions of the page
        page_rect       = page.rect
        page_height     = page_rect.height
        # Define exclusion regions for header and footer
        header_height   = 50  # Height in points to exclude from the top
        footer_height   = 50  # Height in points to exclude from the bottom

        # Define the text extraction area by excluding header and footer regions
        extraction_rect = fitz.Rect(
            page_rect.x0,                   # Left boundary
            page_rect.y0 + header_height,   # Exclude header
            page_rect.x1,                   # Right boundary
            page_rect.y1 - footer_height    # Exclude footer
        )
        # Extract text from the defined rectangular area
        text       += page.get_text("text", clip=extraction_rect)
    # Construct the output text file path
    output_path = os.path.join(output_dir, f"{name}.txt")
    # Write the extracted text to a new .txt file
    with open(output_path, "w", encoding="utf-8") as txt_file:
        txt_file.write(text)

    
def count_tokens_and_calculate_cost(file_path:str, output_dir:str) -> dict:
    """
    Counts the number of tokens generated for the input text using a specified OpenAI model and calculates the cost.

    Parameters:
    input_text (str):       The input text to be tokenized.
    model_name (str):       The OpenAI model name.
    cost_per_token (float): The cost per token in dollars.

    Returns:
    dict:                   A dictionary containing the number of tokens and the estimated cost.
    """
    base_name               = os.path.basename(file_path)
    print(file_path, base_name)
    with open(file_path, 'r') as file:
                text        = file.read()
    cost                    = 5e-6
    model_name              = para.MODEL_NAME
    # Get the appropriate encoding for the specified model
    encoding                = tiktoken.encoding_for_model(model_name)
    # Encode the input text to get the tokens
    num_tokens              = len(encoding.encode(text))
    # Calculate the estimated cost
    estimated_cost          = num_tokens * cost
    # Return the number of tokens and the estimated cost
    print(f"Estimated costs for {base_name} : {estimated_cost} for the provided: {num_tokens} tokens.")
    return {"num_tokens": num_tokens, "estimated_cost": estimated_cost}

def extract_synthesis(extraction: str):
    """
    Extracts different aspects of synthesis data from text files by processing files 
    in designated directories using the appropriate extraction function.

    Parameters:
    extraction (str): Specifies the type of extraction to perform. 
                      Accepted values: "chemicals", "procedure", "preSteps", "cbu", "steps", "characterisation".

    Returns:
    None: The function processes files in the corresponding directories and does not return a value.
    """
    match extraction:
        case "chemicals":
            # Extracts chemical information from synthesis text
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_txt")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_chemicals1")
            process_files_in_directory(llm.extract_chemicals, in_directory, out_directory)
        case "procedure":
            # Extracts full synthesis procedures from text
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_txt")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_extractedProcedure")
            process_files_in_directory(llm.extract_procedure, in_directory, out_directory)
        case "preSteps":
            # Extracts preprocessing steps from the extracted procedures
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_extractedProcedure")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_preSteps")
            process_files_in_directory(llm.extract_pre_steps, in_directory, out_directory)
        case "cbu":
            # Extracts chemical building unit (CBU) data from the extracted chemical information
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_chemicals1")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_cbu")
            process_files_in_directory(llm.extract_cbu, in_directory, out_directory)
        case "steps":
            # Extracts synthesis steps from pre-processed steps
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_preSteps")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_steps")
            process_files_in_directory(llm.extract_steps, in_directory, out_directory)
        case "characterisation":
            # Extracts characterisation data from synthesis procedures
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_extractedProcedure")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_characterisation")
            process_files_in_directory(llm.extract_characterization, in_directory, out_directory)
        case _:
            # Handles invalid input by providing a valid list of options
            print("There was no match with extraction variable specify one of the following: chemicals, procedure, preSteps, steps, or characterisation!")
    return

def upload_chemcials():
    """
    Uploads extracted chemical data by processing files in the designated directory.

    This function processes chemical data files from the specified input directory 
    and uploads them using the `chemicals_upload` function.

    Parameters:
    None

    Returns:
    None: The function processes files in the directory and uploads them, but does not return a value.
    """
    # Define the input directory where extracted chemical data is stored
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_chemicals1")
    # Print the directory path for debugging purposes 
    print("in directory: ", in_directory)
    # Process and upload chemical data from the input directory
    process_files_in_directory(up.chemicals_upload, in_directory, in_directory)

def upload_steps_dir():
    """
    Uploads extracted synthesis step data by processing files in the designated directory.

    This function iterates through all synthesis step files in the specified directory 
    and uploads them using the `upload_steps` function.

    Parameters:
    None

    Returns:
    None: The function processes files in the directory and uploads them, but does not return a value.
    """
    # Construct the input directory path where extracted synthesis step data is stored
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_steps")
    # Print the directory path for debugging and verification purposes
    print("in directory: ", in_directory)
    # Process and upload synthesis step data from the input directory
    process_files_in_directory(up.upload_steps, in_directory, in_directory)

def upload_characterisation_dir():
    """
    Uploads extracted characterisation data by processing files in the designated directory.

    This function iterates through all characterisation data files in the specified directory 
    and uploads them using the `characterisation_upload` function.

    Parameters:
    None

    Returns:
    None: The function processes files in the directory and uploads them, but does not return a value.
    """
    # Construct the input directory path where extracted characterisation data is stored
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_characterisation")
    # Print the directory path for debugging and verification purposes
    print("in directory: ", in_directory)
    # Process and upload characterisation data from the input directory
    process_files_in_directory(up.characterisation_upload, in_directory, in_directory)

def link_cbu_dir():
    """
    Links extracted Chemical Building Unit (CBU) data by processing files in the designated directory.

    This function processes CBU data files from the specified directory 
    and links them using the `link_cbu` function.

    Parameters:
    None

    Returns:
    None: The function processes files in the directory and links them, but does not return a value.
    """
    # Construct the input directory path where extracted CBU data is stored
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_cbu")
    # Print the directory path for debugging and verification purposes
    print("in directory: ", in_directory)
    # Process and link CBU data from the input directory
    process_files_in_directory(up.link_cbu, in_directory, in_directory)

def transform_txt_folder(file_dir):    
    """
    Converts PDF files to plain text files by extracting text from PDFs in the specified directory.

    This function processes all PDF files in the input directory and extracts their text content, 
    saving the results as text files in the output directory.

    Parameters:
    file_dir (str): The base directory where PDF files are stored and where the extracted text files will be saved.

    Returns:
    None: The function processes files and saves the extracted text but does not return a value.
    """
    # Define the input directory containing PDF files
    in_directory                        = os.path.join(file_dir, f"{para.BATCH_NAME}_pdf")
    # Define the output directory where extracted text files will be saved
    out_directory                       = os.path.join(file_dir, f"{para.BATCH_NAME}_txt")
    # Process all PDF files in the input directory and extract text
    process_files_in_directory(extract_text_from_pdf, in_directory, out_directory)



