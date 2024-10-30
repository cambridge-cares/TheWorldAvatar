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
    """Processes each .xyz file in the input directory and saves the modified files to the output directory."""
    for file_path in glob.glob(os.path.join(input_dir, '*')):
        if os.path.isfile(file_path):
            print("current file: ", file_path)
            transformed_text        = func(file_path, output_dir)

def transform_xyz_string(file_path:str, output_dir:str) -> None:
    """
    Transforms a single-line XYZ format string into a multi-line format and writes it to a file.
    Used to fix wrongly generated .xyz files

    Parameters:
    xyz_string (str): The input string in XYZ format with a single line.
    output_dir (str): The directory where the output file will be saved.

    Returns:
    str: adjusted file content
    """
    with open(file_path, 'r') as file:
        text                = file.read()
    # Split the input string into individual lines based on the pattern
    elements                = text.split()
    
    # Group elements in sets of 4 (element, X, Y, Z)
    atom_lines              = [f"{elements[i]} {elements[i+1]} {elements[i+2]} {elements[i+3]}" 
                for i in range(0, len(elements), 4)]
    
    # Construct the output text, file name and path
    output_text             = f"\n".join(atom_lines)
    file_name               = os.path.basename(file_path)
    output_path             = os.path.join(output_dir, file_name)
    # write to output file
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
    Extracts text from the PDF file.
    """
    # Extract the base name of the file (e.g., '10.1021_acs.inorgchem.8b01130.pdf')
    base_name       = os.path.basename(file_path)
    # Split the base name into name and extension (e.g., ('10.1021_acs.inorgchem.8b01130', '.pdf'))
    name, _         = os.path.splitext(base_name)
    text = ""
    pdf_document    = fitz.open(file_path)
    # exclude first and last page as they are worthless for synthesis
    for page_num in range(len(pdf_document)-1):
        page        = pdf_document.load_page(page_num)
                    # Define the extraction rectangle excluding header and footer
                    # Define the page size
        page_rect       = page.rect
        page_height     = page_rect.height
        # Define regions to exclude: header and footer
        header_height   = 50  # Height in points to exclude from the top
        footer_height   = 50  # Height in points to exclude from the bottom

        extraction_rect = fitz.Rect(
            page_rect.x0, 
            page_rect.y0 + header_height, 
            page_rect.x1, 
            page_rect.y1 - footer_height
        )

        # Extract text from the defined rectangle
        text       += page.get_text("text", clip=extraction_rect)
    with open(output_dir+"/"+name+".txt", "w", encoding="utf-8") as txt_file:
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

def extract_synthesis(extraction):
    match extraction:
        case "chemicals":
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_txt")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_chemicals1")
            process_files_in_directory(llm.extract_chemicals, in_directory, out_directory)
        case "procedure":
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_txt")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_extractedProcedure")
            process_files_in_directory(llm.extract_procedure, in_directory, out_directory)
        case "preSteps":
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_extractedProcedure")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_preSteps")
            process_files_in_directory(llm.extract_pre_steps, in_directory, out_directory)
        case "cbu":
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_chemicals1")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_cbu")
            process_files_in_directory(llm.extract_cbu, in_directory, out_directory)
        case "steps":
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_preSteps")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_steps")
            process_files_in_directory(llm.extract_steps, in_directory, out_directory)
        case "characterisation":
            in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_extractedProcedure")
            out_directory                       = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_characterisation")
            process_files_in_directory(llm.extract_characterization, in_directory, out_directory)
        case _:
            print("There was no match with extraction variable specify one of the following: chemicals, procedure, preSteps, steps, or characterisation!")
    return

def upload_chemcials():
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_chemicals1")
    print("in directory: ", in_directory)
    process_files_in_directory(up.chemicals_upload, in_directory, in_directory)

def upload_steps_dir():
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_steps")
    print("in directory: ", in_directory)
    process_files_in_directory(up.upload_steps, in_directory, in_directory)

def upload_characterisation_dir():
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_characterisation")
    print("in directory: ", in_directory)
    process_files_in_directory(up.characterisation_upload, in_directory, in_directory)

def link_cbu_dir():
    in_directory                        = os.path.join(para.DATA_FOLDER, f"{para.BATCH_NAME}_cbu")
    print("in directory: ", in_directory)
    process_files_in_directory(up.link_cbu, in_directory, in_directory)

def transform_txt_folder(file_dir):
    """transform pdf to txt. Expsnd readme"""
    in_directory                        = os.path.join(file_dir, f"{para.BATCH_NAME}_pdf")
    out_directory                       = os.path.join(file_dir, f"{para.BATCH_NAME}_txt")
    process_files_in_directory(extract_text_from_pdf, in_directory, out_directory)



