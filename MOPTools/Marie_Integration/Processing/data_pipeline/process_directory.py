import os
import glob
import fitz  # PyMuPDF
import tiktoken
from pipeline import get_literature, ChatGPTAPI, chemicals, append_si_to_paper, query_mop_names, input_for_cbu
from upload import upload, chemicals_upload_json, upload_steps
import json
import subprocess
import datetime

    
def delete_beginning(file_path: str, output_dir: str):
    """Processes a single .xyz file."""
    # Read the input file
    with open(file_path, 'r') as file:
        lines = file.readlines()
        
    # Check if the first line is an integer
    if lines and lines[0].strip().isdigit():
        # Remove the first two lines
        lines = lines[2:]
    
    # Get the original filename from the file path
    filename = os.path.basename(file_path)
    
    # Create the full output file path
    output_file_path = os.path.join(output_dir, filename)
    
    # Write the processed lines to the output file
    with open(output_file_path, 'w') as output_file:
        output_file.writelines(lines)


def process_files_in_directory(func, input_dir, output_dir, settings):
    """Processes each .xyz file in the input directory and saves the modified files to the output directory."""
    for file_path in glob.glob(os.path.join(input_dir, '*')):
        if os.path.isfile(file_path):
            print("current file: ", file_path)
            transformed_text        = func(file_path, output_dir, settings)

def transform_xyz_string(file_path:str, output_dir:str) -> None:
    """
    Transforms a single-line XYZ format string into a multi-line format and writes it to a file.

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

def update_owl_urls(input_file_path, output_file_path=None):
    # Specify the URLs to be replaced and their replacements
    replacements = {
        "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#": "https://www.theworldavatar.com/kg/ontomops/",
        "http://www.theworldavatar.com/kb/ontomops/": "https://www.theworldavatar.com/kg/ontomops/"
    }
    
    # Read the contents of the input file
    with open(input_file_path, 'r', encoding='utf-8') as file:
        content = file.read()
        content = content.replace("MetalOrganicPolyhedra", "MetalOrganicPolyhedron")
    
    # Replace the URLs in the content
    for old_url, new_url in replacements.items():
        content = content.replace(old_url, new_url)


    # Prepare the output file path
    file_name           = os.path.basename(input_file_path)
    output_path         = os.path.join(output_file_path, file_name)
    
    # Write the modified content back to the output file
    with open(output_path, 'w', encoding='utf-8') as file:
        file.write(content)
    
    print(f"URLs updated successfully in {output_file_path}")

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
    model_name              = "gpt-4o-2024-05-13"
    # Get the appropriate encoding for the specified model
    encoding                = tiktoken.encoding_for_model(model_name)
    # Encode the input text to get the tokens
    num_tokens              = len(encoding.encode(text))
    # Calculate the estimated cost
    estimated_cost          = num_tokens * cost
    # Return the number of tokens and the estimated cost
    print(f"Estimated costs for {base_name} : {estimated_cost} for the provided: {num_tokens} tokens.")
    return {"num_tokens": num_tokens, "estimated_cost": estimated_cost}

def use_openai(file_path:str, output_dir:str, settings: dict):
    """
    Extracts text from the PDF file.
    """
    # Extract the base name of the file (e.g., '10.1021_acs.inorgchem.8b01130.pdf')
    base_name                   = os.path.basename(file_path)
    # Split the base name into name and extension (e.g., ('10.1021_acs.inorgchem.8b01130', '.pdf'))
    name, _                     = os.path.splitext(base_name)            
    with open(file_path, "r", encoding="utf-8") as file:
            content             = file.read()
    if name.endswith('_si'):
        doi                     = name.rsplit('_si', 1)[0]
    else:   
        doi                     = name[:] 
    doi                         = doi.replace("_", "/")
    prompt                      = settings["promptNumber"]
    match prompt:
        case 5:
            mop_formula         = get_literature(doi)
            extend              = ".txt"
            dynamic_prompt          = {}
            print("mops: ", mop_formula)
            prompt_syn          = f""" 
Objective: 
Extract and provide detailed, verbatim copies of all synthesis procedures, product characterizations,
and experimental steps described in the provided text. Make sure to fully capture the Experimental Section if there is one.
Assign each MOP synthesis procedure to the corresponding MOPFormula and CCDC number from the following list: {mop_formula}. 
Ensure the output is clean, precise, and formatted for easy reading.

Instructions:
Focus on Synthesis-Related Information:
Extract all paragraphs that describe synthesis procedures, product characterizations, or experimental steps.
Exclude any background information, discussions, or non-experimental content.
Start with any general synthesis information provided to give context to the detailed steps that follow.
Add for each procedure a comment with either the CCDC number or MOPFormula that best fits the procedure or a comment that it was not possible to assign the procedure to a MOP.
Make sure to also extract all sentences that include a CCDC number.
Assign MOPFormula and CCDC number When Clear:
Where it is clear and certain, assign each synthesis procedure to the corresponding MOPFormula and CCDC number as additional comment. This could be inferred from the following list: {mop_formula} or explicitly mentioned in the text.
If the assignment is not clear or certain, include the synthesis procedure without linking it to any specific MOPFormula or CCDC number.

Exact Text Reproduction:
Provide a verbatim copy of the relevant paragraphs. Do not modify the text.
Maintain all relevant details, including reagents, conditions, quantities, and characterization data.

Context and Relevance:
Ensure the extracted text captures all necessary details related to the synthesis process, including each step, reagents, conditions, and quantities.
Include characterization methods and results if described in the same context as the synthesis. Make sure to include also the general information if provided which usually include type of equipment used and comments on the described chemicals.
Add a comment about the assigned MOPFormula or CCDC number.

No Additional Information:
Do not add any commentary, interpretation, or extra information. The response should contain only the exact text from the input, formatted as per these instructions. Except for the comments about CCDC number and MOPFormula.

Formatting for Clarity:
Present the extracted paragraphs clearly and neatly. Use appropriate headings or labels for each synthesis procedure.
Separate each relevant paragraph with a line break for easy reading.
Maintain a consistent structure to enhance readability, especially when multiple synthesis procedures are provided.

Truthfulness and Completeness:
Include all relevant synthesis information based on the provided context. If certain parts of the synthesis are not explicitly described in the input text, leave them as they are without making assumptions.
Ensure that each procedure is self-contained and independent, with no reliance on shared understanding outside the provided text. Make sure to start with the general synthesis information if any are provided.

Example Output Format:
Paragraph 1: (Synthesis of 9-isopropyl-carbazole. Carbazole (5 g, 0.03 mol), potassium hydroxide (10.5 g, 187
mmol) and N,N’-dimethylformamide (50 mL) were added to a 100 mL round bottom flask. The
resulting solution was stirred at room temperature for 30 minutes. To this mixture isopropyl iodide
(9 mL, 0.09 mol) was added. The resulting solution was then heated at 358 K overnight. The
mixture was allowed to cool to room temperature and then poured into a round bottom flask
containing DI water (250 mL). The precipitated solids were collected via vacuum filtration and
washed with DI water. No further purification was done prior to the following synthesis.)

Paragraph 2: (Synthesis of EG3-MOPs: Copper acetate (0.86 g, 4.73 mmol) and
TEGME-IPA (1.45 g, 4.41 mmol) were dissolved in DMF (90 mL), and
then, the solution was heated in a Teflon-lined bomb reactor at
808C for 1 day. After cooling the solution, diethyl ether (400 mL)
was added to precipitate the blue crystalline products. Single-crystals
for X-ray crystallography were obtained by slow vapor diffusion
of diethyl ether into a DMF solution of EG3-MOPs. Elemental
analysis calc.(%) for C438H656N26O239Cu24 (EG3-MOP): C, 45.22; H, 5.68;
N, 3.13 Found: C, 45.11; H, 5.32; N, 3.31.)
...

Provided Text: """
 
        # test structured output
        case 12:
            json_output             = True
            chemical_names          = chemicals(doi)
            mop_names               = query_mop_names(doi)
            dynamic_prompt          = {}
            extend                  = ".json"
            print("mop names: ", mop_names)
            prompt_syn              = f""" 
Task Description:
Write a JSON file that organizes synthesis steps. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema. Ensure that each step has an entry "stepNumber" that represents the chronological order of the steps if possible use the number given in the text.
Try to assign a vessel type from one of the following: Teflon-lined stainless-steel vessel, glass vial, quartz tube, round bottom flask, glass scintillation vial, pyrex tube, or schlenk flask. If it is impossible to assign a vessel insert N/A.
Enter both vessel name and type in the "usedVesssel" entry. Example entry for vessel name: "vessel 1"
Fill productNames with names that match best from the following list: {mop_names}.
Category Specifications:
Add: Steps involving adding material to a mixture or vessel. If multiple reagents or solvents are added, group each separately. Assign the chemicals added to one of the following and use the name of the chemical as follows: {chemical_names}
HeatChill: Steps where a mixture or vessel is heated or cooled. If multiple temperature changes occur, separate each into distinct steps. If the heatingrate is not given as number fill it in as heatChillRateComment. Example: "Heat slowly"
Filter: Steps involving filtration or washing of a solid with a solvent. Use the most appropriate from the following list as the solvent name: {chemical_names}. Make sure to separate repetitions and amount. E.g. if it is repeated 3 times with 4mL assign 4mL to amount and 3 to repetitions. Add additional information to filterComment. Example comment: "Obtain the brown precipitate"
Stirr: Steps where a mixture is stirred without additional actions.
Sonicate: Steps where sonication is used.
Crystallize: Crystallize dissolved solid by ramping temperature to given temp over given time.
Dry: Drying process. 
Evaporate: Describes evaporation of solvent. 
Group Steps: Categorize all synthesis steps into their respective groups (Add, HeatChill, Filter, Stirr, Sonicate, Crystallize, Evaporate, or Dry) while maintaining their original chronological order.
Assign Step Numbers: Retain the original step number from the input. If steps are omitted or split into several steps correct the number in the other synthesis steps as well. There should never be duplicate step numbers.
Chemical Names: For all ChemicalName entries if the chemical is a mixture make sure to enter the name in the following format: namechemical1/namechemical2, example: ethanol/DMF. Make sure that the correct chemical name is used and that abbreviations are not confused.
Chemical Amount: For all ChemicalAmount entries if the chemical is a mixture make sure to enter the amount for all components either by specifying the absolute amount as the names (Example: for 10 mL ethanol and 20 mL water write: chemicalName: ethanol/water, chemicalAmount: 10mL/20mL.) or give the ratio of the two chemicals (Example: chemicalAmount: 30 mL (1:2))
in the following format: namechemical1/namechemical2, example: ethanol/DMF. Make sure that the correct chemical name is used and that abbreviations are not confused.
Return:
Provide a single JSON document containing the categorized synthesis steps with assigned step numbers, ensuring it accurately represents the chronological order of the entire synthesis procedure.
Make sure to fill in all the requried fields. If a field is unknown fill in "N/A" for strings and 0 for numeric types.
Synthesis Text: 
"""

        case 13:
            json_output             = True
            chemical_names          = chemicals(doi)
            mop_formula             = get_literature(doi)
            mop_names               = query_mop_names(doi)
            dynamic_prompt          = {}
            extend                  = ".json"

            prompt_syn              = f""" 
Objective:
Extract characterization data from the synthesis section of scientific papers in two steps:

Step 1: Extract the names of the characterization devices from the general synthesis section.
Step 2: Extract detailed characterization data for each synthesis procedure. Make sure to include the characterisation data for each and every product separately.
Write the extracted data into a JSON file following the specified schema. Ensure all required fields are filled. If any field is missing or unknown, use "N/A" for strings and 0 for numeric types.

Additionally, extract chemical product characterization data from the synthesis text and organize it according to the JSON schema. Create a new entry for each synthesized product, including the product name and CCDC number.
Fill productNames with names that match best from the following list: {mop_names}.

Instructions: Focus on extracting mentions of measurement devices and characterization data specifically from the synthesis section of the paper, particularly in the general synthesis paragraph.

Category Specifications:

HNMR (Proton Nuclear Magnetic Resonance):
Copy the HNMR device name as stated in the paper.
Include the frequency (with units) and list the solvent(s) used in the measurement.
Extract all chemical shifts, solvent information, and temperature (if listed).
Example:
10.58 (s, 12H, O-H), 7.94 (s, 24H, Ph-H), 6.64 (s, 60H, Cp-H)
Elemental Analysis:
Copy the Elemental Analysis device name as stated in the paper.
Extract the weight percentage for each element, and if provided, the chemical formula.
Capture both measured (usually prefixed by "found:") and calculated data (indicated by "Anal. Calcd. for ...:").
Include the chemical formula used for the calculation and the device used for measuring the weight percentage.
Infrared Spectroscopy (IR):
Copy the IR device name as stated in the paper.
Include the solvent(s) used in the process, if available.
Extract all relevant bands and the material or technique used, such as "KBr pellet."
Data Entry Guidelines:
Make sure to extract the peaks and bands for all {len(mop_formula)} mops in the text.
For each synthesis procedure, fill in the relevant details under the specified categories.
If any information is missing or unclear, use "N/A" as a placeholder.
            """
        case 14:
            json_output         = False
            mop_formula         = get_literature(doi)
            extend              = ".json"
            dynamic_prompt      = {}
            prompt_syn          = f""" 
Task Specification: "Summarize the synthesis procedure into a JSON file. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema.
Try to use product names and CCDC numbers from the following list: {mop_formula}
List the chemicals for each synthesis procedure. Make sure to only list one synthesis product per synthesis procedure. 
The supplier name and purity are ussualy given in the general procedure of the paper while additional names and the chemical formula ussually is listed in the exact procedure. 
If any information is missing or uncertain, fill the cell with N/A for strings or 0 for numeric types.

Category Specifications:
"synthesisProcedure": Some of the provided text describe procedures for multiple products make sure to list each of them as a separate synthesisProcedure. 
    "procedureName": name the procedure after the product or copy an existing title.
        "inputChemicals": all chemicals listed that are not the end product of the synthesis and used in the specific procedure.
        If multiple chemical species are added or used as washing solvent etc. in one step make a new entry for each chemical. For all ChemicalAmount entries if the chemical is a mixture make sure to enter the amount for all components either by specifying the absolute amount as the names
        (Example: for 10 mL ethanol and 20 mL water write: "addedChemical":[{{"addedChemicalName":["ethanol"],"addedChemicalAmount":"10 mL"}}, {{"addedChemicalName":["water"],"addedChemicalAmount":"20 mL"}}] 
            "chemicalFormula": a set of chemical symbols showing the elements present in a compound and their relative proportions.
            "names": name of the inputChemicals, make sure to list all names that are used in the text. Separate the names by comma and extract them as individual strings. 
            "chemicalAmount": amount of the chemical in mole, kg or both. Example: 1.45 g, 4.41 mmol 
            "supplierName": Company name that produces the chemical which is usually given in the general procedure.  Example: Iron-
            (III) sulfate hydrate, 1,4-benzenedicarboxylic acid (H2BDC) was
            purchased from Aldrich Chemical Co. => "supplierName" = Aldrich Chemical Co.
            "purity": Usually an additional percentage value indicating the purity of the chemical. Example: N,N-Dimethylformamide (DMF) (99.9%) => "purity" = 99.9%
        "outputChemical": "Product or target of the synthesis.
        "chemicalFormula": a set of chemical symbols showing the elements present in a compound and their relative proportions.
        "names": name of the outputChemicals, make sure to list all names that are used in the text. 
        "yield": Ratio expressing the efficiency of a mass conversion process. Usually reported as follows (28% yield based on H2BPDC) or (65% yield). Please extract the percentage: E.g.: yield = "65%"
        "CCDCNumber": Number of the Cambridge Crystallographic Database. Specific to metalic organic polyhedra chemical. 

Return:
Make sure to extract the input and output chemicals for for all {len(mop_formula)} mops in the text.
"""          
        case 15:
            json_output                         = False
            mop_formula                         = get_literature(doi)
            extend                              = ".json"     
            mop_list, cbu_list, species_list    = input_for_cbu(doi)
            dynamic_prompt                      = {}

            print("cbu list: ", cbu_list, "species_list:", species_list, "mop list: ", mop_list)
            species_string                  = ""
            intro_string                    =  """You will be given different metal organic polyhedron(MOP) CCDC numbers (identifier). Each MOP has two cbus. 
                Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema.
                You will also be given a list of species that are used to synthesise the MOP. 
                The cbu formulas are just abstractions of the cbu and your task is to find the respecting equivalent(s) from the lab species list. Write the result to a JSON file
                adhering to the specified schema. If any information is missing or uncertain, fill the cell with N/A for strings or 0 for numeric types.
                \n"""
            
        case 16:
            json_output         = False
            mop_formula         = get_literature(doi)
            chemical_names      = chemicals(doi)
            mop_names           = query_mop_names(doi)
            dynamic_prompt      = {}
            extend              = ".txt"
            prompt_syn          = f""" 
Step-by-Step Instructions: Break down each synthesis procedure into precise and sequential steps. Each procedure should stand alone and not reference any other procedures or require shared understanding.
Assign the steps in categories of Add, Heat chill, Filter, Stirr, Dry, Evaporate, Dissolve, Transfer, Separate, and Sonicate. Make sure to break down the synthesis in precise, sequential and standalone steps. 
Make sure to return all described procedures. 
Category specification: 
Add is a step where material is added to a mixture or vessel this also includes acidify. Make sure to assign each chemical addition to an independent step. Also be carefull about the order of additions. Don't strictly follow the order the chemicals are listed in the procedure but in order they are added.
If the text specifies for example chemical X in water then water then chemical X and water neeed to be mixed first in a separate vessel and then added to the other vessel.
If more than one reagent or solvent is assign each of them their own step. If something is added twice list them as independent addition steps. An Add step requires a vessel and the name of the chemical that is added.
HeatChill is a synthesis step where a mixture or vessel is heated or cooled. If a mixture is heated or cooled to several different temperatures, 
assign each entry to a new step for each temperature. Each Heat and cooling step requires a target temperature to which is heated or cooled.
Make sure to assign the heating rate to the step with the assigned target temperature. 
Filter is a step in which a solid is extracted by filtration or washed with a solvent. Each filtration and washing should be listed as separate steps.
Stirr is a step where a mixture is stirred for a certain time without heating, cooling, adding or filtering material. If the procedure mentions waiting for a certain time assign it to stir and mention stirringrate = 0. Other words for wait are "stand", "age", or static vacuum. 
Crystallize: Crystallize dissolved solid by ramping temperature to given temp over given time.
Sonication describes the use of a sonication device for a certain time. 
Dry: Drying process that can involve a pressure and temperature. 
Evaporate: Describes evaporation of solvent. Include the time for which the evaporation process is performed. 
Dissolve: Dissolve solid in solvent. Assign soaking here as well.
Transfer: Transfer vessel content from one vessel to another.
Separate: Separate by extraction, centrifuge or column. Make sure to assign filtration as Filter step and not separation. Always mention the vessel name the separation is transfered to  eve nif it stays in the same vessesl.
If nothing is done and one just wait assign the step to Stir with stirring rate = 0.
Explicit Details: Ensure that all implicit information in the original text is made explicit.
For example, specify temperature, time, and any other conditions that are critical for the synthesis but may not be directly stated.
Include explicit comments from the text regarding MOP chemcial formula and CCDC number. 
Formatting: Use bullet points or numbered lists to clearly delineate steps. 
Truthfulness and Completeness: Answer the questions based solely on the provided context. 
If any information is missing or unclear, use "N/A" to indicate this. Make sure to start with the general synthesis information if provided in the input text 
and continue with the component specific synthesis descriptions.
Assign each step a vessel that is used. Vessels should be numbered in the order they are used and correctly resemble the procedure. If given also mention the type or name of the vessel in the procedure.
Try to assign the vessel to one of the following: Teflon-lined stainless-steel vessel, glass vial, quartz tube, round bottom flask, glass scintillation vial, pyrex tube, or schlenk flask.
Example:## MOP-14 Synthesis: 
1. **Add:** Place glycine tert-butyl ester hydrochloride (Gly-tBu·HCl, 242 mg) into a glass vial refered to as vessel 1.
2. **Add:** Add 6.0 mL of DMF into vessel 1.
3. **Add:** Add triethylamine (TEA, 0.20 mL) into vessel 1.
4. **Filter:** Remove the white precipitate (TEA·HCl) by filtration of the content of vessel 1.
5. **Add:** Add Cu(OAc)2·H2O (144 mg) into a new vessel called vessel 2.
6. **Add:** Add 6 mL of DMF into a scintilation vial refered to as vessel 2 containing Cu(OAc)2·H2O.
7. **Stirr:** Stir the solution in vessel 2 for 10 minutes until the color turns blue-violet (solution A).
8. **Add:** Dissolve H25-Br-mBDC (36 mg) product vessel 1 in 2.4 mL of DMF.
9. **Add:** Mix 2.2 mL of solution A from vessel 2 with the dissolved H25-Br-mBDC from vessel 1 in a capped vial (20 mL) vessel 3.
10. **HeatChill:** Heat the mixture in vessel 3 to 85 °C at a constant rate and maintain for 14 hours.
11. **Filter:** Collect the green cubic crystals formed by washing with 3 × 2 mL of DMF.
12. **Filter:** Wash the crystals with 2 × 1 mL of cyclohexane.
**Comment:** Yield: 18 mg, 24% based on H25-Br-mBDC. MOP Formula: [Cu2]12[(C6H3Br)(CO2)2]24, CCDC Number: 706816.
"""
        case 17:
            json_output                         = True
            chemical_names                      = chemicals(doi)
            mop_names                           = query_mop_names(doi)
            chatgpt_api                         = ChatGPTAPI()
            model_name                          = "gpt-4o-2024-08-06"
            prompt_steps                        = """Task Description:
Write a JSON file that organizes synthesis steps. Indicate wheter the type of a step is used in the following synthesis procedure. 
If so write True to for the respective step name and false otherwise. Make sure to fill in all the requried fields.

Synthesis Text: 
"""
            response                            = chatgpt_api.send_request(content, prompt_steps , model_name, 18, {})
            dynamic_prompt                      = json.loads(response)
            print("response: ", dynamic_prompt)
            extend                  = ".json"
            print("mop names: ", mop_names)
            category_spec           = ""
            if dynamic_prompt["Add"]:
                category_spec      += f"Add: Steps involving adding material to a mixture or vessel. If multiple reagents or solvents are added, group each separately. Assign the chemicals added to one of the following and use the name of the chemical as follows: {chemical_names}."
            if dynamic_prompt["HeatChill"]:
                category_spec      += f"""HeatChill: Steps where a mixture or vessel is heated or cooled. If multiple temperature changes occur, separate each into distinct steps. If the heatingrate is not given as number fill it in as comment. Example: "Heat slowly" """
            if dynamic_prompt["Filter"]:
                category_spec      += f"""Filter: Steps involving filtration or washing of a solid with a solvent. Use the most appropriate from the following list as the solvent name: {chemical_names}. Make sure to separate repetitions and amount. E.g. if it is repeated 3 times with 4mL assign 4mL to amount and 3 to repetitions. Add additional information to comment. Example comment: "Obtain the brown precipitate".Make sure to write each name as separate string. """
            if dynamic_prompt["Stir"]:
                category_spec      += f"""Stir: Steps where a mixture is stirred without additional actions. """
            if dynamic_prompt["Sonicate"]:
                category_spec      += f"""Sonicate: Steps where sonication is used. """
            if dynamic_prompt["Crystallization"]:
                category_spec      += f"""Crystallize: Crystallize dissolved solid by ramping temperature to given temp over given time. """
            if dynamic_prompt["Dry"]:
                category_spec      += f"""Dry: Drying process. """
            if dynamic_prompt["Evaporate"]:
                category_spec      += f"""Evaporate: Describes evaporation of solvent. """
            if dynamic_prompt["Dissolve"]:
                category_spec      += f"""Dissolve: Dissolve or soak solid in solvent. """
            if dynamic_prompt["Separate"]:
                category_spec      += f"""Separate: Separation step that is not covered otherwise. Usually separate by extraction, centrifuge or column. Make sure to assign filtration as Filter step and not separation. """
            if dynamic_prompt["Transfer"]:
                category_spec      += f"""Transfer: Transfer vessel content from one vessel to another. """
                       
            prompt_syn              = f""" 
            
Task Description:
Write a JSON file that organizes synthesis steps. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema. Ensure that each step has an entry "stepNumber" that represents the chronological order of the steps if possible use the number given in the text.
Try to assign a vessel type from one of the following: Teflon-lined stainless-steel vessel, glass vial, quartz tube, round bottom flask, glass scintillation vial, pyrex tube, or schlenk flask. If it is impossible to assign a vessel insert N/A.
Enter both vessel name and type in the "usedVesssel" entry. Example entry for vessel name: "vessel 1"
Fill productNames with names that match best from the following list: {mop_names}. If there is no match copy the name given in the text.
For chemical names make sure to write each name as separate string. Wrong: ["C4H9NO, DMA, N,N'-dimethylacetamide"], Correct: ["C4H9NO", "DMA", "N,N'-dimethylacetamide"]

Category Specifications:
{category_spec}
Group Steps: Categorize all synthesis steps into their respective groups (Add, HeatChill, Filter, Stirr, Sonicate, Crystallize, Evaporate, Transfer, Dissolve, or Dry) while maintaining their original chronological order.
Assign Step Numbers: Retain the original step number from the input. If steps are omitted or split into several steps correct the number in the other synthesis steps as well. There should never be duplicate step numbers.
If multiple chemical species are added or used as washing solvent etc. in one step make a new entry for each chemical. For all ChemicalAmount entries if the chemical is a mixture make sure to enter the amount for all components either by specifying the absolute amount as the names
(Example: for 10 mL ethanol and 20 mL water write: "addedChemical":[{{"addedChemicalName":["ethanol"],"addedChemicalAmount":"10 mL"}}, {{"addedChemicalName":["water"],"addedChemicalAmount":"20 mL"}}] 
For chemical names make sure to write each name as separate string. Wrong: ["C4H9NO, DMA, N,N'-dimethylacetamide"], Correct: ["C4H9NO", "DMA", "N,N'-dimethylacetamide"]
Chemical Amount: For all ChemicalAmount entries if the chemical is a mixture make sure to enter the amount for all components either by specifying the absolute amount or give the ratio of the two chemicals (Example: chemicalAmount: 30 mL (1:2))
Return:
Provide a single JSON document containing the categorized synthesis steps with assigned step numbers, ensuring it accurately represents the chronological order of the entire synthesis procedure.
Make sure to fill in all the requried fields. If a field is unknown fill in "N/A" for strings and -1 for numeric types.
Synthesis Text: 
"""

    chatgpt_api                         = ChatGPTAPI()
    model_name                          = "gpt-4o-2024-08-06"
    response                            = chatgpt_api.send_request(content, prompt_syn, model_name, prompt, dynamic_prompt)
    # remove "_si"
    if name.endswith('_si'):
        name                     = name.rsplit('_si', 1)[0]
    with open(output_dir+"/"+name+extend, "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 
    #with open(output_dir+"/"+"_prompt.txt", "w", encoding="utf-8") as txt_file:
    #    txt_file.write(prompt_syn) 

def create_directory(script_dir, folder_name):
    """
    Creates a directory at the specified path. If intermediate directories
    in the path don't exist, they will be created as well.

    :param path: The path of the directory to create.
    """
    input_dir_pdf                   = os.path.join(script_dir,f"../Data/{folder_name}_pdf")
    input_dir_txt                   = os.path.join(script_dir,f"../Data/{folder_name}_txt")
    input_dir_cbu                   = os.path.join(script_dir,f"../Data/{folder_name}_cbu")
    input_dir_preSteps              = os.path.join(script_dir,f"../Data/{folder_name}_preSteps")
    input_dir_steps                 = os.path.join(script_dir, f"../Data/{folder_name}_steps")
    input_dir_extractedProcedure    = os.path.join(script_dir, f"../Data/{folder_name}_extractedProcedure")
    input_dir_chemicals1            = os.path.join(script_dir, f"../Data/{folder_name}_chemicals1")
    input_dir_characterisation      = os.path.join(script_dir, f"../Data/{folder_name}_characterisation")
    dir_list                        = [input_dir_pdf, input_dir_txt, input_dir_cbu, input_dir_preSteps, input_dir_steps, input_dir_extractedProcedure, input_dir_chemicals1, input_dir_characterisation]
    for direct in dir_list:
    # The 'exist_ok=True' argument ensures that no error is raised if the directory already exists.
        os.makedirs(direct, exist_ok=True)
        print(f"Directory '{direct}' created successfully.")
def extract_synthesis(script_dir, folder_name, extraction):
    create_directory(script_dir, folder_name)
    match extraction:
        case "chemcials":
            settings                            = {"promptNumber":14}
            in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_txt")
            out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_chemicals1")
        case "procedure":
            settings                            = {"promptNumber":5}
            in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_txt")
            out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_extractedProcedure")
        case "preSteps":
            settings                            = {"promptNumber":16}
            in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_extractedProcedure")
            out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_preSteps")
        case "steps":
            settings                            = {"promptNumber":17}
            in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_preSteps")
            out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_steps")
        case "characterisation":
            settings                            = {"promptNumber":13}
            in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_extractedProcedure")
            out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_characterisation")
        case _:
            print("There was no match with extraction variable specify one of the following: chemicals, procedure, preSteps, steps, or characterisation!")
    process_files_in_directory(use_openai, in_directory, out_directory, settings)
    return

def generate_backup(filename):
    # Get the current date in YYYY-MM-DD format
    current_date = datetime.datetime.now().strftime("%Y-%m-%d")

    # Define the output file name using the current date
    output_file = f"OntoSynbackup_{current_date}_{filename}.ttl"

    # Define the curl command to be executed
    curl_command = [
        "curl",
        "-X", "POST",
        "--url", "http://68.183.227.15:3838/blazegraph/namespace/OntoSynthesis/sparql",
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

def main():
    """Main function to run the script."""
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
#    in_directory                        = os.path.join(script_dir, "../Data/first10_prompt4")
#    out_directory                       = os.path.join(script_dir, "../Data/first10_prompt52")

    #process_files_in_directory(chemicals_upload, in_directory, out_directory)
    #process_files_in_directory(prepend_line_count, in_directory, out_directory)
    #processor.process_files_in_directory(processor.transform_xyz_string)
    
    # pre steps
    #in_directory                        = os.path.join(script_dir, "../Data/batch2_txt")
    #out_directory                       = os.path.join(script_dir, "../Data/third10_chemicals1")
    folder_name                         = "eight10"
    extraction                          = "steps"   

    

        
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_pdf")
    in_directory                        = os.path.join(script_dir,f"../Data/{folder_name}_preSteps")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_steps")
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_preStep")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_cbu")
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_txt")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_extractedProcedure")
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_extractedProcedure")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_preSteps")
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_txt")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_chemicals1")
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_preSteps")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_steps")
    in_directory                        = os.path.join(script_dir, "../Data/batch6_txt")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_characterisation")
    in_directory                        = os.path.join(script_dir, f"../Data/{folder_name}_preSteps")
    out_directory                       = os.path.join(script_dir, f"../Data/{folder_name}_steps")
    #process_files_in_directory(use_openai, in_directory, out_directory)
    # steps
    #extract_synthesis(script_dir, folder_name, extraction)
    #process_files_in_directory(extract_text_from_pdf, in_directory, out_directory)
    #process_files_in_directory(chemicals_upload_json, f"../Data/{folder_name}_chemicals1", out_directory, settings=None)
    #result = subprocess.run(["bash", "make_backup_chemicals.sh"], capture_output=True, text=True)
    #process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    #generate_backup(folder_name)
    #mop_names                           = get_literature("10.1039/C5DT04764A")
    #process_files_in_directory(upload, in_directory, out_directory)
    #process_files_in_directory(count_tokens_and_calculate_cost, in_directory, out_directory)
    #append_si_to_paper(out_directory)
    #process_files_in_directory(update_owl_urls, in_directory, out_directory)
    #print(chemicals("10.1021/ja0104352"))
    """ 
    folder_name                         = "eight10"
    process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    generate_backup(folder_name)
    """
    """
    folder_name                         = "tenth10"
    extraction                          = "chemcials"
    process_files_in_directory(chemicals_upload_json, f"../Data/{folder_name}_chemicals1", out_directory, settings=None)
    generate_backup(folder_name)
    folder_name                         = "eleventh10"
    extract_synthesis(script_dir, folder_name, extraction)
    process_files_in_directory(chemicals_upload_json, f"../Data/{folder_name}_chemicals1", out_directory, settings=None)
    generate_backup(folder_name)
    folder_name                         = "twelfth10"
    extract_synthesis(script_dir, folder_name, extraction)
    process_files_in_directory(chemicals_upload_json, f"../Data/{folder_name}_chemicals1", out_directory, settings=None)
    generate_backup(folder_name)
    folder_name                         = "eight10"
    process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    


    folder_name                         = "ninth10"
    process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    generate_backup(f"{folder_name}_steps")
    folder_name                         = "tenth10"
    process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    generate_backup(f"{folder_name}_steps")
    """
    folder_name                         = "eleventh10"
    process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    generate_backup(f"{folder_name}_steps_all")
    """
    folder_name                         = "twelfth10"
    process_files_in_directory(upload_steps,  f"../Data/{folder_name}_steps", out_directory, settings=None)
    generate_backup(f"{folder_name}_steps")
    # Run the bash script
    #result = subprocess.run(["bash", "make_backup.sh"], capture_output=True, text=True)
    # Print the output of the script
    #print("stdout:", result.stdout)
    #print("stderr:", result.stderr)xw

    folder_name                         = "ninth10"
    extraction                          = "steps"
    extract_synthesis(script_dir, folder_name, extraction)
    
    folders                                 = ["sixth10", "seventh10", "eight10", "ninth10", "tenth10", "eleventh", "twelfth10"]
    for folder in folders:
        try:
            folder_name                         = folder
            extraction                          = "characterisation"
            extract_synthesis(script_dir, folder_name, extraction)
        except Exception as e:
            print("error: ", e)
            print(f"failed folder: {folder} when extracting {extraction}.")
    """ 
    """
    folders                                 = ["tenth10", "eleventh", "twelfth10"]
    for folder in folders:
        try:
            folder_name                         = folder
            extraction                          = "preSteps"
            extract_synthesis(script_dir, folder_name, extraction)
        except:
            print(f"failed folder: {folder} when extracting {extraction}.")
    folders                                 = ["ninth10", "tenth10", "eleventh", "twelfth10"]
    for folder in folders:
        try:
            folder_name                         = folder
            extraction                          = "steps"
            extract_synthesis(script_dir, folder_name, extraction)
        except:
            print(f"failed folder: {folder} when extracting {extraction}.")

    """ 
if __name__ == "__main__":
    main()


