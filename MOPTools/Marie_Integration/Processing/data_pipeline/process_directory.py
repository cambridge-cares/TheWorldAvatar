import os
import glob
import fitz  # PyMuPDF
import tiktoken
from pipeline import get_literature, ChatGPTAPI, chemicals, append_si_to_paper, query_mop_names
from upload import chemicals_upload, upload, chemicals_upload_json


    
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


def process_files_in_directory(func, input_dir, output_dir):
    """Processes each .xyz file in the input directory and saves the modified files to the output directory."""
    for file_path in glob.glob(os.path.join(input_dir, '*')):
        if os.path.isfile(file_path):
            transformed_text        = func(file_path, output_dir)

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

def use_openai(file_path:str, output_dir:str):
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
    prompt                      = 7
    match prompt:
        case 1:
                extend          = ".txt"
                prompt_syn      = """   Provide a word-for-word copy of each paragraph related to synthesis procedures.
                                        Please make sure to capture the synthesis related information in the experimental section.
                                        Answer the question as truthfully as possible using the provided context. 
                                        Only include the text from the input and do not add any additional information or commentary.
                                        Occurrences of "°" should be written as deg.

                                        Here is the text: """
        case 2:
                mop_formula     = pipeline.get_literature(doi)
                extend          = "_prompt_2.txt"
                prompt_syn      = f"""  For the following MOP or MOPs: {mop_formula}, please rewrite the provided synthesis procedures into separate,
                                clear, and self-contained step-by-step instructions.  Ensure that each synthesis procedure is entirely
                                independent, with no cross-references to the other, and doesn't rely on any shared understanding. 
                                Any information that appears implicit should be made explicit in the rewrite. Please make sure that 
                                for each procedure a list of "Reagents and Materials" and "Characterization" is listed separately if possible. 
                                Make sure to include all listed materials with the used quantities if available.
                                Answer the question as truthfully as possible using the provided context. If any information is not provided 
                                or you are unsure, use "N/A". 
                                
                                Here is the text:
                                """
        case 3:
                mop_formula     = pipeline.get_literature(doi)
                extend          = ".txt"
                prompt_syn      = f""" Objective: Extract and provide an exact word-for-word copy of each paragraph specifically related to synthesis procedures from the given text, focusing on the experimental section.

Instructions:

Focus on Synthesis-Related Information: Extract only the paragraphs that directly describe the synthesis procedures, the product characterisation, or the experimental steps involved in the synthesis. Exclude any background information, discussions, or non-experimental content.

Exact Text Reproduction: Provide a verbatim copy of the relevant paragraphs. Do not modify the text.

Context and Relevance: Ensure the extracted text captures all necessary details related to the synthesis process and each step, including reagents, conditions, quantities, characterisation, and steps described in the experimental section.

No Additional Information: Do not include any added commentary, interpretation, or additional information. Your response should only contain the exact text as provided in the input.

Formatting for Clarity: Present the extracted paragraphs clearly and neatly. Separate each relevant paragraph with a line break for easy reading.

Truthfulness and Completeness: Include all relevant synthesis information based on the provided context. If certain parts of the synthesis are not explicitly described in the input text, leave them as they are without making assumptions.

Make sure to start with the general synthesis information if any are provided.

Example Output Format:

Optional Start: Experimental Section General Considerations. 
All reagents were obtained from commercial vendors and used without purification, excluding solvents. 
Methanol was obtained from a solvent drying system and stored in a glove box under 3Å sieves. 
Thermogravimetric analyses (TGA) were carried out from 50 oC to 600 degC at a 2 degC min-1 heating rate with a TA Q5000 SA under a nitrogen environment. 
All adsorption measurements were obtained on a Micromeritics 3Flex. 
Infrared spectroscopy was performed on a material using a Bruker Tensor 27 instrument. 
1H-NMR spectra were taken on a AV 400 spectrometer equipped with a cryogenic QNP probe.

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
Replacement Rule: Replace occurrences of "°" with "deg".

Provided Text: """
                
        case 4:
                extend          = ".txt"
                prompt_syn      = f"""Objective: Rewrite the provided synthesis procedures into clear, self-contained, and step-by-step instructions.

Instructions:

Step-by-Step Instructions: Break down each synthesis procedure into precise and sequential steps. Each procedure should stand alone and not reference any other procedures or require shared understanding.

Reagents and Materials: At the beginning of each procedure, list all the reagents, solvents, and materials used, including their quantities (e.g., "10 mL of acetone"). If quantities are not provided in the original text, denote them as "N/A".

Characterization: After the procedural steps, list the characterization techniques and results (e.g., NMR, IR spectra) separately. Include only the relevant data provided in the original text.

Explicit Details: Ensure that all implicit information in the original text is made explicit. For example, specify temperature, time, and any other conditions that are critical for the synthesis but may not be directly stated.
Include explicit comments from the text regarding MOP chemcial formula and CCDC number.

Formatting: Use bullet points or numbered lists to clearly delineate steps and sections (Reagents and Materials, Procedure, Characterization).

Truthfulness and Completeness: Answer the questions based solely on the provided context. If any information is missing or unclear, use "N/A" to indicate this.

Make sure to start with the general synthesis information if provided in the input text and continue with the component specific synthesis descriptions.

Example Format:

...
Reagents and Materials:

Acetone, 10 mL
Sodium hydroxide, 5 g
...
Procedure:

Dissolve 5 g of sodium hydroxide in 10 mL of acetone.
Stir the solution at room temperature for 30 minutes.
...
Characterization:

NMR (δ in ppm): 1.25 (s, 3H), 3.50 (m, 2H)
Melting point: 120°C
Yield: 0.15g (76%)
...
Provided Text: """ 
        case 5:
            mop_formula         = get_literature(doi)
            extend              = ".txt"
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
        case 6:
            mop_formula         = pipeline
            extend              = ".txt"
            prompt_syn          = f""" 
Task Specification:
"Summarize the given synthesis text into a CSV table. Each row in the CSV should represent a unique synthesis process."

Column Headers:
"Use the following column headers exactly as provided: Nr, Product, Precursors, Solvents, Temperatures, Heating time, Stirring time, Time to Crystallize, Vessel Type, Yield (percentage), Characterisation Instrumentation, CCDC number, MOPFormula."

Data Entry Guidelines:
"For each synthesis described in the text, fill in the relevant details under these columns. If any information is missing or uncertain, fill the cell with N/A."

Data Extraction Instructions:

Nr: Sequential numbering starting from 1 for each unique synthesis.
Product: Name of the synthesis product.
Precursors: List all reagents and their quantities involved in the synthesis.
Solvents: List all solvents used and their quantities if specified.
Temperatures: Specify the temperature(s) used during the synthesis.
Heating time: Provide the heating duration in hours if applicable.
Stirring time: Provide the stirring duration in hours if applicable.
Time to Crystallize: Time taken for the crystals to form.
Vessel Type: Type of vessel used for the reaction (e.g., vial, flask).
Yield (percentage): Report both the weight and percentage of yield if available.
Characterisation Instrumentation: Techniques used for characterization (e.g., PXRD, TGA, NMR).
CCDC number: Provide the CCDC number if available.
MOPFormula: Chemical formula of the MOP if specified.
Formatting Instructions:
"Surround each cell's content with double quotes (“”). Use a new row for each synthesis procedure and ensure each product and its synthesis details are captured completely."

Example CSV Entry:
"Nr","Product","Precursors","Solvents","Temperatures","Heating time","Stirring time","Time to Crystallize","Vessel Type","Yield (percentage)","Characterisation Instrumentation","CCDC number","MOPFormula"
"1","Goldberg MOP-4","4,4’,4’’-s-Triazine-2,4,6-triyl-tribenzoic acid (H3TATB) (25 mg), VOSO4·5H2O (51 mg)","DMF (3 mL)","433 K","48 hours","N/A","N/A","Parr Teflon-lined stainless steel vessel","20 mg (40%)","Elemental analysis","1552951","[WV5O11]12[(C3N3)(C6H4)3(CO2)3]20"

Synthesis Text:"""
            

        case 7:
            json_output         = False
            mop_formula         = get_literature(doi)
            chemical_names      = chemicals(doi)
            mop_names           = query_mop_names(doi)
            extend              = ".txt"
            prompt_syn          = f""" 
Step-by-Step Instructions: Break down each synthesis procedure into precise and sequential steps. Each procedure should stand alone and not reference any other procedures or require shared understanding.
Assign the steps in categories of Add, Heat chill, Filter, Stirr, and Sonicate. Category specification: Add is a step where material is added to a mixture or vessel.
If more than one reagent or solvent is added group each of them in their own step. An Add step requires a vessel and the name of the chemical that is added.
HeatChill is a synthesis step where a mixture or vessel is heated or cooled. If a mixture is heated or cooled to several different temperatures, 
assign each entry to a new step for each temperature. Each Heat and cooling step requires a target temperature to which is heated or cooled.
Make sure to assign the heating rate to the step with the assigned target temperature. 
Filter is a step in which a solid is extracted by filtration or washed with a solvent. 
Stirr is a step where a mixture is stirred for a certain time without heating, cooling, adding or filtering material. 
Crystallize: Crystallize dissolved solid by ramping temperature to given temp over given time.
Sonication describes the use of a sonication device for a certain time. 
Explicit Details: Ensure that all implicit information in the original text is made explicit.
For example, specify temperature, time, and any other conditions that are critical for the synthesis but may not be directly stated.
Include explicit comments from the text regarding MOP chemcial formula and CCDC number. 
Formatting: Use bullet points or numbered lists to clearly delineate steps. 
Truthfulness and Completeness: Answer the questions based solely on the provided context. 
If any information is missing or unclear, use "N/A" to indicate this. Make sure to start with the general synthesis information if provided in the input text 
and continue with the component specific synthesis descriptions.
Example:## MOP-14 Synthesis: 
1. **Add:** Place glycine tert-butyl ester hydrochloride (Gly-tBu·HCl, 242 mg) into a vessel.
2. **Add:** Add 6.0 mL of DMF into the vessel.
3. **Add:** Add triethylamine (TEA, 0.20 mL) into the vessel.
4. **Filter:** Remove the white precipitate (TEA·HCl) by filtration.
5. **Add:** Add Cu(OAc)2·H2O (144 mg) into a new vessel.
6. **Add:** Add 6 mL of DMF into the vessel containing Cu(OAc)2·H2O.
7. **Stirr:** Stir the solution for 10 minutes until the color turns blue-violet (solution A).
8. **Add:** Dissolve H25-Br-mBDC (36 mg) in 2.4 mL of DMF.
9. **Add:** Mix 2.2 mL of solution A with the dissolved H25-Br-mBDC in a capped vial (20 mL).
10. **HeatChill:** Heat the mixture to 85 °C at a constant rate and maintain for 14 hours.
11. **Filter:** Collect the green cubic crystals formed by washing with 3 × 2 mL of DMF.
12. **Filter:** Wash the crystals with 2 × 1 mL of cyclohexane.
13. **Comment:** Yield: 18 mg, 24% based on H25-Br-mBDC. MOP Formula: [Cu2]12[(C6H3Br)(CO2)2]24, CCDC Number: 706816.
"""

        case 9:
            json_output         = False
            mop_formula         = get_literature(doi)
            extend              = ".txt"
            prompt_syn          = f""" 
Task Specification: "Summarize the given synthesis text into a CSV table. Each row in the CSV should represent a unique chemical species. Group the chemicals by synthesis procedure." 
Column Headers: "Use the following column headers exactly as provided: Nr, Chemical Name, Chemical Formula, amount and unit, CCDC Number, Alternative Names, Synthesis Role, Synthesis Product" 
Data Entry Guidelines: "For each synthesis described in the text, fill in the relevant details under these columns.
If any information is missing or uncertain, fill the cell with N/A." 
Data Extraction Instructions: Nr: Sequential numbering starting from 1 for each unique synthesis. Chemical Name: Name of the chemical entity used in the text. Chemical Formula: Chemical Formula extracted from the text. "amount and unit": if given list the amount or quantity used of the chemical. 
CCDC Number: Number specified as assigned CCDC number Often noted in or as a comment. Alternative Names: 
Other Names and Chemical Formulas used in the text including empirical formulas. 
Synthesis Role: For each entry assign one of the following roles in a synthesis that fits best the part they 
take in the synthesis: "Reagent", "Solvent", "Product". 
Synthesis Product: Group the product according to the procedure they are used in. So if water is used to synthesise methanol assign water to the synthesis product methanol. 
Example CSV Entry: "Nr", "Chemical Name", "Chemical Formula", "amount and unit", "CCDC Number", "Alternative Names", "Synthesis Role","Synthesis Product" "1", "Goldberg MOP-4", "[WV5O11]12[(C3N3)(C6H4)3(CO2)3]20", "1212282", "MOP-4", "Product", "Goldberg MOP-4"
Extract the data from the following text:
"""          
        case 10:
            json_output             = False
            mop_formula             = get_literature(doi)
            extend                  = ".txt"
            prompt_syn              = f""" 
Task Specification: "Summarize the given synthesis text into a CSV table.
Each row in the CSV should represent a unique vessel used in the synthesis.
" Column Headers: "Use the following column headers exactly as provided: Nr, Vessel Name, Vessel Volume, Vessel Dimensions,
Vessel Material, Synthesis Product Name, Synthesis Product CCDC Number" Data Entry Guidelines: 
"For each synthesis described in the text, fill in the relevant details under these columns. 
If any information is missing or uncertain, fill the cell with N/A." Data Extraction Instructions: 
Nr: Sequential numbering starting from 1 for each unique vessel. Vessel Name: Name of the vessel entity used in the text.
Vessel Volume: Specified volume of the vessel from the text. Vessel Material: Specified material or infered material. 
Vessel Dimensions: If applicaply copy the specified length and/or width of the vessel. Synthesis Product Name: 
Assign each vessel to the synthesis it is used for and list the main synthesis product name. 
Synthesis Product CCDC Number: Assign each vessel to the synthesis it is used for and list the CCDC number of the MOP 
product if applicable. " Example CSV Entry: "Nr", "Vessel Name", "Vessel Volume", "Vessel Dimensions", "Vessel Material", 
"Synthesis Product Name", "Synthesis Product CCDC Number" "1", "Glass vial", "6 mL", "5 mm diameter and 100 mm length).",
"glass", "[Zr3O(OH)3(C5H5)3]4[(C6H4)2(CO2)2]6", "950332" Extract the data from the following text:
"""
        case 11:
            json_output             = True
            mop_formula             = get_literature(doi)
            extend                  = ".json"
            prompt_syn              = f""" 
Task Description:
Write a JSON file that extracts chemical product characterisation data. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema. Make sure to make a new entry for each synthesised product and fill in the product name and CCDC number if possible. 
Try to use product names and CCDC numbers from the following list: {mop_formula}
Category Specifications:
HNMR: HNMR data focus on extracting all shifts listed, the solvent that was used for the measurement and if listed the temperature at which the experiment was performed. 
Elemental Analysis: Extract the weight percentage of for each element and if listed the chemical formula. Include both measured and anal. calc. data. Usually the measured data is after the keyword "found:" while the calculated data is indicated by "Anal. Calcd. for ...:". Also if possible add the chemical formula used for the calculation and the device used for the measured weight percentage.
Infrared Spectroscopy: Extract bands and the used technique/material, example for material: "KBr pellet".
Data Entry Guidelines: "For each synthesis described in the text, fill in the relevant details under these columns. If any information is missing or uncertain, fill the cell with N/A." 
Return:
Provide a single JSON document containing all the characterisation data for each of the synthesis products. Make sure to fill in all the requried fields. If a field is unknown fill in "N/A" for strings and 0 for numeric types.
    Synthesis Text: 
"""
        # test structured output
        case 12:
            json_output             = True
            chemical_names          = chemicals(doi)
            mop_names               = query_mop_names(doi)
            extend                  = ".json"
            print("mop names: ", mop_names)
            prompt_syn              = f""" 
Task Description:
Write a JSON file that organizes synthesis steps. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema. Ensure that each step has an entry "stepNumber" that represents the chronological order of the steps if possible use the number given in the text.
Try to assign the vessel to one of the following: Teflon-lined stainless-steel vessel, glass vial, quartz tube, round bottom flask, glass scintillation vial, pyrex tube. If it is impossible to assign a vessel insert N/A.
Fill productNames with names that match best from the following list: {mop_names}.
Category Specifications:
Add: Steps involving adding material to a mixture or vessel. If multiple reagents or solvents are added, group each separately. Assign the chemicals added to one of the following and use the name of the chemical as follows: {chemical_names}
HeatChill: Steps where a mixture or vessel is heated or cooled. If multiple temperature changes occur, separate each into distinct steps. If the heatingrate is not given as number fill it in as heatChillRateComment. Example: "Heat slowly"
Filter: Steps involving filtration or washing of a solid with a solvent. Use the most appropriate from the following list as the solvent name: {chemical_names}. Make sure to separate repetitions and amount. E.g. if it is repeated 3 times with 4mL assign 4mL to amount and 3 to repetitions. Add additional information to filterComment. Example comment: 
Stirr: Steps where a mixture is stirred without additional actions.
Sonicate: Steps where sonication is used.
Crystallize: Crystallize dissolved solid by ramping temperature to given temp over given time.
Group Steps: Categorize all synthesis steps into their respective groups (Add, HeatChill, Filter, Stirr, Sonicate, Crystallize) while maintaining their original chronological order.
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
            prompt_syn          = f""" 
Task Specification: "Summarize the synthesis procedure into a JSON file. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema.
Try to use product names and CCDC numbers from the following list: {mop_formula}
List the chemicals for each synthesis procedure. Make sure to only list one synthesis product per synthesis procedure. 
The supplier name and purity are ussualy given in the general procedure of the paper while additional names and the chemical formula ussually is listed in the exact procedure. 
If any information is missing or uncertain, fill the cell with N/A for strings or 0 for numeric types.
Extract mixtures as one chemical but by separating the chemical names of the individual names with a "/". Example: chemicalName: "Ethanol/Water" for a mixture of Ethanol and Water. For all ChemicalAmount entries if the chemical is a mixture make sure to enter the amount for all components either by specifying the absolute amount as the names
(Example: for 10 mL ethanol and 20 mL water write: chemicalName: ethanol/water, chemicalAmount: 10mL/20mL.) or give the ratio of the two chemicals (Example: chemicalAmount: 30 mL (1:2))

Category Specifications:
"synthesisProcedure": Some of the provided text describe procedures for multiple products make sure to list each of them as a separate synthesisProcedure. 
    "procedureName": name the procedure after the product or copy an existing title.
        "inputChemicals": all chemicals listed that are not the end product of the synthesis and used in the specific procedure.
            "chemicalFormula": a set of chemical symbols showing the elements present in a compound and their relative proportions.
            "names": name of the inputChemicals, make sure to list all names that are used in the text. Separate the names by comma and extract them as individual strings. Example: 
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
        case 8:
            json_output             = True
            mop_formula             = get_literature(doi)
            extend                  = ".json"
            prompt_syn              = f""" 
            Task Description:
            Write a JSON file that organizes synthesis steps. Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema. Ensure that each step has an entry step number that represents the chronological order of the steps in the input steps.
            Try to assign the vessel to one of the following: Teflon-lined stainless-steel vessel, glass vial, quartz tube, round bottom flask, glass scintillation vial, pyrex tube. If it is impossible to assign a vessel insert N/A.
            Category Specifications:
            Add: Steps involving adding material to a mixture or vessel. If multiple reagents or solvents are added, group each separately. Try to assign the vessel to one of the following: Teflon-lined stainless-steel vessel, glass vial, quartz tube, round bottom flask, glass scintillation vial, pyrex tube. If it is impossible to assign a vessel insert N/A.
            HeatChill: Steps where a mixture or vessel is heated or cooled. If multiple temperature changes occur, separate each into distinct steps.
            Filter: Steps involving filtration or washing of a solid with a solvent.
            Stirr: Steps where a mixture is stirred without additional actions.
            Sonicate: Steps where sonication is used.
            Procedure for Handling Synthesis Steps:
            Group Steps: Categorize all synthesis steps into their respective groups (Add, HeatChill, Filter, Stirr, Sonicate) while maintaining their original chronological order.
            Assign Step Numbers: Retain the original step number from the input. If steps are omitted or split into several steps correct the number in the other synthesis steps as well. There should never be duplicate step numbers.
            Return:
            Provide a single JSON document containing the categorized synthesis steps with assigned step numbers, ensuring it accurately represents the chronological order of the entire synthesis procedure.
            JSOUND schema that should be used as a basis for the output file: 

{{
"Synthesis": 
    [{{
    "product name"          : "string",
    "product CCDC number"   : "string",
    "Add"                   : {{
        "used Vessel"                   : "string"  ,
        "added chemical name"           : "string"  ,
        "added chemical amount"         : "string"  ,
        "step number"                   : "integer"
    }},
    "HeatChill"             : {{
        "Heat or cooling Time"          : "string"  ,
        "used Device"                   : "string"  ,
        "Target temperature"            : "string"  ,
        "Heating or cooling rate"       : "string"  ,
        "under Vacuum"                  : "boolean" ,
        "used Vessel"                   : "string"  ,
        "sealed Vessel"                 : "boolean" ,
        "step Number"                   : "integer" 
    }},
    "Filter"                : {{
        "Name of the washing solvent"   : "string"  ,
        "Amount of washing solvent"     : "string"  ,
        "Repetitions"                   : "integer" ,
        "used Vessel"                   : "string"  ,
        "step Number"                   : "integer"
    }}, 
    "Stirr"                 : {{
        "stirring Time"                 : "string"  ,
        "used Vessel"                   : "string"  ,
        "step Number"                   : "integer"
    }},
    "Sonicate"              : {{
        "SonicationTime"                : "string"  ,
        "used Vessel"                   : "string"  ,
        "step Number"                   : "integer" 
    }}
    }}]
}}

Synthesis Text: 
"""
    chatgpt_api                         = ChatGPTAPI()
    model_name                          = "gpt-4o-2024-08-06"
    response                            = chatgpt_api.send_request(content, prompt_syn, model_name, prompt)
    with open(output_dir+"/"+name+extend, "w", encoding="utf-8") as txt_file:
        txt_file.write(response) 
    with open(output_dir+"/"+"_prompt.txt", "w", encoding="utf-8") as txt_file:
        txt_file.write(prompt_syn) 


def main():
    """Main function to run the script."""
    script_dir                          = os.path.dirname(os.path.abspath(__file__))
#    in_directory                        = os.path.join(script_dir, "../Data/first10_prompt4")
#    out_directory                       = os.path.join(script_dir, "../Data/first10_prompt52")
    in_directory                        = os.path.join(script_dir, "../Data/third10_extractedProcedure")
    out_directory                       = os.path.join(script_dir, "../Data/third10_preStep")
    #process_files_in_directory(chemicals_upload, in_directory, out_directory)
    #process_files_in_directory(prepend_line_count, in_directory, out_directory)
    #processor.process_files_in_directory(processor.transform_xyz_string)
    #processor.process_files_in_directory(processor.extract_text_from_pdf)
    # pre steps
    #in_directory                        = os.path.join(script_dir, "../Data/batch2_txt")
    #out_directory                       = os.path.join(script_dir, "../Data/third10_chemicals1")
    process_files_in_directory(use_openai, in_directory, out_directory)
    # steps
    #process_files_in_directory(chemicals_upload_json, in_directory, out_directory)
    #mop_names                           = get_literature("10.1039/C5DT04764A")
    #process_files_in_directory(upload, in_directory, out_directory)
    #process_files_in_directory(count_tokens_and_calculate_cost, in_directory, out_directory)
    #append_si_to_paper(out_directory)
    #process_files_in_directory(update_owl_urls, in_directory, out_directory)
    #print(chemicals("10.1021/ja0104352"))

if __name__ == "__main__":
    main()


