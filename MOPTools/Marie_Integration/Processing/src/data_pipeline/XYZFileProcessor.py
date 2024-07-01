import os
import sys
import glob
import fitz  # PyMuPDF
import tiktoken
import re
import DataPipeline as DP


class XYZFileProcessor:
    def __init__(self, input_dir, output_dir):
        self.input_dir          = input_dir
        self.output_dir         = output_dir
    
    def delete_beginning(self, file_path):
        """Processes a single .xyz file."""
        with open(file_path, 'r') as file:
            lines               = file.readlines()
            # Remove atom count and comments if they exist
            if lines and lines[0].strip().isdigit():
                lines           = lines[2:]  # Remove the first two lines if the first line is an integer
            line_count          = len(lines)
            return line_count, lines



    def process_files_in_directory(self, func):
        """Processes each .xyz file in the input directory and saves the modified files to the output directory."""
        for file_path in glob.glob(os.path.join(self.input_dir, '*')):
            transformed_text        = func(file_path)
            self.input_dir          = self.output_dir

    def transform_xyz_string(self, file_path:str) -> None:
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
        elements            = text.split()
        
        # Group elements in sets of 4 (element, X, Y, Z)
        atom_lines          = [f"{elements[i]} {elements[i+1]} {elements[i+2]} {elements[i+3]}" 
                  for i in range(0, len(elements), 4)]
        
        # Construct the output text, file name and path
        output_text         = f"\n".join(atom_lines)
        file_name           = os.path.basename(file_path)
        output_path         = os.path.join(self.output_dir, file_name)
        # write to output file
        with open(output_path, 'w') as output_file:
            output_file.write(output_text)
            print(output_text)



    def prepend_line_count(self, file_path:str):
        """
        Reads a file from the given file_path, prepends its line count followed by two newlines
        to its contents, and saves the modified content to a new file in the output directory.
        
        Args:
        - file_path (str): Path to the file to be processed.
        """
        # Read the file and get its contents
        with open(file_path, 'r') as file:
            lines           = file.readlines()

        line_count          = len(lines)  # Get the number of lines in the file

        # Prepare the output file path
        file_name           = os.path.basename(file_path)
        output_path         = os.path.join(self.output_dir, file_name)

        # Write to the output file
        with open(output_path, 'w') as file:
            file.write(f"{line_count}\n\n")  # Prepend line count and two newlines
            file.writelines(lines)          # Write the original content back

        print(f"Successfully processed {file_path}: {line_count} lines")
    
    def extract_text_from_pdf(self, file_path:str):
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
        with open(self.output_dir+"/"+name+".txt", "w", encoding="utf-8") as txt_file:
            txt_file.write(text) 

        
    def count_tokens_and_calculate_cost(self, file_path:str) -> dict:
        """
        Counts the number of tokens generated for the input text using a specified OpenAI model and calculates the cost.

        Parameters:
        input_text (str):       The input text to be tokenized.
        model_name (str):       The OpenAI model name.
        cost_per_token (float): The cost per token in dollars.

        Returns:
        dict:                   A dictionary containing the number of tokens and the estimated cost.
        """
        base_name       = os.path.basename(file_path)
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

    def use_openai(self, file_path:str):
        """
        Extracts text from the PDF file.
        """
        # Extract the base name of the file (e.g., '10.1021_acs.inorgchem.8b01130.pdf')
        base_name       = os.path.basename(file_path)
        # Split the base name into name and extension (e.g., ('10.1021_acs.inorgchem.8b01130', '.pdf'))
        name, _         = os.path.splitext(base_name)            
        with open(file_path, "r", encoding="utf-8") as file:
                content = file.read()
        if name.endswith('_si'):
            doi         = name.rsplit('_si', 1)[0]
        else:   
            doi        = name[:] 
        doi                         = doi.replace("_", "/")
        prompt                      = 5
        match prompt:
            case 1:
                    extend        = ".txt"
                    prompt_syn    = """   Provide a word-for-word copy of each paragraph related to synthesis procedures.
                                            Please make sure to capture the synthesis related information in the experimental section.
                                            Answer the question as truthfully as possible using the provided context. 
                                            Only include the text from the input and do not add any additional information or commentary.
                                            Occurrences of "°" should be written as deg.

                                            Here is the text: """
            case 2:
                    mop_formula   = DP.get_literature(doi)
                    extend        = "_prompt_2.txt"
                    prompt_syn    = f"""  For the following MOP or MOPs: {mop_formula}, please rewrite the provided synthesis procedures into separate,
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
                    mop_formula   = DP.get_literature(doi)
                    extend        = ".txt"
                    prompt_syn    = f""" Objective: Extract and provide an exact word-for-word copy of each paragraph specifically related to synthesis procedures from the given text, focusing on the experimental section.

Instructions:

Focus on Synthesis-Related Information: Extract only the paragraphs that directly describe the synthesis procedures, the product characterisation, or the experimental steps involved in the synthesis. Exclude any background information, discussions, or non-experimental content.

Exact Text Reproduction: Provide a verbatim copy of the relevant paragraphs. Do not modify the text, except to replace occurrences of "°" with "deg".

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
                mop_formula   = DP.get_literature(doi)
                extend        = ".txt"
                prompt_syn    = f""" 
Objective: 
Extract and provide detailed, verbatim copies of all synthesis procedures, product characterizations,
and experimental steps described in the provided text.
When possible and clear, assign each synthesis procedure to the corresponding MOPFormula and CCDC number provided. 
Ensure the output is clean, precise, and formatted for easy reading.

Instructions:
Focus on Synthesis-Related Information:
Extract all paragraphs that describe synthesis procedures, product characterizations, or experimental steps.
Exclude any background information, discussions, or non-experimental content.
Start with any general synthesis information provided to give context to the detailed steps that follow.

Assign MOPFormula and CCDC number When Clear:
Where it is clear and certain, assign each synthesis procedure to the corresponding MOPFormula and CCDC number as additional comment. This could be inferred from the following list: {mop_formula} or explicitly mentioned in the text.
If the assignment is not clear or certain, include the synthesis procedure without linking it to any specific MOPFormula or CCDC number.

Exact Text Reproduction:
Provide a verbatim copy of the relevant paragraphs. Do not modify the tex..
Maintain all relevant details, including reagents, conditions, quantities, and characterization data.

Context and Relevance:
Ensure the extracted text captures all necessary details related to the synthesis process, including each step, reagents, conditions, and quantities.
Include characterization methods and results if described in the same context as the synthesis.

No Additional Information:
Do not add any commentary, interpretation, or extra information. The response should contain only the exact text from the input, formatted as per these instructions.

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
                mop_formula   = DP.get_literature(doi)
                extend        = ".txt"
                prompt_syn    = f""" 
Task Specification:
"Summarize the given synthesis text into a CSV table. Each row in the CSV should represent a unique synthesis process for a Molecular Organic Polyhedron (MOP)."

Column Headers:
"Use the following column headers exactly as provided: Nr, Product, Precursors, Solvents, Temperatures, Heating time, Stirring time, Time to Crystallize, Vessel Type, Yield (percentage), Characterisation Instrumentation, CCDC number, MOPFormula."

Data Entry Guidelines:
"For each MOP synthesis described in the text, fill in the relevant details under these columns. If any information is missing or uncertain, fill the cell with N/A."

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
"1","Goldberg MOP-4","4,4’,4’’-s-Triazine-2,4,6-triyl-tribenzoic acid (H3TATB) (25 mg), VOSO4·5H2O (51 mg)","DMF (3 mL)","433 K","48 hours","N/A","N/A","Parr Teflon-lined stainless steel vessel","20 mg (40\%)","Elemental analysis","1552951","[WV5O11]12[(C3N3)(C6H4)3(CO2)3]20"

Synthesis Text:"""
        chatgpt_api                 = DP.ChatGPTAPI()
        model_name                  = "gpt-4o-2024-05-13"
        response                    = chatgpt_api.send_request(content, prompt_syn, model_name)
        with open(self.output_dir+"/"+name+extend, "w", encoding="utf-8") as txt_file:
            txt_file.write(response) 
        with open(self.output_dir+"/"+"_prompt.txt", "w", encoding="utf-8") as txt_file:
            txt_file.write(prompt_syn) 


def main():
    """Main function to run the script."""
    script_dir              = os.path.dirname(os.path.abspath(__file__))
    in_directory            = os.path.join(script_dir, "../../Data/batch3_txt")
    out_directory           = os.path.join(script_dir, "../../Data/batch3_response12")
    processor               = XYZFileProcessor(in_directory, out_directory)
    #processor.process_files_in_directory(processor.transform_xyz_string)
    #processor.process_files_in_directory(processor.extract_text_from_pdf)
    processor.process_files_in_directory(processor.use_openai)
    #processor.process_files_in_directory(processor.count_tokens_and_calculate_cost)
    #DP.append_si_to_paper(out_directory)

if __name__ == "__main__":
    main()

