import os
import kg_queries as kgq
import utils
import llm_extraction as llme
import llm_prompts as llmp
import parameters as para

def write_step_text(query_output:dict, synthesis_text:str, doi:str):
    """
    Generates a synthesis step text based on the provided query_output dictionary by 
    pluging in the obtained dictionairy values in a pre writen text that can be later on passed
    to an LLM for refinement.
    Usually the dictionary provides information about a single step in the synthesis procedure. 
    The generated text is therefore added to the already existing synthesis text.
    The overall procedure is obtained by iterating over the different steps in the procedure.
    
    Parameters:
    query_output (dict): Dictionary containing step details such as step number, species, duration, etc.
    synthesis_text (str): The text accumulating synthesis steps.
    doi (str): Digital Object Identifier for provenance reference.
    
    Returns:
    str: Updated synthesis text with the new step details.
    """
    # Print input query_output for debugging purposes
    print("step input: ", query_output)
    # If this is the first step, add a synthesis header
    if query_output["stepnumber"] == "1":
       synthesis_text                  += f"\n # Synthesis of {query_output["Plabels"]} \n ## Provenance:\n{doi} \n## Steps"
    # Initialize additional information text
    additional_text                     = ""
    # Include duration details if applicable
    if "Filter" not in query_output["step"]: 
        if query_output["Duration"]     != "0.0 N/A":
            additional_text             += f"over the duration of {query_output["Duration"]}"
    # Include environment details if specified
    if query_output["environment"] != "N/A":
       additional_text                  += f"under {query_output["environment"]} atmosphere"
    # Handle "Add" step type
    if "Add" in query_output["step"]:
        species_list              = query_output['speciesLabels'].split(",")
        species_string        = ""
        for species in species_list[:3]:
            species_string        += (species + " ")
        synthesis_text            += f"""{query_output["stepnumber"]}. Add: Add {query_output["speciesAmount"]} of \
        {species_string} to {query_output["vesselName"]} of type {query_output["vesselType"]} {additional_text}. """
        # Include optional conditions
        if query_output["targetPH"] != '-1.0':
            synthesis_text         += f"Add until a Ph of {query_output["targetPH"]} is reached."
        if query_output['layered']:
            synthesis_text         += f"Make sure to layer while adding."
        if query_output['stirred']:
            synthesis_text         += f"Stir while adding."
    # Handle "HeatChill" step type
    elif "HeatChill" in query_output["step"]:
        synthesis_text          += f"""{query_output["stepnumber"]}. HeatChill: Heat or chill {query_output["vesselName"]}  to temperature {query_output["temperature"]} with a 
        rate of {query_output["temperatureRate"]} using {query_output["heatChillDevice"]} {additional_text}. 
        """
        if query_output["vacuum"]:
            synthesis_text          += f"Keep under vacuum for heating."
        if query_output["sealed"]:
            synthesis_text          += f"Seal the vessel for the heating process or keep sealed."
    # Handle "Filter" step typ
    elif "Filter" in query_output["step"]:
        # Determine if vacuum filtration is needed
        if query_output["vacuum"]:
            additional_text  += f"under vacuum."
        # If there is a species -> washing is involved
        if 'speciesLabels' in query_output and query_output['speciesLabels'] != "" and query_output['speciesLabels'] != "N/A":
          species_list          = query_output['speciesLabels'].split(",")
          species_string        = ""
          for species in species_list[:3]:
             species_string     += (species + " ")
          synthesis_text       += f"""{query_output["stepnumber"]}. Wash: Wash the solid in {query_output["vesselName"]} with {query_output['speciesAmount']} of {species_string}. Repeat {query_output['repeated']} times. Filter {additional_text}.
            """
        
        else: 
          # Standard filtration step
          synthesis_text      += f"""{query_output["stepnumber"]}. Filter content of {query_output["vesselName"]} repeat {query_output['repeated']} times. Perform all filtrations {additional_text}.
            """           
    # Handle "Dry" step type
    elif "Dry" in query_output["step"]:
        # distinguish between washing and filtering:
        synthesis_text        += f"""
{query_output["stepnumber"]}. Dry:Dry the content of vessel {query_output["vesselName"]} {additional_text}
        """          
        # Include temperature if specified
        if "temperature" in query_output:
           synthesis_text        += f"Dry at a temperature of {query_output["vesselName"]}"
           # check key
        # Include pressure if specified
        if "pressure" in query_output:
           synthesis_text        += f"Dry at {query_output["pressure"]}"
        # Include drying agent if specified
        if 'speciesLabels' in query_output and query_output['speciesLabels'] != "" and query_output['speciesLabels'] != "N/A":
          species_list          = query_output['speciesLabels'].split(",")
          species_string        = ""
          for species in species_list[:3]:
             species_string     += (species + " ")
          synthesis_text        += f"""Use {query_output['speciesAmount']} of {species_string} as drying agent.
            """
    # Append any additional comments
    if "comment" in query_output:
        synthesis_text          += f"{query_output["comment"]}"
    # Ensure step ends with a newline for readability
    synthesis_text          += f"\n"
    return synthesis_text

def write_characterisation_text(query_output:dict, synthesis_text:str):
    """
    Extends the synthesis_text with characterization data extracted from query_output.
    
    Parameters:
    query_output (dict): A dictionary containing characterization data.
    synthesis_text (str): A string to append characterization information to.
    
    Returns:
    str: The updated synthesis_text with characterization details.
    """
    # Print the characterization data for debugging purposes
    print("characterisation data: ", query_output)
    # Append the chemical characterization heading to the synthesis text
    synthesis_text                  += f"\n Characterization of {query_output['ChemicalOutput']} \n"
    # Extract the NMR solvent names and create a formatted string
    species_string                   = ""
    species_list                     = query_output['NMRSolventNames'].split(",")
    # Limit the solvent species string to the first three solvents
    for species in species_list[:3]:
        species_string     += (species + " ")
    # Append characterization details to synthesis_text
    synthesis_text                  += f"""
- IR characterization: Ir measured with a {query_output['irDeviceLabels']} values are: {query_output['IRx1s']}.
- NMR characterization: NMR measured with a {query_output['nmrDeviceLabels']} and solvent {species_string} values are: {query_output['nmraxis_com']}
- Elemental analysis characterization: Measured with a {query_output['eleDeviceLabels']} values are: {query_output['elementalAnalysis']} calculated values based on molecular formula {query_output['molecularFormulas']}.
- Yield: {query_output['yield']}
    """
    return synthesis_text

def generate_synthesis_text_llm(txt, doi):
    """
    Generates a synthesis text passing the input text to an LLM (Large Language Model) and saves it as a markdown file.
    
    Parameters:
    txt (str): The input text describing the synthesis process.
    doi (str): The DOI (Digital Object Identifier) used for naming the output file.
    """
    # Generate a synthesis text prompt from the input text
    prompt                      = llmp.synthesis_text_prompt(txt)
    # Define conversation messages for the LLM API
    messages                    =[
              {"role": "system","content": "You are a Chemist at a University and you translate abstract synthesis texts in experimental procedures that could be published in a scientific paper. "},
              {"role": "user", "content": prompt} ]
    # Initialize the ChatGPT API instance
    chatgpt_api                 = llme.ChatGPTAPI()
    # Make the LLM call and retrieve the generated synthesis text
    response                    = chatgpt_api.make_llm_call(messages)
    # Define the output file path, replacing '/' in DOI with '_' to ensure valid filenames
    ouptut_path_llm             = os.path.join(para.DATA_FOLDER, f"synText/{doi.replace("/", "_")}_llm.md")
    # Initialize a PDF converter instance
    pdf_converter               = utils.PdfConverter("", "")
    # Save the generated text response to the specified markdown file
    pdf_converter.save_text_to_file(response, ouptut_path_llm)

def generate_syn_text():
    """
    Generates synthesis text for a given DOI by querying synthesis 
    and characterization data, formatting it, and saving it to a file.
    Additionally, the text is processed with an LLM.
    """
    # Get the directory of the current script
    script_dir       = os.path.dirname(os.path.abspath(__file__))
    # DOI of the paper to be processed
    # Example DOI commented out:
    # doi = "10.1021/ic501012e"
    doi              = "10.1021/jacs.8b05780"
    # Query synthesis and characterization data for the given DOI
    query_output     = kgq.query_synthesis_full(doi)
    query_char       = kgq.query_characterisation(doi)
    # Initialize an empty string to store synthesis text
    synthesis_text              = ""
    # Process each synthesis step and append to the synthesis text
    for steps in query_output:
        synthesis_text          = write_step_text(steps, synthesis_text, doi)
        print("syn text: ", synthesis_text)
    # Process each characterization entry and append to the synthesis text
    for product in query_char:
        synthesis_text          = write_characterisation_text(product, synthesis_text)
    print("query characterisation: ", query_char)   # Debugging output
    # Define output file path, replacing '/' in DOI with '_'
    ouptut_path                 = os.path.join(script_dir, f"../Data/synText/{doi.replace("/", "_")}.md")
    # Convert and save the synthesis text as a .md
    pdf_converter               = utils.PdfConverter(ouptut_path, ouptut_path)
    pdf_converter.save_text_to_file(synthesis_text, ouptut_path)
    # Additionaly generate additional synthesis text asking llm to write it based on puzzle text. 
    generate_synthesis_text_llm(synthesis_text, doi)


# Example usage:
if __name__ == "__main__":
    generate_syn_text()