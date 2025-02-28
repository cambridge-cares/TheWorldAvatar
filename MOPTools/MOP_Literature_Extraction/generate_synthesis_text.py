import os
import kg_queries as kgq
import utils
import llm_extraction as llme
import llm_prompts as llmp
import parameters as para

def write_step_text(query_output:dict, synthesis_text:str, doi:str):
    # general information
    print("step input: ", query_output)
    if query_output["stepnumber"] == "1":
       synthesis_text                  += f"\n # Synthesis of {query_output["Plabels"]} \n ## Provenance:\n{doi} \n## Steps"
    additional_text                     = ""
    # duration
    if "Filter" not in query_output["step"]: 
        if query_output["Duration"]     != "0.0 N/A":
            additional_text             += f"over the duration of {query_output["Duration"]}"
    if query_output["environment"] != "N/A":
       additional_text                  += f"under {query_output["environment"]} atmosphere"

    if "Add" in query_output["step"]:
        species_list              = query_output['speciesLabels'].split(",")
        species_string        = ""
        for species in species_list[:3]:
            species_string        += (species + " ")
        synthesis_text            += f"""{query_output["stepnumber"]}. Add: Add {query_output["speciesAmount"]} of \
        {species_string} to {query_output["vesselName"]} of type {query_output["vesselType"]} {additional_text}. """
        if query_output["targetPH"] != '-1.0':
            synthesis_text         += f"Add until a Ph of {query_output["targetPH"]} is reached."
        if query_output['layered']:
            synthesis_text         += f"Make sure to layer while adding."
        if query_output['stirred']:
            synthesis_text         += f"Stir while adding."
    
    elif "HeatChill" in query_output["step"]:
        synthesis_text          += f"""{query_output["stepnumber"]}. HeatChill: Heat or chill {query_output["vesselName"]}  to temperature {query_output["temperature"]} with a 
        rate of {query_output["temperatureRate"]} using {query_output["heatChillDevice"]} {additional_text}. 
        """
        if query_output["vacuum"]:
            synthesis_text          += f"Keep under vacuum for heating."
        if query_output["sealed"]:
            synthesis_text          += f"Seal the vessel for the heating process or keep sealed."
    
    elif "Filter" in query_output["step"]:
        # distinguish between washing and filtering:
        if query_output["vacuum"]:
            additional_text  += f"under vacuum."
        if 'speciesLabels' in query_output and query_output['speciesLabels'] != "" and query_output['speciesLabels'] != "N/A":
          species_list          = query_output['speciesLabels'].split(",")
          species_string        = ""
          for species in species_list[:3]:
             species_string     += (species + " ")
          synthesis_text       += f"""{query_output["stepnumber"]}. Wash: Wash the solid in {query_output["vesselName"]} with {query_output['speciesAmount']} of {species_string}. Repeat {query_output['repeated']} times. Filter {additional_text}.
            """
        else: 
          synthesis_text      += f"""{query_output["stepnumber"]}. Filter content of {query_output["vesselName"]} repeat {query_output['repeated']} times. Perform all filtrations {additional_text}.
            """           
    elif "Dry" in query_output["step"]:
        # distinguish between washing and filtering:
        synthesis_text        += f"""
{query_output["stepnumber"]}. Dry:Dry the content of vessel {query_output["vesselName"]} {additional_text}
        """          
        if "temperature" in query_output:
           synthesis_text        += f"Dry at a temperature of {query_output["vesselName"]}"
           # check key
        if "pressure" in query_output:
           synthesis_text        += f"Dry at {query_output["pressure"]}"
        if 'speciesLabels' in query_output and query_output['speciesLabels'] != "" and query_output['speciesLabels'] != "N/A":
          species_list          = query_output['speciesLabels'].split(",")
          species_string        = ""
          for species in species_list[:3]:
             species_string     += (species + " ")
          synthesis_text        += f"""Use {query_output['speciesAmount']} of {species_string} as drying agent.
            """
    if "comment" in query_output:
        synthesis_text          += f"{query_output["comment"]}"

    synthesis_text          += f"\n"
    return synthesis_text

def write_characterisation_text(query_output:dict, synthesis_text:str):
    print("characterisation data: ", query_output)
    synthesis_text                  += f"\n Characterization of {query_output['ChemicalOutput']} \n"
    species_string        = ""
    species_list          = query_output['NMRSolventNames'].split(",")
    for species in species_list[:3]:
        species_string     += (species + " ")
    synthesis_text                  += f"""
- IR characterization: Ir measured with a {query_output['irDeviceLabels']} values are: {query_output['IRx1s']}.
- NMR characterization: NMR measured with a {query_output['nmrDeviceLabels']} and solvent {species_string} values are: {query_output['nmraxis_com']}
- Elemental analysis characterization: Measured with a {query_output['eleDeviceLabels']} values are: {query_output['elementalAnalysis']} calculated values based on molecular formula {query_output['molecularFormulas']}.
- Yield: {query_output['yield']}
    """
    return synthesis_text

def generate_synthesis_text_llm(txt, doi):
    prompt      = llmp.synthesis_text_prompt(txt)
    messages    =[
              {"role": "system","content": "You are a Chemist at a University and you translate abstract synthesis texts in experimental procedures that could be published in a scientific paper. "},
              {"role": "user", "content": prompt} ]
    chatgpt_api                 = llme.ChatGPTAPI()
    response                    = chatgpt_api.make_llm_call(messages)
    ouptut_path_llm             = os.path.join(para.DATA_FOLDER, f"synText/{doi.replace("/", "_")}_llm.md")
    pdf_converter               = utils.PdfConverter("", "")
    pdf_converter.save_text_to_file(response, ouptut_path_llm)

def generate_syn_text():
    script_dir       = os.path.dirname(os.path.abspath(__file__))
    #doi              = "10.1021/ic501012e"
    # specify doi of the paper that should be processed
    doi              = "10.1021/jacs.8b05780"
    query_output     = kgq.query_synthesis_full(doi)
    query_char       = kgq.query_characterisation(doi)
    # steps:
    synthesis_text              = ""
    for steps in query_output:
        synthesis_text          = write_step_text(steps, synthesis_text, doi)
        print("syn text: ", synthesis_text)
    for product in query_char:
        synthesis_text          = write_characterisation_text(product, synthesis_text)
    print("query characterisation: ", query_char)
    ouptut_path                 = os.path.join(script_dir, f"../Data/synText/{doi.replace("/", "_")}.md")
    pdf_converter               = utils.PdfConverter(ouptut_path, ouptut_path)
    pdf_converter.save_text_to_file(synthesis_text, ouptut_path)
    # generate text with llm: 
    generate_synthesis_text_llm(synthesis_text, doi)


# Example usage:
if __name__ == "__main__":
    generate_syn_text()