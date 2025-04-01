import kg_queries as kgq
def step_types_prompt():
    """
    Generates a prompt to guide an LLM in identifying and categorizing synthesis step types.

    Returns:
    str: A structured prompt instructing the LLM to create a JSON file indicating which step types
         are present in the given synthesis procedure.
    """
    # prompt text
    prompt                              = """Task Description:
    Write a JSON file that organizes synthesis steps. Indicate wheter the type of a step is used in the following synthesis procedure. 
    If so write True to for the respective step name and false otherwise. Make sure to fill in all the requried fields.

    Synthesis Text: 
    """
    return prompt

def step_prompt(doi, dynamic_prompt):
    """
    Generates a detailed prompt for an LLM to extract and categorize synthesis steps from a given text.

    Parameters:
    doi (str): The Digital Object Identifier (DOI) of the document containing the synthesis steps.
    dynamic_prompt (dict): A dictionary indicating which step types (e.g., Add, HeatChill, Filter) are relevant.

    Returns:
    str: A structured prompt that instructs the LLM on how to extract and categorize synthesis steps into JSON format.
    """
    # Retrieve supporting information from the knowledge graph
    chemical_names                      = kgq.get_input_species(doi)    # Get the list of chemical species involved in the synthesis
    mop_names                           = kgq.query_mop_names(doi)      # Get the list of metal organic polyhedra (MOPs)
    # Initialize an empty string to store category-specific instructions
    category_spec           = ""
    # Dynamically generate instructions for each relevant synthesis step type
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
    # Construct the prompt text with category-specific details and synthesis step formatting rules     
    prompt                  = f"""              
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
    return prompt

def chemical_prompt(doi):
    """
    Generates a prompt for the LLM to summarize the synthesis procedure and structure it into a JSON file. 
    The prompt focuses on extracting relevant data, such as chemicals, CCDC numbers, and synthesis details.

    Parameters:
    doi (str): The Digital Object Identifier (DOI) of the document containing the synthesis procedure.

    Returns:
    str: A structured prompt instructing the LLM on how to extract and format the synthesis data.
    """
    # Retrieve the list of MOPs (molecular organic precursors) related to the given DOI from the knowledge graph
    mop_formula         = kgq.get_literature(doi)
    # Construct the prompt text, including task specifications and category instructions
    prompt          = f""" 
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
    return prompt 
    
def pre_steps_prompt():
    """
    Generates a structured prompt for an LLM to extract and categorize synthesis pre-steps 
    from a given text.

    Returns:
    str: A formatted prompt instructing the LLM on how to break down synthesis steps 
         into precise, sequential, and standalone instructions.
    """
    # Define the structured prompt that guides the LLM in extracting synthesis steps
    prompt          = f""" 
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
    return prompt

def procedure_prompt(doi):
    """
    Generates a structured prompt for an LLM to extract and format synthesis procedures 
    from a given text.

    Parameters:
    doi (str): The Digital Object Identifier (DOI) of the document containing the synthesis procedures.

    Returns:
    str: A formatted prompt instructing the LLM on how to extract and structure synthesis procedures, 
         ensuring completeness and clarity.
    """
    # Retrieve the list of molecular organic polyhedra (MOP) formulas related to the given DOI
    mop_formula                 = kgq.get_literature(doi)
    # Construct the structured prompt guiding the LLM in extracting synthesis procedures
    prompt                      = f""" 
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
    return prompt    

def cbu_prompt(doi):
    """
    Generates a structured prompt for an LLM to extract and match Chemical Building Units (CBUs) 
    with corresponding lab species based on synthesis data.

    Parameters:
    doi (str): The Digital Object Identifier (DOI) of the document containing synthesis information.

    Returns:
    tuple: A formatted prompt string and a boolean indicating whether the prompt was successfully generated.
           Returns ("", False) if required data is missing.
    """
    # Retrieve the list of MOPs, CBUs, and species involved in synthesis from the knowledge graph
    mop_list, cbu_list, species_list    = kgq.input_for_cbu(doi)
    # Print retrieved data for debugging
    print("cbu list: ", cbu_list, "species_list:", species_list, "mop list: ", mop_list)
    # Check if any required data is missing or empty, return an empty prompt if so
    if cbu_list == [] or species_list == [] or mop_list == [] or species_list==[[]] or mop_list == [[]] or cbu_list == [[]]:
        return "", False 
    # Ensure that none of the species lists are empty
    for species in species_list:
        if species == []:
            return "", False 
    # Construct the structured prompt guiding the LLM in mapping CBUs to corresponding species
    prompt                              =  f"""You will be given different metal organic polyhedron(MOP) CCDC numbers (identifier). Each MOP has two cbus. 
        Extract the relevant data from the synthesis text and structure it into a JSON file adhering to the specified schema.
        You will also be given a list of species that are used to synthesise the MOP. 
        The cbu formulas are just abstractions of the cbu and your task is to find the respecting equivalent(s) from the lab species list. Write the result to a JSON file
        adhering to the specified schema. If any information is missing or uncertain, fill the cell with N/A for strings or 0 for numeric types.
        For chemical names make sure to write each name as separate string. Wrong: ["C4H9NO, DMA, N,N'-dimethylacetamide"], Correct: ["C4H9NO", "DMA", "N,N'-dimethylacetamide"]
        \n"""
    # Append specific MOP, CBU, and species matching instructions for each MOP
    for i, species in enumerate(species_list):
        prompt                         += (f"The {i+1}th Mop has CCDC number: {mop_list[i]}. The MOP has the following two chemical building units (CBUs): {cbu_list[i]}, assign respecting equivalent(s) from the following list of chemicals: {species} \n")
    # Print the generated prompt for debugging
    print("prompt: ", prompt)
    return prompt, True

def characterisation_prompt(doi):
    """
    Generates a structured prompt for an LLM to extract characterization data 
    from synthesis sections of scientific papers.

    Parameters:
    doi (str): The Digital Object Identifier (DOI) of the document containing 
               the synthesis and characterization details.

    Returns:
    str: A formatted prompt instructing the LLM on how to extract and structure 
         characterization data into a JSON format.
    """
    # Retrieve a list of molecular organic polyhedra (MOP) formulas related to the given DOI
    mop_formula             = kgq.get_literature(doi)
    # Retrieve a list of MOP names associated with the DOI
    mop_names               = kgq.query_mop_names(doi)
    # Construct the structured prompt guiding the LLM in extracting characterization data
    prompt = f""" 
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
    return prompt
# output prompt
def synthesis_text_prompt(txt):
    """
    Generates a structured prompt for an LLM to rewrite synthesis procedures 
    in a format suitable for publication in a scientific journal.

    Parameters:
    txt (str): The synthesis text to be reformatted.

    Returns:
    str: A formatted prompt instructing the LLM on how to structure the synthesis procedure 
         into clearly defined sections: Materials, Procedure, and Characterization.
    """
    # Construct the structured prompt guiding the LLM in rewriting the synthesis text
    prompt      = f"""Please translate the following synthesis text in a precise and well written synthesis procedure that could be published in a scientific journal. \n
    synthesis procedure: {txt}. Make sure to copy the product IRI as given in the text and use it as title. Write each procedure indepently and group the information in three paragraphs: 
    Material, Procedure, and Characterisation. Follow markdown syntax and follow a well readable formating.
    
    Example: 
    # Synthesis of HCCF-1
    ## Provenance 
    DOI: 10.1002/chem.201700798 
    ## Materials
    - p-tert-butylthiacalix[4]arene (0.054 g, 0.075 mmol)
    - Cobalt(II) chloride hexahydrate (0.036 g, 0.15 mmol)
    - Cobalt(II) sulfate heptahydrate (0.042 g, 0.15 mmol)
    - 2,2'-bipyridine-4,4'-dicarboxylic acid (0.036 g, 0.15 mmol)
    - Triethylamine (1 drop)
    - Methanol (3.0 mL)
    - Dimethylformamide (3.0 mL)

    ## Equipment 
    - vessel 1 (Teflon-lined stainless-steel vessel)
    - Elemental analysis device: German Elementary Vario EL III
    - Oven

    ## Procedure
    1. To vessel 1, add 0.054 g of p-tert-butylthiacalix[4]arene. Stir continuously while adding to ensure even distribution.
    2. Add 0.036 g of cobalt(II) chloride hexahydrate to the vessel, maintaining stirring to achieve a homogeneous mixture.
    3. Introduce 0.042 g of cobalt(II) sulfate heptahydrate into the vessel, continuing to stir.
    4. Add 0.036 g of 2,2'-bipyridine-4,4'-dicarboxylic acid, ensuring thorough mixing.
    5. Add 1 drop of triethylamine to the mixture, stirring to incorporate.
    6. Pour 3.0 mL of methanol into the vessel, followed by 3.0 mL of dimethylformamide, stirring continuously.
    7. Seal the vessel and heat to 130°C under vacuum in an oven. Maintain this temperature for the required duration.
    8. Gradually cool the vessel to 20°C at a rate of 4°C per hour, keeping it sealed under vacuum.
    9. Filter the contents of the vessel under vacuum to isolate the solid product.
    10. Wash the solid with methanol, filtering under vacuum to remove any impurities.

    ## Characterization
    - Elemental Analysis: Found: C, 49.78%; H, 4.65%; N, 3.45%. Calculated for [(C2H5)3NH]2[Co4(TC4A)(Cl)]2[Co4(TC4A)(SO4)]4(C12H6O4N2)8·(CH3OH)10(DMF)4: C, 50.98%; H, 4.52%.

    Note: IR and NMR characterizations were not specified.
    """
    return prompt