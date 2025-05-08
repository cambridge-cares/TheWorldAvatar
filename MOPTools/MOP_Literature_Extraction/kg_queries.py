import os 
import sys
import utils
PROCESSING_DIR      = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
import upload_utils as uputils
import predefine_iris as piris

def query_characterisation(doi:str):
    """
    Queries chemical characterization data from an OntoSynthesis knowledge graph based on a given DOI.
    
    Parameters:
        doi (str): The DOI of the document describing the chemical synthesis.
        
    Returns:
        list: A list of dictionaries containing chemical output data, including yield, elemental analysis,
              IR and NMR spectroscopy data, and molecular formula information.
    """
    # Initialize connection to OntoSynthesis database
    updater                             = utils.get_client("OntoSynthesisConnection")
    # Define SPARQL query with necessary prefixes and structured selection of relevant chemical characterization data
    query                               = f"""
    PREFIX osyn: <{piris.ONTOSYN_BASE}>  
    PREFIX skos: <{piris.SKOS_BASE}>
    PREFIX om: <{piris.ONTOMEASURE_BASE}>
    PREFIX rdfs: <{piris.RDFS_BASE}>
    PREFIX bibo: <{piris.BIBO_BASE}>
    PREFIX os: <{piris.SPECIES_BASE}>
    PREFIX mop: <{piris.MOPS_BASE}>

    SELECT ?ChemicalOutput
        (GROUP_CONCAT(DISTINCT ?Plabel; separator=", ") AS ?Plabels) 
        (GROUP_CONCAT(DISTINCT ?irDeviceLabel; SEPARATOR=", ") AS ?irDeviceLabels)
        (GROUP_CONCAT(DISTINCT CONCAT(STR(?IRx1), " ", ?iraxis, " (", STR(?IRX1Com), ")"); SEPARATOR=", ") AS ?IRx1s)
        (CONCAT(STR(?yieldnum), " ", STR(?yieldUnit)) AS ?yield)
        (GROUP_CONCAT(DISTINCT CONCAT(STR(?nmrx1), " ", STR(?nmraxis), " (", STR(?nmrX1Com), ")"); SEPARATOR=", ") AS ?nmraxis_com)
        (GROUP_CONCAT(DISTINCT CONCAT(STR(?mfnum), " ", STR(?mfunit), " of ", STR(?elelabel)); SEPARATOR=", ") AS ?elementalAnalysis)
        (GROUP_CONCAT(DISTINCT ?nmrDeviceLabel; SEPARATOR=", ") AS ?nmrDeviceLabels)
        (GROUP_CONCAT(DISTINCT ?NMRSolventName; SEPARATOR=", ") AS ?NMRSolventNames)
        (GROUP_CONCAT(DISTINCT ?eleDeviceLabel; SEPARATOR=", ") AS ?eleDeviceLabels)
        (GROUP_CONCAT(DISTINCT ?molecularFormula; SEPARATOR=", ") AS ?molecularFormulas)
    WHERE {{
        # Retrieve the chemical transformation and its output
        ?ChemicalTransformation	osyn:isDescribedBy ?ChemicalSynthesis 	;
            osyn:hasChemicalOutput ?ChemicalOutput .
            OPTIONAL {{ ?ChemicalOutput skos:altLabel ?Plabel . }}
            ?ChemicalSynthesis osyn:hasYield ?Yield .
            ?Yield          om:hasValue      ?YieldValue .
            ?YieldValue     om:hasNumericalValue ?yieldnum ;
                            om:hasUnit ?yieldU .
        ?yieldU rdfs:label ?yieldUnit .	
        # Retrieve various chemical properties and analyses
        ?ChemicalOutput os:hasElementalAnalysis ?EleAnal ;	
            os:hasFourierTransformSpectrum	?FTIR ;
            os:has1H1HNMR ?HNMR .
        # NMR Spectroscopy data
        ?HNMR os:hasInstrumentType ?NMRDevice ;
            os:hasSolvent ?solvent ;
            os:hasSpectraGraph ?nmrSpectraGraph	.
        ?solvent rdfs:label ?NMRSolventName	.
        ?NMRDevice rdfs:label ?nmrDeviceLabel	.
        ?nmrSpectraGraph os:hasPeak ?nmrShift ;
            os:hasX1Axis ?nmraxis .
        ?nmrShift os:hasX1 ?nmrx1 ;
            rdfs:comment ?nmrX1Com .
        # IR Spectroscopy data
        ?FTIR os:hasInstrumentType ?IRDevice ;
            os:hasSpectraGraph ?SpectraGraph .
        ?SpectraGraph os:hasPeak ?irBand ;
            os:hasX1Axis ?iraxis .
        ?irBand os:hasX1 ?IRx1 ;
            rdfs:comment ?IRX1Com .
        ?IRDevice rdfs:label ?irDeviceLabel	.
        
        # Elemental analysis data
        ?EleAnal os:hasElementWeightPercentage	?wper .
        ?wper os:hasMassFraction ?mf ;				
            os:isReferingToElement ?ele .
        ?ele rdfs:label ?elelabel	.
        ?mf om:hasValue ?mfValue .
        ?mfValue om:hasNumericalValue ?mfnum ;
            om:hasUnit ?munit .
        ?munit rdfs:label ?mfunit .	

        # Retrieve source document details
        ?ChemicalSynthesis osyn:retrievedFrom ?doc .
        ?doc bibo:doi "{doi}" .
        
        # Optional: Retrieve elemental analysis device data
        OPTIONAL {{
            ?EleAnal os:hasElementalDevice 	?eleDevice 		.
            ?eleDevice rdfs:label			?eleDeviceLabel .
        }}
        # Optional: Retrieve molecular formula
        OPTIONAL {{ 
            ?EleAnal os:isBasedOnMolecularFormula ?molForm	.
            ?molForm os:value ?molecularFormula .
        }}
        ?doc <{piris.BIBO_BASE}doi> ?doi .
    }}

    # Group results by chemical output, yield number, and yield unit
    GROUP BY ?ChemicalOutput ?yieldnum ?yieldUnit
      """        
    # Execute SPARQL query and retrieve results
    species_labels                        = updater.perform_query(query) 
    # Print and return query results
    print("species: ", species_labels)
    return species_labels

def get_literature(doi:str) -> dict:
    """
    Retrieves literature information based on a given DOI using a SPARQL query.

    Args:
        doi (str): The DOI (Digital Object Identifier) of the literature.

    Returns:
        dict: A dictionary containing the retrieved literature data, 
              including the formula and CCDC number.
    """
    # Print the DOI for debugging purposes
    print("DOI: ", doi)
    # Instantiate the PySparqlClient to interact with the OntoMOP knowledge graph
    updater                             = utils.get_client("OntoMOPConnection")
    # Define the SPARQL query to retrieve literature details using the provided DOI
    query                               = f"""  
                                        PREFIX mop: <{piris.MOPS_BASE}>
                                        SELECT ?Formula ?CCDCNum
                                        WHERE   {{
                                                ?MOPIRI         mop:hasProvenance            ?ProvenanceIRI                  ;
                                                                mop:hasCCDCNumber            ?CCDCNum                        ;
                                                                mop:hasMOPFormula            ?Formula                        .
                                                ?ProvenanceIRI	mop:hasReferenceDOI          "{doi}"                         . 
                                                }}"""
    # Execute the query using the updater client
    mop_cbu                             = updater.perform_query(query) 
    # Print the query output for debugging purposes
    print("mop query output: ", mop_cbu)
    # Return the retrieved data
    return mop_cbu

def get_input_species(doi:str) -> dict:
    """
    Retrieves species labels associated with a given DOI from the OntoSynthesis knowledge graph.

    Parameters:
    doi (str): The DOI of the document referencing the chemical synthesis process.

    Returns:
    dict: A list of species labels extracted from the queried RDF data.
    """
    # Instantiate PySparqlClient to interact with the OntoSynthesis knowledge graph
    updater                             = utils.get_client("OntoSynthesisConnection")
    # Define SPARQL query to retrieve species labels linked to the given DOI
    query                               = f"""
    PREFIX osyn: <{piris.ONTOSYN_BASE}>  
    PREFIX skos: <{piris.SKOS_BASE}>
    PREFIX rdfs: <{piris.RDFS_BASE}>

    SELECT (GROUP_CONCAT(DISTINCT ?label; SEPARATOR=", ") AS ?labels) 
    WHERE {{	
        ?Species (<http://www.w3.org/2000/01/rdf-schema/label> | rdfs:label) ?label .
        ?PhaseComponent <{piris.ONTOCAPE_BASE}material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <{piris.ONTOCAPE_BASE}upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <{piris.ONTOCAPE_BASE}material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?InputChemical osyn:referencesMaterial ?Material                .   
      	?ChemicalSynthesis osyn:hasChemicalInput ?InputChemical         .
        ?ChemicalSynthesis osyn:retrievedFrom ?doc                      .
        ?doc <{piris.BIBO_BASE}doi> "{doi}"                .
    }}
    group by ?Species
                                              """       
    # Execute the SPARQL query using the client 
    species_labels                        = updater.perform_query(query) 
    # Debugging output to check retrieved species labels
    print("species labels: ", species_labels)
    # Extract species labels into a list
    species_list = [species["labels"] for species in species_labels]
    return species_list

def input_for_cbu(doi:str) -> dict:
    """
    Retrieves chemical building unit (CBU) and species information associated with a given DOI 
    from the OntoSynthesis knowledge graph.

    Parameters:
    doi (str): The DOI of the document referencing the chemical synthesis process.

    Returns:
    tuple: A tuple containing three lists:
        - mop_list: List of CCDC numbers for the metal organic polyhedra (MOPs).
        - cbu_list: List of extracted bracket substrings from MOP formulas.
        - species_list: List of species labels retrieved from the queried RDF data.
    """
    # Initialize the knowledge graph client for OntoSynthesis
    updater                             = utils.get_client("OntoSynthesisConnection")
    # Retrieve metal organic polyhedra (MOPs) linked to the given DOI
    mops                                = get_literature(doi)
    # Initialize lists to store results
    species_list                        = [] # Stores species labels retrieved from the knowledge graph
    cbu_list                            = [] # Stores chemical building units (CBUs) extracted from MOP formulas
    mop_list                            = [] # Stores CCDC numbers for the retrieved MOPs
    print("queried mops in ontomops: ", mops)
    # Iterate over retrieved MOPs
    for mop in mops:
      print("mops: ", mop)
      # Extract and store bracketed substrings from the MOP formula as CBUs
      cbu_list.append({utils.extract_bracket_substrings(mop["Formula"])})
      # Construct SPARQL query to retrieve species labels linked to the current MOP
      query                               = f"""
      PREFIX osyn: <{piris.ONTOSYN_BASE}>  
      PREFIX skos: <{piris.SKOS_BASE}>
      PREFIX om: <{piris.ONTOMEASURE_BASE}>
      PREFIX rdfs: <{piris.RDFS_BASE}>
      PREFIX bibo: <{piris.BIBO_BASE}>
      PREFIX mop: <{piris.MOPS_BASE}>	


      SELECT distinct 
            (GROUP_CONCAT(DISTINCT ?label; separator=", ") AS ?allLabels) 
      WHERE {{
      ?ChemicalSynthesis 	osyn:retrievedFrom 			?doc 			.
        ?doc				bibo:doi					?provenance 	.
      ?doc <{piris.BIBO_BASE}doi> 		?doi 			.
        ?transform  osyn:isDescribedBy 	?ChemicalSynthesis ;
                    osyn:hasChemicalOutput ?output 			. 
        ?output 	osyn:isRepresentedBy 	?MOP	.
        ?MOP 		mop:hasCCDCNumber		"{mop["CCDCNum"]}" .
        ?ChemicalSynthesis osyn:hasChemicalInput ?InputChemical.
        
        
        ?InputChemical osyn:referencesMaterial ?Material 	.
        ?Species skos:altLabel ?label .
        ?PhaseComponent <{piris.ONTOCAPE_BASE}material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <{piris.ONTOCAPE_BASE}upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <{piris.ONTOCAPE_BASE}material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?PhaseComponent	<{piris.ONTOCAPE_BASE}upper_level/system.owl#hasProperty>	?PhaseComponentConc	.
        ?PhaseComponentConc om:hasValue		?concval					.
        ?concval	om:hasNumericalValue ?concnum			;
                  <{piris.ONTOCAPE_BASE}upper_level/system.owl#hasUnitOfMeasure>	?cunit					.
        ?cunit	rdfs:label		?concunit				.
        }}
        GROUP BY ?Species  ?doi ?ChemicalSynthesis """  
      # Print query for debugging      
      print("query: ", query)
      # Execute the query using the knowledge graph client
      species_labels                        = updater.perform_query(query) 
      # Print retrieved species labels for debugging
      print("query output: ", species_labels)
      # Store retrieved species labels
      species_list.append(species_labels)
      # Store the CCDC number of the current MOP
      mop_list.append(mop["CCDCNum"])
    # Print the extracted CBU and species lists for verification
    print(cbu_list, species_list)
    # Return the lists containing MOPs, CBUs, and species information
    return mop_list, cbu_list, species_list

def query_mop_names(doi:str):
    """
    Queries and retrieves Metal Organic Polyhedra (MOP) names associated with a given DOI 
    from the OntoSynthesis knowledge graph.

    Parameters:
    doi (str): The DOI of the document referencing the chemical synthesis process.

    Returns:
    list: A list of MOP names (alternative labels) extracted from the queried RDF data.
    """
    # Instantiate the PySparqlClient to interact with the OntoSynthesis knowledge graph
    updater                             = utils.get_client("OntoSynthesisConnection")
    # Construct SPARQL query to retrieve MOP names (alternative labels) linked to the given DOI
    query                               = f"""
    PREFIX skos:    <{piris.SKOS_BASE}>
    PREFIX os:      <{piris.SPECIES_BASE}>
    PREFIX rdfs:    <{piris.RDFS_BASE}>
    PREFIX rdf:     <{piris.RDF_BASE}>
    PREFIX mops:    <{piris.MOPS_BASE}>
    PREFIX osyn:    <{piris.ONTOSYN_BASE}>  
    SELECT ?lab
    WHERE {{
      
      ?chemicalTransformation   osyn:hasChemicalOutput 	      ?chemicalOutput 	  ;
                                osyn:isDescribedBy		        ?chemicalSynthesis 	.
      ?chemicalOutput	          skos:altLabel                    ?lab             .
      ?chemicalSynthesis	      osyn:retrievedFrom 		        ?document			      . 
      ?document 			          <{piris.BIBO_BASE}doi>   "{doi}"		  .
          }}
                                              """   
    # Execute the SPARQL query using the knowledge graph client     
    species_labels                      = updater.perform_query(query) 
    # Print retrieved species labels for debugging
    print("species labels: ", species_labels)
    # Extract species labels into a list
    species_list                        = [species["lab"] for species in species_labels]
    return species_list
# -------------------------------------------------------------------
# generate synthesis text queries
# -------------------------------------------------------------------
def query_characterisation(doi:str):
    """
    Queries and retrieves characterization data for a given DOI from the OntoSynthesis knowledge graph.
    
    Parameters:
    doi (str): The DOI of the document referencing the chemical synthesis process.
    
    Returns:
    list: A list of dictionaries containing characterization data, including:
          - Chemical output names
          - Yield values and units
          - Fourier Transform Infrared Spectroscopy (FTIR) data
          - Nuclear Magnetic Resonance (NMR) data
          - Elemental analysis data
          - Molecular formula information
    """
    # Instantiate the PySparqlClient to interact with the OntoSynthesis knowledge graph
    updater                             = utils.get_client("OntoSynthesisConnection")
    # Construct SPARQL query to retrieve characterization data related to the given DOI
    query                               = f"""
    PREFIX osyn:        <{piris.ONTOSYN_BASE}>  
    PREFIX skos:        <{piris.SKOS_BASE}>
    PREFIX om:          <{piris.ONTOMEASURE_BASE}>
    PREFIX rdfs:        <{piris.RDFS_BASE}>
    PREFIX bibo:        <{piris.BIBO_BASE}>
    PREFIX os:          <{piris.SPECIES_BASE}>
    PREFIX mop:         <{piris.MOPS_BASE}>

    SELECT ?ChemicalOutput
            (GROUP_CONCAT(DISTINCT ?Plabel; separator=", ") AS ?Plabels) 
        (GROUP_CONCAT(DISTINCT ?irDeviceLabel; SEPARATOR=", ") AS ?irDeviceLabels)
        (GROUP_CONCAT(DISTINCT CONCAT(STR(?IRx1), " ", ?iraxis, " (", STR(?IRX1Com), ")"); SEPARATOR=", ") AS ?IRx1s)
            (CONCAT(STR(?yieldnum), " ", STR(?yieldUnit)) AS ?yield)
        (GROUP_CONCAT(DISTINCT CONCAT(STR(?nmrx1), " ", STR(?nmraxis), " (", STR(?nmrX1Com), ")"); SEPARATOR=", ") AS ?nmraxis_com)
        (GROUP_CONCAT(DISTINCT CONCAT(STR(?mfnum), " ", STR(?mfunit), " of ", STR(?elelabel)); SEPARATOR=", ") AS ?elementalAnalysis)
        (GROUP_CONCAT(DISTINCT ?nmrDeviceLabel; SEPARATOR=", ") AS ?nmrDeviceLabels)
        (GROUP_CONCAT(DISTINCT ?NMRSolventName; SEPARATOR=", ") AS ?NMRSolventNames)
        (GROUP_CONCAT(DISTINCT ?eleDeviceLabel; SEPARATOR=", ") AS ?eleDeviceLabels)
        (GROUP_CONCAT(DISTINCT ?molecularFormula; SEPARATOR=", ") AS ?molecularFormulas)
    WHERE {{
        ?ChemicalTransformation	osyn:isDescribedBy ?ChemicalSynthesis 	;
            osyn:hasChemicalOutput ?ChemicalOutput .
            OPTIONAL {{ ?ChemicalOutput skos:altLabel ?Plabel . }}
            ?ChemicalSynthesis osyn:hasYield ?Yield .
            ?Yield          om:hasValue      ?YieldValue .
            ?YieldValue     om:hasNumericalValue ?yieldnum ;
                            om:hasUnit ?yieldU .
        ?yieldU rdfs:label ?yieldUnit .	
        ?ChemicalOutput os:hasElementalAnalysis ?EleAnal ;	
            os:hasFourierTransformSpectrum	?FTIR ;
            os:has1H1HNMR ?HNMR .
        ?HNMR os:hasInstrumentType ?NMRDevice ;
            os:hasSolvent ?solvent ;
            os:hasSpectraGraph ?nmrSpectraGraph	.
        ?solvent rdfs:label ?NMRSolventName	.
        ?NMRDevice rdfs:label ?nmrDeviceLabel	.
        ?nmrSpectraGraph os:hasPeak ?nmrShift ;
            os:hasX1Axis ?nmraxis .
        ?nmrShift os:hasX1 ?nmrx1 ;
            rdfs:comment ?nmrX1Com .
        ?FTIR os:hasInstrumentType ?IRDevice ;
            os:hasSpectraGraph ?SpectraGraph .
        ?SpectraGraph os:hasPeak ?irBand ;
            os:hasX1Axis ?iraxis .
        ?irBand os:hasX1 ?IRx1 ;
            rdfs:comment ?IRX1Com .
        ?IRDevice rdfs:label ?irDeviceLabel	.
        ?EleAnal os:hasElementWeightPercentage	?wper .
        ?wper os:hasMassFraction ?mf ;				
            os:isReferingToElement ?ele .
        ?ele rdfs:label ?elelabel	.
        ?mf om:hasValue ?mfValue .
        ?mfValue om:hasNumericalValue ?mfnum ;
            om:hasUnit ?munit .
        ?munit rdfs:label ?mfunit .	
        ?ChemicalSynthesis osyn:retrievedFrom ?doc .
        ?doc bibo:doi "{doi}" .
        OPTIONAL {{
            ?EleAnal os:hasElementalDevice 	?eleDevice 		.
            ?eleDevice rdfs:label			?eleDeviceLabel .
        }}
        OPTIONAL {{ 
            ?EleAnal os:isBasedOnMolecularFormula ?molForm	.
            ?molForm os:value ?molecularFormula .
        }}
        ?doc <{piris.BIBO_BASE}doi> ?doi .
    }}

    GROUP BY ?ChemicalOutput ?yieldnum ?yieldUnit
    """        
    # Execute the SPARQL query using the knowledge graph client
    species_labels                        = updater.perform_query(query) 
    # Print retrieved characterization data for debugging
    print("species: ", species_labels)
    return species_labels

def query_synthesis_full(doi:str) -> dict:
    """
    Queries and retrieves full synthesis information associated with a given DOI 
    from the OntoSynthesis knowledge graph.

    Parameters:
    doi (str): The DOI of the document referencing the chemical synthesis process.

    Returns:
    dict: A dictionary containing various synthesis-related details, including:
          - Provenance (DOI reference)
          - Chemical outputs and MOP labels
          - Synthesis steps, vessels, and environmental conditions
          - Duration, temperature, concentration, and pressure details
          - Specific synthesis actions (e.g., adding chemicals, filtering, heating)
          - Characterization data such as NMR, FTIR, and elemental analysis
    """
    # Instantiate the PySparqlClient to interact with the OntoSynthesis knowledge graph
    updater                             = utils.get_client("OntoSynthesisConnection")
    # Construct SPARQL query to retrieve synthesis details related to the given DOI
    query                               = f"""
    PREFIX osyn: <{piris.ONTOSYN_BASE}>  
    PREFIX skos: <{piris.SKOS_BASE}>
    PREFIX om: <{piris.ONTOMEASURE_BASE}>
    PREFIX rdfs: <{piris.RDFS_BASE}>
    PREFIX bibo: <{piris.BIBO_BASE}>
    PREFIX mop: <{piris.MOPS_BASE}>
    PREFIX os:  <{piris.SPECIES_BASE}>

    SELECT DISTINCT ?provenance ?Output 
       (GROUP_CONCAT(DISTINCT ?Plabel; separator=", ") AS ?Plabels) 
       (GROUP_CONCAT(DISTINCT ?MOPFormula; separator=", ") AS ?mopLabels) 
       ?ChemicalSynthesis ?step ?stepnumber ?vessel ?vesselName 
       ?vesselType ?environment 
       (CONCAT(STR(?durNumVal), " ", STR(?durUnitLabel)) AS ?Duration) 
       (GROUP_CONCAT(DISTINCT ?speciesLabel; separator=", ") AS ?speciesLabels) 
       (GROUP_CONCAT(DISTINCT CONCAT(STR(?concNumVal), " ", STR(?concUnitLabel)); separator=", ") AS ?speciesAmount)
       (CONCAT(STR(?tempNumVal), " ", STR(?tempUnitLabel)) AS ?temperature) 
       (CONCAT(STR(?volNumVal), " ", STR(?volUnitLabel)) AS ?volume) 
       ?pressure ?rotaryEvaporator ?targetPH ?stirred ?layered ?repeated ?vacuum 
       ?wait ?newVessel ?newVesselName ?newVesselType ?separationType 
       ?separationTypeLabel ?sealed ?heatChillDevice 
       (CONCAT(STR(?tempRateNumVal), " ", STR(?tempRateUnitLabel)) AS ?temperatureRate) 
       ?comment
       
    WHERE {{
        # Retrieve synthesis process linked to the given DOI
        ?ChemicalSynthesis osyn:retrievedFrom ?doc ;
                        osyn:hasSynthesisStep ?step .
        ?doc bibo:doi ?provenance .
        ?step osyn:hasOrder ?stepnumber ;
            osyn:hasVesselEnvironment ?envi.
        ?envi rdfs:label ?environment.
        ?doc <{piris.BIBO_BASE}doi> "{doi}" .
        
        # Retrieve molecular organic polyhedra (MOPs) and chemical outputs
        OPTIONAL {{
            ?chemTrans osyn:isDescribedBy ?ChemicalSynthesis ;
                    osyn:hasChemicalOutput ?Output .
            ?Output osyn:isRepresentedBy ?MOP ;
            os:hasElementalAnalysis ?EleAnal ;	
            os:hasFourierTransformSpectrum	?FTIR ;
            os:has1H1HNMR ?HNMR .
            OPTIONAL {{ ?Output skos:altLabel ?Plabel . }}
            ?MOP mop:hasMOPFormula ?MOPFormula .
        }}
        
        # Retrieve vessel details
        OPTIONAL {{
            ?step osyn:hasVessel ?vessel .
            ?vessel rdfs:label ?vesselName ;
                    osyn:hasVesselType ?vesType .
            ?vesType rdfs:label ?vesselType .
        }}
    
    # Retrieve step duration
    OPTIONAL {{
        ?step osyn:hasStepDuration ?duration .
        ?duration om:hasValue ?durVal .
        ?durVal om:hasNumericalValue ?durNumVal ;
            om:hasUnit ?durUnit .
        ?durUnit rdfs:label ?durUnitLabel .
    }}
    
    # Retrieve chemical additions (e.g., stirring, layering)
    OPTIONAL {{
        ?step a osyn:Add .
        # added species, stirred and layered
        OPTIONAL {{
        ?step osyn:hasAddedChemicalInput ?InputChemical ;
                osyn:isStirred ?stirred ;
                osyn:isLayered ?layered .
        }}
        # target pH
        OPTIONAL {{
        ?step osyn:hasTargetPh ?targetPH .
    }}
  }}
  
  # Retrieve filtering details
  OPTIONAL {{
    ?step a osyn:Filter .
    OPTIONAL {{
      ?step osyn:hasWashingSolvent ?InputChemical .
    }}
    OPTIONAL {{
      ?step osyn:isRepeated ?repeated ;
            osyn:isVacuumFiltration ?vacuum .
    }}
  }}
  
  # Retrieve heating/chilling process details
  OPTIONAL {{
    ?step a osyn:HeatChill ;
            osyn:hasTargetTemperature ?temp ;
            osyn:hasTemperatureRate ?tempRate ;
            osyn:hasVacuum ?vacuum ;
            osyn:isSealed ?sealed ;
            osyn:hasHeatChillDevice ?hcDevice .
    
    ?hcDevice rdfs:label ?heatChillDevice .
    ?temp om:hasValue ?tempVal .
    ?tempVal om:hasNumericalValue ?tempNumVal ;
            om:hasUnit ?tempUnit .
    ?tempUnit rdfs:label ?tempUnitLabel .
    ?tempRate om:hasValue ?tempRateVal .
    ?tempRateVal om:hasNumericalValue ?tempRateNumVal ;
                om:hasUnit ?tempRateUnit .
    ?tempRateUnit rdfs:label ?tempRateUnitLabel .
    }}
    
    # Retrieve species information involved in synthesis steps
    OPTIONAL {{
        ?step a ?type .
        VALUES ?type {{ osyn:Crystallize osyn:Evaporate osyn:Stir }}
        ?temp om:hasValue ?tempVal .
        ?tempVal om:hasNumericalValue ?tempNumVal ;
                om:hasUnit ?tempUnit .
        ?tempUnit rdfs:label ?tempUnitLabel .
    }}
    
    # GENERAL
    # species
    OPTIONAL {{
    ?step a ?type .
    VALUES ?type {{ osyn:Filter osyn:Add osyn:Evaporate osyn:Dissolve }}
    ?InputChemical osyn:referencesMaterial ?Material .
    ?Material <{piris.ONTOCAPE_BASE}material/material.owl#thermodynamicBehaviour> ?SinglePhase .
    ?SinglePhase <{piris.ONTOCAPE_BASE}upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
    
    ?PhaseComponent <{piris.ONTOCAPE_BASE}material/phase_system/phase_system.owl#representsOccurenceOf> ?species .
    ?species ?labelProp ?speciesLabel .
    VALUES ?labelProp {{ rdfs:label skos:altLabel }}
   
    ?PhaseComponent <{piris.ONTOCAPE_BASE}upper_level/system.owl#hasProperty> ?PhaseComponentConc .
    ?PhaseComponentConc om:hasValue ?concVal .
    ?concVal om:hasNumericalValue ?concNumVal ;
             <{piris.ONTOCAPE_BASE}upper_level/system.owl#hasUnitOfMeasure> ?concUnit .
    ?concUnit rdfs:label ?concUnitLabel .
    }}
    }}

    GROUP BY ?provenance ?Output ?mopLabels ?ChemicalSynthesis ?step ?stepnumber ?vessel ?vesselName ?vesselType ?environment ?durNumVal ?durUnitLabel 
            ?speciesLabels ?speciesAmount ?volNumVal ?volUnitLabel ?tempNumVal ?tempUnitLabel ?pressure ?rotaryEvaporator ?targetPH ?stirred ?layered 
            ?repeated ?vacuum ?wait ?newVessel ?newVesselName ?newVesselType ?separationType ?separationTypeLabel ?sealed ?heatChillDevice ?tempRateNumVal 
            ?tempRateUnitLabel ?comment
    ORDER BY ?ChemicalSynthesis ?provenance ?stepnumber

      """        
    # Execute the SPARQL query using the knowledge graph client
    species_labels                        = updater.perform_query(query) 
    # Print retrieved synthesis details for debugging
    print("species: ", species_labels)
    return species_labels


# -------------------------------------------------------------------
# generate upload queries
# -------------------------------------------------------------------
def species_querying(client, species_label):
    """
    Queries species information from the OntoSpecies knowledge graph based on given species labels.

    Parameters:
    client: The SPARQL client used to perform the query.
    species_label (list): A list of species labels to search for in the knowledge graph.

    Returns:
    list: A list of species entities retrieved from the query.
    """
    # Remove 'N/A' values from the species label list to avoid linking to non-existent instances
    species_label               = [item for item in species_label if item != 'N/A']
    # Initialize a string to hold formatted species labels for the query
    insert_string               = ""
    # Loop through each species label to format it correctly for the SPARQL VALUES clause
    for label in species_label:
        # If the label contains double quotes, use triple double quotes for proper escaping
        if '"' in label:
            insert_string += f' """{label}""" '
        else: 
            insert_string += f""" "{label}" """
    # Construct the SPARQL query to retrieve species based on various identifiers (IUPAC name, formula, SMILES, etc.)
    query = f"""
        PREFIX skos:    <{piris.SKOS_BASE}>
        PREFIX os:      <{piris.SPECIES_BASE}>
        PREFIX rdfs:    <{piris.RDFS_BASE}>
        PREFIX rdf:     <{piris.RDF_BASE}>
        SELECT distinct ?Species WHERE {{
        ?Species a os:Species .

        # Use the VALUES clause to match species labels against various properties
        VALUES ?Text {{{insert_string}}}

        ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES)/os:value)
        | rdfs:label | rdf:label | skos:altLabel 
        | <http://www.w3.org/2000/01/rdf-schema/label>) ?Text . 
        }}"""
    # Print the generated query for debugging purposes
    print("species query: ", query)
    # Execute the query using the provided SPARQL client
    query_result                    = client.perform_query(query)
    # Print the query result for debugging purposes
    print("query result: ", query_result)
    return query_result

def species_querying_ontosyn(client, species_label):
    """
    Queries a database for species instances matching the given labels, avoiding 'N/A' values.
    
    Parameters:
    client (object): A database client capable of executing SPARQL queries.
    species_label (list): A list of species labels (strings) to be used in the query.
    
    Returns:
    list: Query results containing matching species instances.
    """
    # Remove 'N/A' values from the list to prevent incorrect matches
    species_label               = [item for item in species_label if item != 'N/A']
    # Initialize an empty string to store formatted values for the SPARQL query
    insert_string               = ""
    # Loop through each label in the list to format it properly for SPARQL VALUES clause
    for label in species_label:
        # Ensure proper formatting of string literals in SPARQL query
        if '"' in label:
            insert_string += f' """{label}""" '     # Triple quotes for labels containing double quotes
        else: 
            insert_string += f""" "{label}" """     # Standard double quotes for regular labels
    # Construct the SPARQL query to fetch species matching the given labels
    query = f"""
        PREFIX skos:    <{piris.SKOS_BASE}>
        PREFIX os:      <{piris.SPECIES_BASE}>
        PREFIX rdfs:    <{piris.RDFS_BASE}>
        PREFIX rdf:     <{piris.RDF_BASE}>
        SELECT distinct ?Species WHERE {{
        ?Species a os:Species .
        VALUES ?Text {{{insert_string}}}
        ?Species (<http://www.w3.org/2000/01/rdf-schema/label> | rdfs:label|rdf:label) ?Text . 
        }}"""
    # Debugging: Print the generated query
    print("species query: ", query)
    # Execute the query using the client
    query_result                    = client.perform_query(query)
    # Debugging: Print the query results
    print("query result: ", query_result)
    # Return the query result
    return query_result

def mop_querying(client, CCDC_number, mop_formula, mop_name):
    """
    Queries a knowledge graph for a Metal-Organic Polyhedron (MOP) using provided identifiers.

    Args:
        client: An object with a `perform_query` method to execute SPARQL queries.
        CCDC_number (str): The CCDC reference number for the MOP.
        mop_formula (str): The chemical formula of the MOP.
        mop_name (list of str): Alternative names for the MOP.

    Returns:
        list: Query results containing MOP identifiers.
    """
    # Remove 'N/A' or other invalid values from input parameters
    CCDC_number             = uputils.remove_na(CCDC_number)
    mop_formula             = uputils.remove_na(mop_formula)
    mop_name                = uputils.remove_na(mop_name)
    print("querying for mop: ", CCDC_number, mop_formula, mop_name)
    insert_string               = ""
    # Construct a formatted list of mop names for use in the SPARQL query
    for label in mop_name:
        if label != "N/A" and label != "" and label != " " and label != 'lab':
            # Format strings correctly for SPARQL syntax
            if '"' in label:
                insert_string += f' """{label}""" '
            else: 
                insert_string += f""" "{label}" """
    print("mop querying: ", insert_string)
    # Construct the SPARQL query
    query = f"""
        PREFIX skos:    <{piris.SKOS_BASE}>
        PREFIX om:      <{piris.MOPS_BASE}>
        PREFIX os:      <{piris.SPECIES_BASE}>
        PREFIX rdfs:    <{piris.RDFS_BASE}>
        PREFIX xsd: 	<{piris.XSD_BASE}>
        SELECT distinct ?MOPIRI
        WHERE {{
        ?MOPIRI a <{piris.MOPS_BASE}MetalOrganicPolyhedron>     .
        # The VALUES clause allows querying using multiple possible identifiers                   
        VALUES ?Text {{"{CCDC_number}" "{mop_formula}" {insert_string}}}
        ?MOPIRI (<{piris.MOPS_BASE}hasMOPFormula>|skos:altLabel|<{piris.MOPS_BASE}hasCCDCNumber>) ?Text .  
        }}
        GROUP BY ?MOPIRI"""
    
    # Execute the SPARQL query using the provided client
    out                     = client.perform_query(query)
    print("used query: ", query)
    print("MOp query result returned: ", out)
    return out

def CBU_querying(client, cbu_formula):
    """
    Queries a knowledge graph for Chemical Building Units (CBU) based on a given formula.
    
    Parameters:
    client (object): An instance of a SPARQL client used to perform queries.
    cbu_formula (str): The formula representing the chemical building unit.
    
    Returns:
    list: A list of IRIs (Internationalized Resource Identifiers) representing matching CBUs.
    """
    # Handle invalid or placeholder values by returning an empty list
    if cbu_formula == "N/A" and cbu_formula == "" and cbu_formula == " " and cbu_formula =='lab':
            return []
    # Ensure the formula is enclosed in square brackets, which might be required by the query format
    if "[" not in cbu_formula:
        cbu_formula             = "["+cbu_formula+"]"
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    # Log the querying process for debugging purposes
    print("querying for cbu: ", cbu_formula)
    # Construct the SPARQL query to search for CBUs matching the given formula
    query = f"""
        PREFIX skos:    <{piris.SKOS_BASE}>
        PREFIX om:      <{piris.MOPS_BASE}>
        PREFIX os:      <{piris.SPECIES_BASE}>
        PREFIX rdfs:    <{piris.RDFS_BASE}>
        PREFIX xsd: 	<{piris.XSD_BASE}>
        SELECT distinct ?CBUIRI
        WHERE {{
        ?CBUIRI a <{piris.MOPS_BASE}ChemicalBuildingUnit> .
        
        ?CBUIRI om:hasCBUFormula "{cbu_formula}" .  
        }}
        GROUP BY ?CBUIRI"""
    # Log the generated query for debugging
    print("Generated SPARQL query: ", query)
    # Execute the query using the provided client
    out                     = client.perform_query(query)
    # Log the query results
    print("Query results (CBU IRIs): ", out)
    return out

def chemicalOutput_querying(client, CCDC_number, mop_formula, mop_name):
    print("querying for mop: ", CCDC_number, mop_formula, mop_name)
    insert_string               = ""
    # break down mop list of strings in a way to insert in a value sparql statement
    for label in mop_name:
        if label != "N/A" and label != "" and label != " " and label != 'lab':
        # Append each formatted element to the result string
            if '"' in label:
                insert_string += f' """{label}""" '
            else: 
                insert_string += f""" "{label}" """
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    query = f"""
        PREFIX skos:    <{piris.SKOS_BASE}>
        PREFIX om:      <{piris.MOPS_BASE}>
        PREFIX os:      <{piris.SPECIES_BASE}>
        PREFIX rdfs:    <{piris.RDFS_BASE}>
        PREFIX xsd: 	<{piris.XSD_BASE}>
        PREFIX osyn:    <{piris.ONTOSYN_BASE}>
        SELECT distinct ?chemicalOutput
        WHERE {{
        ?chemicalTrans      osyn:hasChemicalOutput  ?chemicalOutput         .
        VALUES ?Text {{{insert_string}}}
        ?chemicalOutput  skos:altLabel ?Text .  
                }}
        GROUP BY ?chemicalOutput"""
    # Execute the query using the provided client
    out                     = client.perform_query(query)
    # Print the query for debugging purposes
    print("used query: ", query)
    # Print the query result for reference
    print("MOp query result returned: ", out)
    return out

def doi_querying(client, doi):
    """
    Queries a SPARQL endpoint for documents associated with a given DOI.
    
    Args:
        client: An instance of a SPARQL client to perform the query.
        doi (str): The DOI (Digital Object Identifier) of the document to be queried.
    
    Returns:
        list: Query results containing document references.
    """
    # Construct the SPARQL query to retrieve documents based on the given DOI
    query = f"""
        PREFIX osyn: <{piris.ONTOSYN_BASE}>  
        PREFIX skos: <{piris.SKOS_BASE}>
        PREFIX om: <{piris.ONTOMEASURE_BASE}>
        PREFIX rdfs: <{piris.RDFS_BASE}>
        PREFIX bibo: <{piris.BIBO_BASE}>
        PREFIX mop: <{piris.MOPS_BASE}>

        SELECT distinct ?doc
        WHERE {{
            ?ChemicalSynthesis osyn:retrievedFrom ?doc .
            ?doc bibo:doi ?provenance .
            ?doc <{piris.BIBO_BASE}doi> "{doi}" .
        }}
        group by ?doc"""
    # Execute the query using the provided client
    out                     = client.perform_query(query)
    # Print the query for debugging purposes
    print("used query: ", query)
    return out

def transformation_querying(client, mop_name):
    """
    Queries a SPARQL endpoint to retrieve chemical transformation data 
    associated with a given Metal Organic Polyhedron (MOP) name.

    Args:
        client: An instance of a SPARQL client to perform the query.
        mop_name (list of str): A list of names associated with the MOP.

    Returns:
        list: Query results containing chemical transformations.
    """
    # Print the MOP name being queried
    print("mop name: ", mop_name)
    insert_string               = ""    # String to store formatted MOP names for the SPARQL query
    # Iterate over each name in the mop_name list and format it for the query
    for label in mop_name:
        # Append each formatted element to the result string
        if label != "N/A" and label != "" and label != " " and label != 'lab':  # Exclude invalid or unwanted labels
            # Check if the label contains double quotes and format accordingly
            if '"' in label:
                insert_string += f' """{label}""" '     # Triple quotes for safety
            else: 
                insert_string += f""" "{label}" """     # Standard double-quoted format
    # Print the formatted query input string for debugging
    print("mop querying: ", insert_string)
    # Construct the SPARQL query to retrieve chemical transformations
    query = f"""
        PREFIX skos:    <{piris.SKOS_BASE}>
        PREFIX os:      <{piris.SPECIES_BASE}>
        PREFIX rdfs:    <{piris.RDFS_BASE}>
        PREFIX xsd: 	<{piris.XSD_BASE}>
        PREFIX osyn:    <{piris.ONTOSYN_BASE}>
        SELECT ?chemicalTrans
        WHERE {{
        ?chemicalTrans      osyn:hasChemicalOutput  ?chemicalOutput         .
        VALUES ?Text {{{insert_string}}}
        ?chemicalOutput  skos:altLabel ?Text .  
                }}
        GROUP BY ?chemicalTrans"""
    # Execute the query using the provided client
    out             = client.perform_query(query)
    # Print the query results for debugging purposes
    print("\n ----- \n", out)
    return out
