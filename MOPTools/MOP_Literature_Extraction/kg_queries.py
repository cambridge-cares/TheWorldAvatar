import os 
import sys
import utils
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
import upload_utils as uputils
def query_characterisation(doi:str):
    # read variables for query from environment file
    updater                             = utils.get_client("OntoSynthesisConnection")
    query                               = f"""
PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>

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
    ?doc <http://purl.org/ontology/bibo/doi> ?doi .
}}

GROUP BY ?ChemicalOutput ?yieldnum ?yieldUnit
      """        
    species_labels                        = updater.perform_query(query) 
    print("species: ", species_labels)
    return species_labels

def get_literature(doi:str) -> dict:
    print("DOI: ", doi)
    # instantiate PySparqlClient
    updater                             = utils.get_client("OntoMOPConnection")
    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]
    query                               = f"""  
                                        PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>
                                        SELECT ?Formula ?CCDCNum
                                        WHERE   {{
                                                ?MOPIRI         mop:hasProvenance            ?ProvenanceIRI                  ;
                                                                mop:hasCCDCNumber            ?CCDCNum                        ;
                                                                mop:hasMOPFormula            ?Formula                        .
                                                ?ProvenanceIRI	mop:hasReferenceDOI          "{doi}"                         . 
                                                }}"""
    
    mop_cbu                             = updater.perform_query(query) 
    print("mop query ouptu: ", mop_cbu)
    return mop_cbu

def get_input_species(doi:str) -> dict:
    # instantiate PySparqlClient
    updater                             = utils.get_client("OntoSynthesisConnection")
    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]

    query                               = f"""
    PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT (GROUP_CONCAT(DISTINCT ?label; SEPARATOR=", ") AS ?labels) 
    WHERE {{	
        ?Species (<http://www.w3.org/2000/01/rdf-schema/label> | rdfs:label) ?label .
        ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?InputChemical osyn:referencesMaterial ?Material .   
      	?ChemicalSynthesis osyn:hasChemicalInput ?InputChemical .
        ?ChemicalSynthesis osyn:retrievedFrom ?doc .
        ?doc <http://purl.org/ontology/bibo/doi> "{doi}" .
    }}
    group by ?Species
                                              """        
    species_labels                        = updater.perform_query(query) 
    print("species labels: ", species_labels)
    species_list                          = []
    for species in species_labels:
      species_list.append(species["labels"])
    return species_list

def input_for_cbu(doi:str) -> dict:
    # initialize KG class
    # instantiate PySparqlClient
    updater                             = utils.get_client("OntoSynthesisConnection")
    mops                                = get_literature(doi)
    species_list                        = []
    cbu_list                            = []
    mop_list                            = []
    print("queried mops in ontomops: ", mops)
    for mop in mops:
      print("mops: ", mop)
      cbu_list.append({utils.extract_bracket_substrings(mop["Formula"])})
      query                               = f"""
      PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX bibo: <http://purl.org/ontology/bibo/>
      PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>	


      SELECT distinct 
            (GROUP_CONCAT(DISTINCT ?label; separator=", ") AS ?allLabels) 
      WHERE {{
      ?ChemicalSynthesis 	osyn:retrievedFrom 			?doc 			.
        ?doc				bibo:doi					?provenance 	.
      ?doc <http://purl.org/ontology/bibo/doi> 		?doi 			.
        ?transform osyn:isDescribedBy 	?ChemicalSynthesis ;
                  osyn:hasChemicalOutput ?output 			. 
        ?output 	osyn:isRepresentedBy 	?MOP	.
        ?MOP 		mop:hasCCDCNumber		"{mop["CCDCNum"]}" .
        ?ChemicalSynthesis osyn:hasChemicalInput ?InputChemical.
        
        
        ?InputChemical osyn:referencesMaterial ?Material 	.
        ?Species skos:altLabel ?label .
        ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#representsOccurenceOf> ?Species .
        ?SinglePhase <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
        ?Material <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehaviour> ?SinglePhase .
        ?PhaseComponent	<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty>	?PhaseComponentConc	.
        ?PhaseComponentConc om:hasValue		?concval					.
        ?concval	om:hasNumericalValue ?concnum			;
                  <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure>	?cunit					.
        ?cunit	rdfs:label		?concunit				.
        }}
        GROUP BY ?Species  ?doi ?ChemicalSynthesis """        
      print("query: ", query)
      species_labels                        = updater.perform_query(query) 
      print("query output: ", species_labels)
      species_list.append(species_labels)
      mop_list.append(mop["CCDCNum"])
    print(cbu_list, species_list)
    return mop_list, cbu_list, species_list

def query_mop_names(doi:str):
    # instantiate PySparqlClient
    updater                             = utils.get_client("OntoSynthesisConnection")

    #where_lit                   = """   ?Provenance	om:hasReferenceDOI      ?DOI     . """
    #select_variables            = """ DISTINCT  ?DOI"""
    #literature_dois             = sparql_point.query_triple(where_lit, select_variables)
    #lit_doi                     = literature_dois[0]
    query                               = f"""
    PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
            PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
            PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX mops: <https://www.theworldavatar.com/kg/ontomops/>
    PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
    SELECT ?lab
    WHERE {{
      
      ?chemicalTransformation   osyn:hasChemicalOutput 	      ?chemicalOutput 	  ;
                                osyn:isDescribedBy		        ?chemicalSynthesis 	.
      ?chemicalOutput	          skos:altLabel                    ?lab             .
      ?chemicalSynthesis	      osyn:retrievedFrom 		        ?document			      . 
      ?document 			          <http://purl.org/ontology/bibo/doi>   "{doi}"		  .
          }}
                                              """        
    species_labels                        = updater.perform_query(query) 
    print("species labels: ", species_labels)
    species_list                          = []
    for species in species_labels:
      species_list.append(species["lab"])
    return species_list
# -------------------------------------------------------------------
# generate synthesis text queries
# -------------------------------------------------------------------
def query_characterisation(doi:str):
    # instantiate PySparqlClient
    updater                             = utils.get_client("OntoSynthesisConnection")
    query                               = f"""
    PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX bibo: <http://purl.org/ontology/bibo/>
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>

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
        ?doc <http://purl.org/ontology/bibo/doi> ?doi .
    }}

    GROUP BY ?ChemicalOutput ?yieldnum ?yieldUnit
    """        
    species_labels                        = updater.sparql_client.perform_query(query) 
    print("species: ", species_labels)
    return species_labels

def query_synthesis_full(doi:str) -> dict:
    # read variables for query from environment file
    updater                             = utils.get_client("OntoSynthesisConnection")
    query                               = f"""
PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX bibo: <http://purl.org/ontology/bibo/>
PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>
PREFIX os:  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

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
  ?ChemicalSynthesis osyn:retrievedFrom ?doc ;
                     osyn:hasSynthesisStep ?step .
  ?doc bibo:doi ?provenance .
  ?step osyn:hasOrder ?stepnumber ;
        osyn:hasVesselEnvironment ?envi.
  ?envi rdfs:label ?environment.
  ?doc <http://purl.org/ontology/bibo/doi> "{doi}" .
	
  # MOPName
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
  
  # Vessel
  OPTIONAL {{
    ?step osyn:hasVessel ?vessel .
    ?vessel rdfs:label ?vesselName ;
            osyn:hasVesselType ?vesType .
    ?vesType rdfs:label ?vesselType .
  }}
  
  # StepDuration
  OPTIONAL {{
    ?step osyn:hasStepDuration ?duration .
    ?duration om:hasValue ?durVal .
    ?durVal om:hasNumericalValue ?durNumVal ;
           om:hasUnit ?durUnit .
    ?durUnit rdfs:label ?durUnitLabel .
  }}
  
  # ADD
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
  
  # FILTER
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
  
  # HEATCHILL
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
  
  # temperature
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
    ?Material <http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehaviour> ?SinglePhase .
    ?SinglePhase <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isComposedOfSubsystem> ?PhaseComponent .
    
    ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#representsOccurenceOf> ?species .
    ?species ?labelProp ?speciesLabel .
    VALUES ?labelProp {{ rdfs:label skos:altLabel }}
   
    ?PhaseComponent <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty> ?PhaseComponentConc .
    ?PhaseComponentConc om:hasValue ?concVal .
    ?concVal om:hasNumericalValue ?concNumVal ;
             <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure> ?concUnit .
    ?concUnit rdfs:label ?concUnitLabel .
  }}
}}

GROUP BY ?provenance ?Output ?mopLabels ?ChemicalSynthesis ?step ?stepnumber ?vessel ?vesselName ?vesselType ?environment ?durNumVal ?durUnitLabel 
         ?speciesLabels ?speciesAmount ?volNumVal ?volUnitLabel ?tempNumVal ?tempUnitLabel ?pressure ?rotaryEvaporator ?targetPH ?stirred ?layered 
         ?repeated ?vacuum ?wait ?newVessel ?newVesselName ?newVesselType ?separationType ?separationTypeLabel ?sealed ?heatChillDevice ?tempRateNumVal 
         ?tempRateUnitLabel ?comment
ORDER BY ?ChemicalSynthesis ?provenance ?stepnumber

      """        
    species_labels                        = updater.sparql_client.perform_query(query) 
    print("species: ", species_labels)
    return species_labels


# -------------------------------------------------------------------
# generate upload queries
# -------------------------------------------------------------------
def species_querying(client, species_label):
    # avoid linking all to N/A instance:
    species_label               = [item for item in species_label if item != 'N/A']
    insert_string               = ""
    # Loop through each element in the list
    for label in species_label:
        #label = re.sub(r'[^\w\s,]', '', label)
        # Append each formatted element to the result string
        if '"' in label:
            insert_string += f' """{label}""" '
        else: 
            insert_string += f""" "{label}" """
    
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT distinct ?Species WHERE {{
        ?Species a os:Species .
        VALUES ?Text {{{insert_string}}}
        ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES)/os:value)|rdfs:label|rdf:label|skos:altLabel|<http://www.w3.org/2000/01/rdf-schema/label>) ?Text . 
        }}"""
    print("species query: ", query)
    query_result                    = client.perform_query(query)
    print("query result: ", query_result)
    return query_result

def species_querying_ontosyn(client, species_label):
    # avoid linking all to N/A instance:
    species_label               = [item for item in species_label if item != 'N/A']
    insert_string               = ""
    # Loop through each element in the list
    for label in species_label:
        #label = re.sub(r'[^\w\s,]', '', label)
        # Append each formatted element to the result string
        if '"' in label:
            insert_string += f' """{label}""" '
        else: 
            insert_string += f""" "{label}" """
    
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT distinct ?Species WHERE {{
        ?Species a os:Species .
        VALUES ?Text {{{insert_string}}}
        ?Species (<http://www.w3.org/2000/01/rdf-schema/label> | rdfs:label|rdf:label) ?Text . 
        }}"""
    print("species query: ", query)
    query_result                    = client.perform_query(query)
    print("query result: ", query_result)
    return query_result

def mop_querying(client, CCDC_number, mop_formula, mop_name):
    CCDC_number             = uputils.remove_na(CCDC_number)
    mop_formula             = uputils.remove_na(mop_formula)
    mop_name                = uputils.remove_na(mop_name)
    print("querying for mop: ", CCDC_number, mop_formula, mop_name)
    insert_string               = ""
    # break down mop list of strings in a way to insert in a value sparql statement
    for label in mop_name:
        if label != "N/A" and label != "" and label != " " and label != 'lab':
            #label = re.sub(r'[^\w\s,]', '', label)
        # Append each formatted element to the result string
            if '"' in label:
                insert_string += f' """{label}""" '
            else: 
                insert_string += f""" "{label}" """
    print("mop querying: ", insert_string)
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX om:      <https://www.theworldavatar.com/kg/ontomops/>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        SELECT distinct ?MOPIRI
        WHERE {{
        ?MOPIRI a <https://www.theworldavatar.com/kg/ontomops/MetalOrganicPolyhedron>                        .
        VALUES ?Text {{"{CCDC_number}" "{mop_formula}" {insert_string}}}
        ?MOPIRI (<https://www.theworldavatar.com/kg/ontomops/hasMOPFormula>|skos:altLabel|<https://www.theworldavatar.com/kg/ontomops/hasCCDCNumber>) ?Text .  
        }}
        GROUP BY ?MOPIRI"""
    out                     = client.perform_query(query)
    print("used query: ", query)
    print("MOp query result returned: ", out)
    return out
def CBU_querying(client, cbu_formula):
    if cbu_formula == "N/A" and cbu_formula == "" and cbu_formula == " " and cbu_formula =='lab':
            return []
    if "[" not in cbu_formula:
        cbu_formula             = "["+cbu_formula+"]"
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    print("querying for cbu: ", cbu_formula)
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX om:      <https://www.theworldavatar.com/kg/ontomops/>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        SELECT distinct ?CBUIRI
        WHERE {{
        ?CBUIRI a <https://www.theworldavatar.com/kg/ontomops/ChemicalBuildingUnit> .
        
        ?CBUIRI om:hasCBUFormula "{cbu_formula}" .  
        }}
        GROUP BY ?CBUIRI"""
    print("query cbu: ", query)
    out                     = client.perform_query(query)
    print("iri cbu: ", out)
    return out
def chemicalOutput_querying(client, CCDC_number, mop_formula, mop_name):
    print("querying for mop: ", CCDC_number, mop_formula, mop_name)
    insert_string               = ""
    # break down mop list of strings in a way to insert in a value sparql statement
    for label in mop_name:
        if label != "N/A" and label != "" and label != " " and label != 'lab':
            #label = re.sub(r'[^\w\s,]', '', label)
        # Append each formatted element to the result string
            if '"' in label:
                insert_string += f' """{label}""" '
            else: 
                insert_string += f""" "{label}" """
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX om:      <https://www.theworldavatar.com/kg/ontomops/>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        PREFIX osyn:    <https://www.theworldavatar.com/kg/OntoSyn/>
        SELECT distinct ?chemicalOutput
        WHERE {{
        ?chemicalTrans      osyn:hasChemicalOutput  ?chemicalOutput         .
        VALUES ?Text {{{insert_string}}}
        ?chemicalOutput  skos:altLabel ?Text .  
                }}
        GROUP BY ?chemicalOutput"""
    out                     = client.perform_query(query)
    print("used query: ", query)
    print("MOp query result returned: ", out)
    return out
def doi_querying(client, doi):
    insert_string               = ""
    query = f"""
        PREFIX osyn: <https://www.theworldavatar.com/kg/OntoSyn/>  
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX bibo: <http://purl.org/ontology/bibo/>
        PREFIX mop: <https://www.theworldavatar.com/kg/ontomops/>

        SELECT distinct ?doc
        WHERE {{
            ?ChemicalSynthesis osyn:retrievedFrom ?doc .
            ?doc bibo:doi ?provenance .
            ?doc <http://purl.org/ontology/bibo/doi> "{doi}" .
        }}
        group by ?doc"""
    out                     = client.perform_query(query)
    print("used query: ", query)
    return out

def transformation_querying(client, mop_name):
    print("mop name: ", mop_name)
    insert_string               = ""
    for label in mop_name:
        # Append each formatted element to the result string
        if label != "N/A" and label != "" and label != " " and label != 'lab':
            #label = re.sub(r'[^\w\s,]', '', label)
            if '"' in label:
                insert_string += f' """{label}""" '
            else: 
                insert_string += f""" "{label}" """
    print("mop querying: ", insert_string)
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        PREFIX osyn:    <https://www.theworldavatar.com/kg/OntoSyn/>
        SELECT ?chemicalTrans
        WHERE {{
        ?chemicalTrans      osyn:hasChemicalOutput  ?chemicalOutput         .
        VALUES ?Text {{{insert_string}}}
        ?chemicalOutput  skos:altLabel ?Text .  
                }}
        GROUP BY ?chemicalTrans"""
    out             = client.perform_query(query)
    print("\n ----- \n", out)
    return out
