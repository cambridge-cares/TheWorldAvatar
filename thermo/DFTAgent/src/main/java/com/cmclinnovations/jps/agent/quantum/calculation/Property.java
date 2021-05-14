package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.File;

/**
 * This enumerated list defines the name of important properties of</br>
 * DFT Agent. Some example properties are:</br>
 * - the name of the agent class</br>
 * - the name of the workspace folder on the machine where the agent runs</br>
 * - the name of the workspace folder on HPC where DFT calculations run</br>
 *  
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public enum Property {

	AGENT_WORKSPACE_PARENT_DIR(uk.ac.cam.cares.jps.base.slurm.job.Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName()),
	JOB_NO_OF_CORES_PREFIX("%nprocshared="),
	JOB_MEMORY_PREFIX("%mem="),
	JOB_MEMORY_UNITS("GB"),
	JOB_CHK_POINT_FILE_PREFIX("%Chk="),
	SPECIES_CHARGE_ZERO("0"),
	SPECIES_MULTIPLICITY("1"),
	RDF4J_SERVER_URL_FOR_LOCALHOST("http://localhost:8080/rdf4j-server/"),
	FUSAKI_URL_FOR_WORLD_AVATAR("http://www.theworldavatar.com/damecoolquestion/agents/query?query="),
	RDF4J_ONTOSPECIES_REPOSITORY_ID("ontospecies"),
	PREFIX_BINDING_ONTOSPECIES("PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> \n"),
	PREFIX_BINDING_RDFS("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n"),
	PREFIX_BINDING_RDF("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n"),
	PREFIX_BINDING_MSM("PREFIX msm: <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#>"),
	PREFIX_BINDING_RAM("PREFIX ram: <http://cookingbigdata.com/linkeddata/ccinstances#>"),
	PREFIX_MSM("msm"),
	PREFIX_RAM("ram"),
	DFT_AGENT_IRI("<http://www.theworldavatar.com/kb/agents/Service__DFT.owl#Service>"),
	ONTOCOMPCHEM_KB_IRI("http://www.theworldavatar.com/kb/ontocompchem/"),
	JSON_INPUT_REF_SPECIES("referenceSpecies"),
	JSON_INPUT_UNIQUE_SPECIES_IRI("uniqueSpeciesIRI"),
	JOB_SETUP_SPECIES_GEOMETRY_ERROR("The geometry of species could not be downloaded from the IRI provided."),
	JOB_SETUP_SPECIES_IRI_MISSING("The species IRI is not provided."),
	JOB_SETUP_LEVEL_OF_THEORY_MISSING("Level of theory is not provided.");
	
	
    public static final String JOB_REQUEST_PATH = "/job/request";
    public static final String JOB_STATISTICS_PATH = "/job/statistics";
    public static final String JOB_SHOW_STATISTICS_PATH = "/job/show/statistics";
	
	private String propertyName;
	private int value;
	private Property(String propertyName){
		this.propertyName = propertyName;
	}
	
	public String getPropertyName(){
		return propertyName;
	}
	
	private Property(final int newValue){
		value = newValue;
	}
	
	public int getValue(){
		return value;
	}
}
