package com.cmclinnovations.jps.agent.quantum.calculation;

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

	AGENT_CLASS("DFTAgent"),
	AGENT_JOB_SPACE(AGENT_CLASS.getPropertyName()),
	AGENT_WORKSPACE_DIR(System.getProperty("user.home")),
	HPC_CAMBRIDGE_ADDRESS("login-skylake.hpc.cam.ac.uk"),
	INPUT_FILE_EXTENSION(".com"),
	CHK_POINT_FILE_EXTENSION(".chk"),
	STATUS_FILE_EXTENSION(".txt"),
	STATUS_FILE_NAME("status"),
	SLURM_SCRIPT_FILE_PATH("C:/TOMCAT/webapps/ROOT/script"),
	SLURM_SCRIPT_FILE_NAME("Slurm.sh"),
	PROPERTY_JOB_STATUS("JobStatus:"),
	PROPERTY_JOB_ID("JobId:"),
	PROPERTY_JOB_AGENT_ID("AgentId:"),
	VALUE_JOB_STATUS_NOT_STARTED("not started"),
	VALUE_JOB_STATUS_RUNNING("running"),
	VALUE_JOB_STATUS_COMPLETED("completed"),
	JOB_NO_OF_CORES("%nprocshared=16"),
	JOB_MEMORY("%mem=60GB"),
	JOB_CHK_POINT_FILE_ADDRESS_PART("%Chk=".concat(HPC_CAMBRIDGE_ADDRESS.getPropertyName())),
	SPECIES_CHARGE_ZERO("0"),
	SPECIES_MULTIPLICITY("1"),
	JOB_PRINT_DIRECTIVE("#n"),
	RDF4J_SERVER_URL_FOR_LOCALHOST("http://localhost:8080/rdf4j-server/"),
	RDF4J_ONTOSPECIES_REPOSITORY_ID("ontospecies"),
	PREFIX_BINDING_ONTOSPECIES("PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> \n");
	
	private String propertyName;
	private Property(String propertyName){
		this.propertyName = propertyName;
	}
	
	public String getPropertyName(){
		return propertyName;
	}	
}
