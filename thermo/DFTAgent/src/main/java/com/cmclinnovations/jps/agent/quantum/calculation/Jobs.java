package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.File;

/**
 * List of folder created for using across the DFT agent implementation.
 * 
 * @author msff2
 *
 */
public enum Jobs {
	STATUS_FILE("status.txt"),
	ATTRIBUTE_JOB_STATUS("JobStatus:"),
	STATUS_JOB_NOT_STARTED("not started"),
	STATUS_JOB_RUNNING("running"),
	STATUS_JOB_COMPLETED("completed"),
	ATTRIBUTE_JOB_ID("JobId:"),
	ATTRIBUTE_AGENT_ID("AgentId:"),
	ATTRIBUTE_HPC_ADDRESS("HPCAddress:"),
	EXTENSION_SLURM_FILE(".sh"),
	EXTENSION_INPUT_FILE(".com"),
	JOB_SETUP_SUCCESS_MSG("The job request has been processed and submitted."),
	JOB_SETUP_JSON_ERROR("The JSON request is ill formatted."),
	JOB_SETUP_SLURM_SCRIPT_ERROR("The Slurm script could not be downloaded."),
	JOB_SETUP_INPUT_FILE_ERROR("The input file for the job could not be created."),
	JOB_SETUP_SPECIES_GEOMETRY_ERROR("The geometry of species could not be downloaded from the IRI provided."),
	JOB_SETUP_SPECIES_IRI_MISSING("The species IRI is not provided."),
	JOB_SETUP_ERROR("The requested job could not be set up, therefore, resubmit the job.");
	
	private String property;
	private Jobs(String property){
		this.property = property;
	}
	
	public String getName(){
		return property;
	}
	
}
