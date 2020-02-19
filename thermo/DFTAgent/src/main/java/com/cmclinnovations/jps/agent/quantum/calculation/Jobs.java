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
	EXTENSION_INPUT_FILE(".com");
	
	private String property;
	private Jobs(String property){
		this.property = property;
	}
	
	public String getName(){
		return property;
	}
	
}
