package com.cmclinnovations.jps.agent.quantum.calculation;

import java.io.File;

/**
 * List of folder created for using across the DFT agent implementation.
 * 
 * @author msff2
 *
 */
public enum Jobs {
	JOB_SPACE("dft-agent-job-space"),
	FOLDER(System.getProperty("user.home").concat(File.pathSeparator+JOB_SPACE.getName())),
	STATUS_FILE("status.txt"),
	ATTRIBUTE_JOB_STATUS("job_status"),
	STATUS_JOB_NOT_STARTED("not started"),
	STATUS_JOB_RUNNING("running"),
	STATUS_JOB_FINISHED("finished"),
	ATTRIBUTE_JOB_ID("job_id"),
	ATTRIBUTE_HPC_ADDRESS("hpc_address"),
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
