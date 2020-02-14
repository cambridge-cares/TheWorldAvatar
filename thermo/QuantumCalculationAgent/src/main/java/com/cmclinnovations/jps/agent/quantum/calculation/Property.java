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

	AGENT_CLASS("DFTAgent"),
	AGENT_JOB_SPACE(AGENT_CLASS.getPropertyName()),
	AGENT_WORKSPACE_DIR(System.getProperty("user.home")),
	HPC_CAMBRIDGE_ADDRESS("login-skylake.hpc.cam.ac.uk"),
	EXTENSION_INPUT_FILE(".com"),
	EXTENSION_CHK_POINT_FILE(".chk"),
	JOB_NO_OF_CORES("%nprocshared=16"),
	JOB_MEMORY("%mem=60GB"),
	JOB_CHK_POINT_FILE(HPC_CAMBRIDGE_ADDRESS.getPropertyName().concat(".chk")),
	JOB_PRINT_DIRECTIVE("#n");
	
	
	private String propertyName;
	private Property(String propertyName){
		this.propertyName = propertyName;
	}
	
	public String getPropertyName(){
		return propertyName;
	}	
}
