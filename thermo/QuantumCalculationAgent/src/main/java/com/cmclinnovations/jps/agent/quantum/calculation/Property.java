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
	ABSOLUTE_PATH_OF_JOB_SPACE(System.getProperty("user.home").concat(File.separator));
	
	
	private String propertyName;
	private Property(String propertyName){
		this.propertyName = propertyName;
	}
	
	public String getPropertyName(){
		return propertyName;
	}	
}
