package com.cmclinnovations.jps.agent.ebr;

/**
 * This enumerated list defines the name of important properties of</br>
 * EBR Agent. Some example properties are:</br>
 * - the name of the agent class</br>
 * - the name of the workspace folder on the machine where the agent runs</br>
 * - the name of the workspace folder on HPC where EBR calculations run</br>
 *  
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public enum Property {

	AGENT_WORKSPACE_PARENT_DIR(uk.ac.cam.cares.jps.base.slurm.job.Property.JOB_WORKSPACE_PARENT_DIR.getPropertyName()),
	EBR_EXECUTABLE("comoenthalpyestimationpaper.jar");
	
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
