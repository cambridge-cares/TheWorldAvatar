package uk.ac.cam.cares.jps.base.slurm.job;

import java.io.File;

/**
 * This enumerated list defines the name of important properties of</br>
 * Slurm jobs. Some example properties are:</br>
 * - the name of the agent class</br>
 * - the name of the workspace folder on the machine where the agent runs</br>
 * - the name of the workspace folder on HPC where DFT calculations run</br>
 *  
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public enum Property {

	JOB_WORKSPACE_PARENT_DIR(System.getProperty("user.home")),
	MAX_NUMBER_OF_JOBS(10),
	CHK_POINT_FILE_EXTENSION(".chk"),
	STATUS_FILE_NAME("status.txt"),
	JSON_INPUT_FILE_NAME("input.json"),
	SLURM_SCRIPT_FILE_NAME("Slurm.sh");
	
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
