package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the phaseArray tag and its value have already been parsed.
 * 
 * @author msff2
 *
 */
public class PhaseArrayParseStatus {
	boolean phaseArray = false;

	public boolean isPhaseArray() {
		return phaseArray;
	}

	public void setPhaseArray(boolean phaseArray) {
		this.phaseArray = phaseArray;
	}
}
