package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains the getter and setter to the flag that maintains
 * whether or not the state tag of a phase has already been parsed.
 * 
 * @author msff2
 *
 */
public class StateParseStatus {
	boolean state = false;

	public boolean isState() {
		return state;
	}

	public void setState(boolean state) {
		this.state = state;
	}
}
