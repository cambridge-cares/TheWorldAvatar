package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the kinetics tag and its attribute have already been parsed.
 * 
 * @author msff2
 *
 */
public class KineticsParseStatus {
	boolean kinetics = false;
	boolean kineticsModel = false;
	public boolean isKinetics() {
		return kinetics;
	}
	public void setKinetics(boolean kinetics) {
		this.kinetics = kinetics;
	}
	public boolean isKineticsModel() {
		return kineticsModel;
	}
	public void setKineticsModel(boolean kineticsModel) {
		this.kineticsModel = kineticsModel;
	}
}
