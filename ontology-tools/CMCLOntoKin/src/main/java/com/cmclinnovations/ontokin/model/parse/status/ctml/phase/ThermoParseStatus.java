package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the thermo tag and its attributes have already been parsed.
 * 
 * @author msff2
 *
 */
public class ThermoParseStatus {

	boolean thermo = false;
	boolean thermoModel = false;
	public boolean isThermo() {
		return thermo;
	}
	public void setThermo(boolean thermo) {
		this.thermo = thermo;
	}
	public boolean isThermoModel() {
		return thermoModel;
	}
	public void setThermoModel(boolean thermoModel) {
		this.thermoModel = thermoModel;
	}
}
