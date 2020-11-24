package com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species;
/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not thermodynamic properties have already been parsed. 
 * 
 * @author msff2
 *
 */
public class SpeciesThermoParseStatus {
	boolean speciesThermo = false;
	boolean thermoComment = false;
	public boolean isSpeciesThermo() {
		return speciesThermo;
	}
	public void setSpeciesThermo(boolean speciesThermo) {
		this.speciesThermo = speciesThermo;
	}
	public boolean isThermoComment() {
		return thermoComment;
	}
	public void setThermoComment(boolean thermoComment) {
		this.thermoComment = thermoComment;
	}
}
