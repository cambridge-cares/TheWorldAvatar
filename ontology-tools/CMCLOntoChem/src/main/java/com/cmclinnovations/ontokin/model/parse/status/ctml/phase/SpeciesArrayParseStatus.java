package com.cmclinnovations.ontokin.model.parse.status.ctml.phase;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not the species array tag of a phase and its attributes have
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class SpeciesArrayParseStatus {
	boolean speciesArray = false;
	boolean speciesDataSrc = false;

	public boolean isSpeciesArray() {
		return speciesArray;
	}
	public void setSpeciesArray(boolean speciesArray) {
		this.speciesArray = speciesArray;
	}
	public boolean isSpeciesDataSrc() {
		return speciesDataSrc;
	}
	public void setSpeciesDataSrc(boolean speciesDataSrc) {
		this.speciesDataSrc = speciesDataSrc;
	}
}
