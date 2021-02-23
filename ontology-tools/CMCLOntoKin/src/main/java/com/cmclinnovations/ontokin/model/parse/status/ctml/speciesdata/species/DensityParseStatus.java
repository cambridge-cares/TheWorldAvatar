package com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the density tag and its attribute have already 
 * been parsed.
 * 
 * @author msff2
 *
 */
public class DensityParseStatus {
	boolean speciesDensity = false;
	boolean speciesDensityUnits = false;
	public boolean isSpeciesDensity() {
		return speciesDensity;
	}
	public void setSpeciesDensity(boolean speciesDensity) {
		this.speciesDensity = speciesDensity;
	}
	public boolean isSpeciesDensityUnits() {
		return speciesDensityUnits;
	}
	public void setSpeciesDensityUnits(boolean speciesDensityUnits) {
		this.speciesDensityUnits = speciesDensityUnits;
	}
}
