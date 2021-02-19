package com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata.species;

/**
 * This class contains the getters and setters to the flags that maintain
 * whether or not the size tag and its attribute have already 
 * been parsed.
 * 
 * @author msff2
 *
 */
public class SizeParseStatus {

	boolean speciesSize = false;
	boolean speciesSizeUnits = false;
	public boolean isSpeciesSize() {
		return speciesSize;
	}
	public void setSpeciesSize(boolean speciesSize) {
		this.speciesSize = speciesSize;
	}
	public boolean isSpeciesSizeUnits() {
		return speciesSizeUnits;
	}
	public void setSpeciesSizeUnits(boolean speciesSizeUnits) {
		this.speciesSizeUnits = speciesSizeUnits;
	}
}
