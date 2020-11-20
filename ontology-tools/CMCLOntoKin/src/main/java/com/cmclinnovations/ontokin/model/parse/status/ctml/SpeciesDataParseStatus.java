package com.cmclinnovations.ontokin.model.parse.status.ctml;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not a CTML species data element or attribute has 
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class SpeciesDataParseStatus {
	boolean speciesData = false;
	boolean speciesDataId = false;
	boolean speciesDataCaseSensitive = false;
	public boolean isSpeciesData() {
		return speciesData;
	}
	public void setSpeciesData(boolean speciesData) {
		this.speciesData = speciesData;
	}
	public boolean isSpeciesDataId() {
		return speciesDataId;
	}
	public void setSpeciesDataId(boolean speciesDataId) {
		this.speciesDataId = speciesDataId;
	}
	public boolean isSpeciesDataCaseSensitive() {
		return speciesDataCaseSensitive;
	}
	public void setSpeciesDataCaseSensitive(boolean speciesDataCaseSensitive) {
		this.speciesDataCaseSensitive = speciesDataCaseSensitive;
	}
}
