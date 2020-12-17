package com.cmclinnovations.ontokin.model.parse.status.ctml.speciesdata;

/**
 * This class contains getters and setters to flags that maintain
 * whether or not a CTML species element or attribute has 
 * already been parsed.
 * 
 * @author msff2
 *
 */
public class SpeciesParseStatus {
	boolean speciesDataSpecies = false;
	boolean speciesName = false;
	boolean speciesPhase = false;
	boolean speciesComment = false;
	boolean speciesNote = false;
	boolean speciesAtomArray = false;
	public boolean isSpeciesDataSpecies() {
		return speciesDataSpecies;
	}
	public void setSpeciesDataSpecies(boolean speciesDataSpecies) {
		this.speciesDataSpecies = speciesDataSpecies;
	}
	public boolean isSpeciesName() {
		return speciesName;
	}
	public void setSpeciesName(boolean speciesName) {
		this.speciesName = speciesName;
	}
	public boolean isSpeciesPhase() {
		return speciesPhase;
	}
	public void setSpeciesPhase(boolean speciesPhase) {
		this.speciesPhase = speciesPhase;
	}
	public boolean isSpeciesNote() {
		return speciesNote;
	}
	public void setSpeciesNote(boolean speciesNote) {
		this.speciesNote = speciesNote;
	}
	public boolean isSpeciesAtomArray() {
		return speciesAtomArray;
	}
	public void setSpeciesAtomArray(boolean speciesAtomArray) {
		this.speciesAtomArray = speciesAtomArray;
	}
	public boolean isSpeciesComment() {
		return speciesComment;
	}
	public void setSpeciesComment(boolean speciesComment) {
		this.speciesComment = speciesComment;
	}

}
