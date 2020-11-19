package com.cmclinnovations.ontochem.model.reference.data.structure;
/**
 * Represents the model of the creator of an artifact,</br>
 * e.g. mechanism and ontology.
 * 
 * @author Feroz Farazi(msff2@cam.ac.uk)
 *
 */
public class Creator {
	
	private String familyName;
	
	private String givenName;
	
	private String name;

	public String getFamilyName() {
		return familyName;
	}

	public void setFamilyName(String familyName) {
		this.familyName = familyName;
	}

	public String getGivenName() {
		return givenName;
	}

	public void setGivenName(String givenName) {
		this.givenName = givenName;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
