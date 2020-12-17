package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class SpeciesLinkParseStatus {
	boolean speciesLink = false;

	public boolean isSpeciesLink() {
		return speciesLink;
	}

	public void setSpeciesLink(boolean speciesLink) {
		this.speciesLink = speciesLink;
	}

	boolean preferredKey = false;

	public boolean isPreferredKey() {
		return preferredKey;
	}

	public void setPreferredKey(boolean preferredKey) {
		this.preferredKey = preferredKey;
	}

	boolean primeID = false;

	public boolean isPrimeID() {
		return primeID;
	}

	public void setPrimeID(boolean primeID) {
		this.primeID = primeID;
	}

}
