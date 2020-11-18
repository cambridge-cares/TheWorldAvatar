package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class SpeciesLinkParseStatus {
	boolean speciesLink = false;
	boolean preferredKey = false;
	boolean primeID = false;

	public boolean isSpeciesLink() {
		return speciesLink;
	}

	public void setSpeciesLink(boolean speciesLink) {
		this.speciesLink = speciesLink;
	}

	public boolean isPreferredKey() {
		return preferredKey;
	}

	public void setPreferredKey(boolean preferredKey) {
		this.preferredKey = preferredKey;
	}

	public boolean isPrimeID() {
		return primeID;
	}

	public void setPrimeID(boolean primeID) {
		this.primeID = primeID;
	}

	private int speciesLinkCount;

	public int getSpeciesLinkCount() {
		return speciesLinkCount;
	}

	public void setSpeciesLinkCount(int speciesLinkCount) {
		this.speciesLinkCount = speciesLinkCount;
	}

}