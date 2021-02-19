package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class BibliographyLinkParseStatus {
	boolean bibliographyLink = false;

	public boolean isBibliographyLink() {
		return bibliographyLink;
	}

	public void setBibliographyLink(boolean bibliographyLink) {
		this.bibliographyLink = bibliographyLink;
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
