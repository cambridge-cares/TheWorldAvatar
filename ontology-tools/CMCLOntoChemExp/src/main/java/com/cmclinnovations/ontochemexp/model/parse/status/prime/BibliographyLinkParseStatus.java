package com.cmclinnovations.ontochemexp.model.parse.status.prime;

public class BibliographyLinkParseStatus {
	boolean bibliographyLink = false;
	boolean preferredKey = false;
	boolean primeID = false;

	public boolean isBibliographyLink() {
		return bibliographyLink;
	}

	public void setBibliographyLink(boolean bibliographyLink) {
		this.bibliographyLink = bibliographyLink;
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
}