package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class SpeciesLinkParseStatus {
	boolean speciesLink = false;
	boolean preferredKey = false;
	boolean primeID = false;
	boolean cas = false;
	boolean inchi = false;
	boolean smiles = false;
	boolean chemName = false;

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
	
	public boolean isCas() {
		return cas;
	}

	public void setCas(boolean cas) {
		this.cas = cas;
	}

	public boolean isInchi() {
		return inchi;
	}

	public void setInchi(boolean inchi) {
		this.inchi = inchi;
	}

	public boolean isSmiles() {
		return smiles;
	}

	public void setSmiles(boolean smiles) {
		this.smiles = smiles;
	}

	public boolean isChemName() {
		return chemName;
	}

	public void setChemName(boolean chemName) {
		this.chemName = chemName;
	}

	private int speciesLinkCount;

	public int getSpeciesLinkCount() {
		return speciesLinkCount;
	}

	public void setSpeciesLinkCount(int speciesLinkCount) {
		this.speciesLinkCount = speciesLinkCount;
	}

}