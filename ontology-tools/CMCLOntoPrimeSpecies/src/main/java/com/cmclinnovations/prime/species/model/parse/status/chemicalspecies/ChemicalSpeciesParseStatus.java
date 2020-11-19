package com.cmclinnovations.prime.species.model.parse.status.chemicalspecies;

public class ChemicalSpeciesParseStatus {
	boolean chemicalSpecies = false;

	public boolean isChemicalSpecies() {
		return chemicalSpecies;
	}

	public void setChemicalSpecies(boolean chemicalSpecies) {
		this.chemicalSpecies = chemicalSpecies;
	}

	boolean primeID = false;

	public boolean isPrimeID() {
		return primeID;
	}

	public void setPrimeID(boolean primeID) {
		this.primeID = primeID;
	}

	boolean xmlns = false;

	public boolean isXmlns() {
		return xmlns;
	}

	public void setXmlns(boolean xmlns) {
		this.xmlns = xmlns;
	}

	boolean xmlnsXsi = false;

	public boolean isXmlnsXsi() {
		return xmlnsXsi;
	}

	public void setXmlnsXsi(boolean xmlnsXsi) {
		this.xmlnsXsi = xmlnsXsi;
	}

	boolean xsiSchemaLocation = false;

	public boolean isXsiSchemaLocation() {
		return xsiSchemaLocation;
	}

	public void setXsiSchemaLocation(boolean xsiSchemaLocation) {
		this.xsiSchemaLocation = xsiSchemaLocation;
	}
}
