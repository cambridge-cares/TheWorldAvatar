package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class DataAttributeLinkParseStatus {
	boolean dataAttributeLink = false;
	boolean id = false;
	boolean primeID = false;

	public boolean isDataAttributeLink() {
		return dataAttributeLink;
	}

	public void setDataAttributeLink(boolean dataAttributeLink) {
		this.dataAttributeLink = dataAttributeLink;
	}

	public boolean isID() {
		return id;
	}

	public void setID(boolean id) {
		this.id = id;
	}
	
	public boolean isPrimeID() {
		return primeID;
	}

	public void setPrimeID(boolean primeID) {
		this.primeID = primeID;
	}
}
