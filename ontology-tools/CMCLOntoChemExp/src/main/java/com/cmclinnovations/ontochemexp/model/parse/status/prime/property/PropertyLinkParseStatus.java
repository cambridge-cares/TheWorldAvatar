package com.cmclinnovations.ontochemexp.model.parse.status.prime.property;

public class PropertyLinkParseStatus {
	boolean propertyLink = false;
	boolean propertyID = false;
	boolean dataGroupID = false;

	public boolean isPropertyLink() {
		return propertyLink;
	}

	public void setPropertyLink(boolean propertyLink) {
		this.propertyLink = propertyLink;
	}

	public boolean isPropertyID() {
		return propertyID;
	}

	public void setPropertyID(boolean propertyID) {
		this.propertyID = propertyID;
	}

	public boolean isDataGroupID() {
		return dataGroupID;
	}

	public void setDataGroupID(boolean dataGroupID) {
		this.dataGroupID = dataGroupID;
	}
}
