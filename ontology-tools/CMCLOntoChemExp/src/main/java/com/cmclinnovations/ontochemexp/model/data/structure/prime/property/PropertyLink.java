package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "dataGroupID", "propertyID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class PropertyLink {
	@XmlAttribute
	private String propertyID;
	@XmlAttribute
	private String dataGroupID;
	
	public String getPropertyId() {
		return propertyID;
	}
	public void setPropertyId(String propertyID) {
		this.propertyID = propertyID;
	}
	
	public String getDataGroupID() {
		return dataGroupID;
	}
	public void setDataGroupID(String dataGroupID) {
		this.dataGroupID = dataGroupID;
	}
	
	@XmlValue
	private String value;
	
	public String getPropertyLinkValue() {
		return value;
	}
	public void setPropertyLinkValue(String value) {
		this.value = value;
	}
}
