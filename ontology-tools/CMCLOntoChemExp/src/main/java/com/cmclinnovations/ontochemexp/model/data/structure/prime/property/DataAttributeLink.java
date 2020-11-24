package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "id", "primeID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class DataAttributeLink {
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String primeID;
	
	public String getDataAttributeLinkId() {
		return id;
	}
	public void setDataAttributeLinkId(String id) {
		this.id = id;
	}
	
	public String getDataAttributeLinkPrimeID() {
		return primeID;
	}
	public void setDataAttributeLinkPrimeID(String primeID) {
		this.primeID = primeID;
	}
	
	@XmlValue
	private String value;
	
	public String getDataAttributeLinkValue() {
		return value;
	}
	public void setDataAttributeLinkValue(String value) {
		this.value = value;
	}
}
