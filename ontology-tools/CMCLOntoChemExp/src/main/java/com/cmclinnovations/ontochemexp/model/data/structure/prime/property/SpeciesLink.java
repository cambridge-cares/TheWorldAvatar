package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlMixed;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlRootElement
@XmlType(propOrder = { "preferredKey", "primeID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class SpeciesLink {
	@XmlAttribute
	private String preferredKey;
	@XmlAttribute
	private String primeID;
	
	public String getSpeciesLinkPreferredKey() {
		return preferredKey;
	}
	public void setSpeciesLinkPreferredKey(String preferredKey) {
		this.preferredKey = preferredKey;
	}
	
	public String getSpeciesLinkPrimeID() {
		return primeID;
	}
	public void setSpeciesLinkPrimeID(String primeID) {
		this.primeID = primeID;
	}
	
	@XmlValue
	private String value;

	public String getSpeciesLinkValue() {
		return value;
	}
	public void setSpeciesLinkValue(String value) {
		this.value = value;
	}
}
