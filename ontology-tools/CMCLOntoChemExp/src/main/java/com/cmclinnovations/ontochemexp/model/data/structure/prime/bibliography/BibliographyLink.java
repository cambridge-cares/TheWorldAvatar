package com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography;


import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "preferredKey", "primeID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class BibliographyLink{
	@XmlAttribute
	private String preferredKey;
	@XmlAttribute
	private String primeID;
	
	public String getPreferredKey() {
		return preferredKey;
	}

	public void setPreferredKey(String preferredKey) {
		this.preferredKey = preferredKey;
	}

	public String getPrimeID() {
		return primeID;
	}
	
	public void setPrimeID(String primeID) {
		this.primeID = primeID;		
	}
	
	@XmlValue
	private String value;
	
	public String getValue() {
		return value;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
}
