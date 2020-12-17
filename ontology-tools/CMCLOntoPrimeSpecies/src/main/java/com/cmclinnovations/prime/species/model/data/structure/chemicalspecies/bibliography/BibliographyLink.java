package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.bibliography;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "preferredKey", "primeID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class BibliographyLink {
	@XmlAttribute
	private String preferredKey;

	public String getPreferredKey() {
		return preferredKey;
	}

	public void setPreferredKey(String preferredKey) {
		this.preferredKey = preferredKey;
	}

	@XmlAttribute
	private String primeID;

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
