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
@XmlType(propOrder = { "preferredKey", "primeID", "cas", "inchi", "smiles", "chemName" })
@XmlAccessorType(XmlAccessType.FIELD)
public class SpeciesLink {
	@XmlAttribute
	private String preferredKey;
	@XmlAttribute
	private String primeID;
	@XmlAttribute
	private String cas;
	@XmlAttribute
	private String inchi;
	@XmlAttribute
	private String smiles;
	@XmlAttribute
	private String chemName;
	
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
	
	public String getCas() {
		return cas;
	}
	public void setCas(String cas) {
		this.cas = cas;
	}
	
	public String getInchi() {
		return inchi;
	}
	public void setInchi(String inchi) {
		this.inchi = inchi;
	}
	
	public String getSmiles() {
		return smiles;
	}
	public void setSmiles(String smiles) {
		this.smiles = smiles;
	}

	public String getChemName() {
		return chemName;
	}
	public void setChemName(String chemName) {
		this.chemName = chemName;
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
