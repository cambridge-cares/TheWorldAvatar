package com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography;


import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "preferredKey", "primeID", "doi" })
@XmlAccessorType(XmlAccessType.FIELD)
public class BibliographyLink{
	@XmlAttribute
	private String preferredKey;
	@XmlAttribute
	private String primeID;
	@XmlAttribute
	private String doi;
	
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
	
	public String getDoi() {
		return doi;
	}

	public void setDoi(String doi) {
		this.doi = doi;
	}

	@XmlValue
	private String value;
	
	public String getValue() {
		return value;
	}
	
	public void setValue(String value) {
		this.value = value;
	}
	
	private String title;
	
	private List<Contributor> contributor;
	
	private PublicationSpecification publicationSpecification;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public List<Contributor> getContributor() {
		return contributor;
	}

	public void setContributor(List<Contributor> contributor) {
		this.contributor = contributor;
	}

	public PublicationSpecification getPublicationSpecification() {
		return publicationSpecification;
	}

	public void setPublicationSpecification(PublicationSpecification publicationSpecification) {
		this.publicationSpecification = publicationSpecification;
	}
}
