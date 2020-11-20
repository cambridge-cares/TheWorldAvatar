package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;

import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.bibliography.BibliographyLink;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.ChemicalComposition;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_identifier.ChemicalIdentifier;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.content.Content;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.copyright.Copyright;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.preferred_key.PreferredKey;

@XmlRootElement(name="ns0:chemicalSpecies")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder={"copyright", "content", "bibliographyLink", "preferredKey", "chemicalIdentifier", "chemicalComposition", "additionalDataItemList"})
public class ChemicalSpecies {
	@XmlAttribute
	private String primeID;

	public String getPrimeID() {
		return primeID;
	}

	public void setPrimeID(String primeID) {
		this.primeID = primeID;
	}

	@XmlAttribute(name="xmlns:ns0")
	private String xmlns;

	public String getXmlns() {
		return xmlns;
	}

	public void setXmlns(String xmlns) {
		this.xmlns = xmlns;
	}

	@XmlAttribute(name="xmlns:xsi")
	private String xmlnsXsi;

	public String getXmlnsXsi() {
		return xmlnsXsi;
	}

	public void setXmlnsXsi(String xmlnsXsi) {
		this.xmlnsXsi = xmlnsXsi;
	}

	@XmlAttribute(name="xsi:schemaLocation")
	private String xsiSchemaLocation;

	public String getXsiSchemaLocation() {
		return xsiSchemaLocation;
	}

	public void setXsiSchemaLocation(String xsiSchemaLocation) {
		this.xsiSchemaLocation = xsiSchemaLocation;
	}

	@XmlElement(name="ns0:copyright")
	private Copyright copyright;

	public Copyright getCopyright() {
		return copyright;
	}

	public void setCopyright(Copyright copyright) {
		this.copyright = copyright;
	}

	@XmlElement(name="ns0:content")
	private Content content;

	public Content getContent() {
		return content;
	}

	public void setContent(Content content) {
		this.content = content;
	}

	@XmlElement(name="ns0:bibliographyLink")
	private BibliographyLink bibliographyLink;

	public BibliographyLink getBibliographyLink() {
		return bibliographyLink;
	}

	public void setBibliographyLink(BibliographyLink bibliographyLink) {
		this.bibliographyLink = bibliographyLink;
	}

	@XmlElement(name="ns0:preferredKey")
	private PreferredKey preferredKey;

	public PreferredKey getPreferredKey() {
		return preferredKey;
	}

	public void setPreferredKey(PreferredKey preferredKey) {
		this.preferredKey = preferredKey;
	}

	@XmlElement(name="ns0:chemicalIdentifier")
	private ChemicalIdentifier chemicalIdentifier;

	public ChemicalIdentifier getChemicalIdentifier() {
		return chemicalIdentifier;
	}

	public void setChemicalIdentifier(ChemicalIdentifier chemicalIdentifier) {
		this.chemicalIdentifier = chemicalIdentifier;
	}

	@XmlElement(name="ns0:chemicalComposition")
	private ChemicalComposition chemicalComposition;

	public ChemicalComposition getChemicalComposition() {
		return chemicalComposition;
	}

	public void setChemicalComposition(ChemicalComposition chemicalComposition) {
		this.chemicalComposition = chemicalComposition;
	}

	@XmlElement(name="ns0:additionalDataItem")
	private ArrayList<AdditionalDataItem> additionalDataItemList;

	public ArrayList<AdditionalDataItem> getAdditionalDataItem() {
		return additionalDataItemList;
	}

	public void setAdditionalDataItem(ArrayList<AdditionalDataItem> additionalDataItemList) {
		this.additionalDataItemList = additionalDataItemList;
	}
}
