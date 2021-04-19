package com.cmclinnovations.ontochemexp.model.data.structure.prime;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Apparatus;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.BibliographyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonProperties;
//import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonPropertiesPropertyComponent;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.copyright.Copyright;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroup;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.preferred_key.PreferredKey;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Holds elements that are one level below the experiment element in the 
 * PrIMe experiment file. Also holds attributes of the experiment element.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@XmlRootElement
@XmlType(propOrder={ "copyright", "preferredKey", "bibliographyLink", "apparatus",  "commonProperties", "dataGroup", "additionalDataItem"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Experiment {
	@XmlAttribute
	private String primeID;
//	@XmlAttribute(name="xmlns")
//	private String xmlns;
//	@XmlAttribute(name="xmlns:xsi")
//	private String xmlnsXsi;
//	@XmlAttribute(name="xsi:schemaLocation")
//	private String xsiSchemaLocation;
	
	public String getPrimeID() {
		return primeID;
	}
	public void setPrimeID(String primeID) {
		this.primeID = primeID;
	}
	
//	public String getXmlns() {
//		return xmlns;
//	}
//	
//	public void setXmlns(String xmlns) {
//		this.xmlns = xmlns;
//	}
//	
//	public String getXmlnsXsi() {
//		return xmlnsXsi;
//	}
//	
//	public void setXmlnsXsi(String xmlnsXsi) {
//		this.xmlnsXsi = xmlnsXsi;
//	}
//	
//	public String getXsiSchemaLocation() {
//		return xsiSchemaLocation;
//	}
//	
//	public void setXsiSchemaLocation(String xsiSchemaLocation) {
//		this.xsiSchemaLocation = xsiSchemaLocation;
//	}
	
	@XmlElement
	private Apparatus apparatus;
	@XmlElement
	private ArrayList<AdditionalDataItem> additionalDataItem;
	@XmlElement
	@XmlPath("node[@name='bibliographyLink']")
	private ArrayList <BibliographyLink> bibliographyLink;
	@XmlElement
	private Copyright copyright;
    @XmlElement
    private CommonProperties commonProperties;
	@XmlElement
	private ArrayList<DataGroup> dataGroup;
	@XmlElement
	private PreferredKey preferredKey;
	
	public Apparatus getApparatus() {
		return apparatus;
	}
	public void setApparatus(Apparatus apparatus) {
		this.apparatus = apparatus;
	}

	public ArrayList<AdditionalDataItem> getAdditionalDataItem() {
		return additionalDataItem;
	}
	public void setAdditionalDataItem(ArrayList<AdditionalDataItem> additionalDataItem) {
		this.additionalDataItem = additionalDataItem;
	}
	
	public ArrayList<BibliographyLink> getBibliographyLink() {
		return bibliographyLink;
	}
	public void setBibliographyLink(ArrayList<BibliographyLink> bibliographyLink) {
		this.bibliographyLink = bibliographyLink;
	}
	
	public Copyright getCopyright() {
		return copyright;
	}
	public void setCopyright(Copyright copyright) {
		this.copyright = copyright;
	}

	public CommonProperties getCommonProperties() {
		return commonProperties;
	}
	public void setCommonProperties(CommonProperties commonProperties) {
		this.commonProperties = commonProperties;
	}
	
	public ArrayList<DataGroup> getDataGroup() {
		return dataGroup;
	}
	public void setDataGroup(ArrayList<DataGroup> dataGroup) {
		this.dataGroup = dataGroup;
	}

	public PreferredKey getPreferredKey() {
		return preferredKey;
	}
	public void setPreferredKey(PreferredKey preferredKey) {
		this.preferredKey = preferredKey;
	}
}
