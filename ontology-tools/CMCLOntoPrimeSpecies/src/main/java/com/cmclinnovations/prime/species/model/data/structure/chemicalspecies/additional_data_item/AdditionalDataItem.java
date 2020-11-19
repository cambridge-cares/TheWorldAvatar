package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.additional_data_item;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "itemType", "description", "MIME" })
@XmlAccessorType(XmlAccessType.FIELD)
public class AdditionalDataItem {
	@XmlAttribute
	private String itemType;

	public String getItemType() {
		return itemType;
	}

	public void setItemType(String itemType) {
		this.itemType = itemType;
	}

	@XmlAttribute
	private String description;

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@XmlAttribute
	private String MIME;

	public String getMIME() {
		return MIME;
	}

	public void setMIME(String MIME) {
		this.MIME = MIME;
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
