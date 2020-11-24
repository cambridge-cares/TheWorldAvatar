package com.cmclinnovations.ontochemexp.model.data.structure.prime.additional_data_item;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "MIME", "itemType", "description" })
@XmlAccessorType(XmlAccessType.FIELD)
public class AdditionalDataItem {
	
	@XmlAttribute
	private String MIME;
	@XmlAttribute
	private String itemType;
	@XmlAttribute
	private String description;
	
	@XmlValue
	private String value;
	
	public String getMIME() {
		return MIME;
	}
	public void setMIME(String MIME) {
		this.MIME = MIME;
	}
	
	public String getItemType() {
		return itemType;
	}
	public void setItemType(String itemType) {
		this.itemType = itemType;
	}
	
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
}
