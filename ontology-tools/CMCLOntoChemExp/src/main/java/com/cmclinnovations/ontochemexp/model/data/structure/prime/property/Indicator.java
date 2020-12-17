package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "id", "transformation", "variableID", "propertyLink", "dataAttributeLink" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Indicator {
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String transformation;
	@XmlAttribute
	private String variableID;
	
	public String getIndicatorId() {
		return id;
	}
	public void setIndicatorId(String id) {
		this.id = id;
	}
	
	public String getIndicatorTransformation() {
		return transformation;
	}
	public void setIndicatorTransformation(String transformation) {
		this.transformation = transformation;
	}
	
	public String getIndicatorVariableID() {
		return variableID;
	}
	public void setIndicatorVariableID(String variableID) {
		this.variableID = variableID;
	}
	
	@XmlElement
	private PropertyLink propertyLink;
	@XmlElement
	private DataAttributeLink dataAttributeLink;
	
	public PropertyLink getIndicatorPropertyLink() {
		return propertyLink;
	}
	public void setIndicatorPropertyLink(PropertyLink propertyLink) {
		this.propertyLink = propertyLink;
	}
	
	public DataAttributeLink getIndicatorDataAttributeLink() {
		return dataAttributeLink;
	}
	public void setIndicatorDataAttriuteLink(DataAttributeLink dataAttributeLink) {
		this.dataAttributeLink = dataAttributeLink;
	}
}
