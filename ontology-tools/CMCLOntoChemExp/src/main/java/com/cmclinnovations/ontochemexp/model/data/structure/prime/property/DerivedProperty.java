package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

@XmlType(propOrder = { "description", "id", "label", "name", "units", "feature" })
@XmlAccessorType(XmlAccessType.FIELD)
public class DerivedProperty {
	
	@XmlAttribute
	private String description;
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String label;
	@XmlAttribute
	private String name;
	@XmlAttribute
	private String units;
	
	public String getDerivedPropertyDescription() {
		return description;
	}
	public void setDerivedPropertyDescription(String description) {
		this.description = description;
	}
	
	public String getDerivedPropertyId() {
		return id;
	}
	public void setDerivedPropertyId(String id) {
		this.id = id;
	}
	
	public String getDerivedPropertyLabel() {
		return label;
	}
	public void setDerivedPropertyLabel(String label) {
		this.label = label;
	}
	
	public String getDerivedPropertyName() {
		return name;
	}
	public void setDerivedPropertyName(String name) {
		this.name = name;
	}
	
	public String getDerivedPropertyUnits() {
		return units;
	}
	public void setDerivedPropertyUnits(String units) {
		this.units = units;
	}
	
	@XmlElement
	private Feature feature;
	
	public Feature getDerivedPropertyFeature() {
		return feature;
	}
	public void setDerivedPropertyFeature(Feature feature) {
		this.feature = feature;
	}
}
