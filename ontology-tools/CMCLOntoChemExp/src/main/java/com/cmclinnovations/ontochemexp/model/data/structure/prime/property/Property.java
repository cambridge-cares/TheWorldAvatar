package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "name", "id", "label", "units", "description", "value", "uncertainty" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Property {
	@XmlAttribute
	private String name;
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String label;
	@XmlAttribute
	private String units;
	@XmlAttribute
	private String description;
	
	public String getPropertyName() {
		return name;
	}
	public void setPropertyName(String name) {
		this.name = name;
	}
	
	public String getPropertyId() {
		return id;
	}
	public void setPropertyId(String id) {
		this.id = id;
	}
	
	public String getPropertyLabel() {
		return label;
	}
	public void setPropertyLabel(String label) {
		this.label = label;
	}
	
	public String getPropertyUnits() {
		return units;
	}
	public void setPropertyUnits(String units) {
		this.units = units;
	}
	
	public String getPropertyDescription() {
		return description;
	}
	public void setPropertyDescription(String description) {
		this.description = description;
	}
	
	
	@XmlElement
	private Uncertainty uncertainty;
	@XmlElement
	private Value value;
	
	public Uncertainty getPropertyUncertainty() {
		return uncertainty;
	}
	public void setPropertyUncertainty(Uncertainty uncertainty) {
		this.uncertainty = uncertainty;
	}
	
	public Value getPropertyValue() {
		return value;
	}
	public void setPropertyValue(Value value) {
		this.value = value;
	}
}
