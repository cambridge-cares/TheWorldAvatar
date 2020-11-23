package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlRootElement
@XmlType(propOrder = { "bound", "kind", "transformation", "type" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Uncertainty {
	@XmlAttribute
	private String bound;
	@XmlAttribute
	private String kind;
	@XmlAttribute
	private String transformation;
	@XmlAttribute
	private String type;
	
	public String getUncertaintyBound() {
		return bound;
	}
	public void setUncertaintyBound(String bound) {
		this.bound = bound;
	}
	
	public String getUncertaintyKind() {
		return kind;
	}
	public void setUncertaintyKind(String kind) {
		this.kind = kind;
	}
	
	public String getUncertaintyTransformation() {
		return transformation;
	}
	public void setUncertaintyTransformation(String transformation) {
		this.transformation = transformation;
	}
	
	public String getUncertaintyType() {
		return type;
	}
	public void setUncertaintyType(String type) {
		this.type = type;
	}
	
	@XmlValue
	private String value;
	
	public String getUncertaintyValue() {
		return value;
	}
	public void setUncertaintyValue(String value) {
		this.value = value;
	}
}
