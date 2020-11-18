package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "id", "variableID" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Observable {
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String variableID;
	
	public String getObservableId() {
		return id;
	}
	public void setObservableId(String id) {
		this.id = id;
	}
	
	public String getObservableVariableID() {
		return variableID;
	}
	public void setObservableVariableID(String variableID) {
		this.variableID = variableID;
	}
	
	@XmlValue
	private String value;
	
	public String getObservableValue() {
		return value;
	}
	public void setObservableValue(String value) {
		this.value = value;
	}
}
