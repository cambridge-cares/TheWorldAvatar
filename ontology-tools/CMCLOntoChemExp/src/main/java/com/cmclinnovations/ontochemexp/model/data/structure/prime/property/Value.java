package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class Value {
	@XmlValue
	private String value;
	
	public String getValueValue() {
		return value;
	}
	public void setValueValue(String value) {
		this.value = value;
	}
}
