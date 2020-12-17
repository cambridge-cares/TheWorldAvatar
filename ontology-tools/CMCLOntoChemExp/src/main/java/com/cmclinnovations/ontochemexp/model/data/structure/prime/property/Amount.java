package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class Amount {
	@XmlAttribute
	private String units;
	
	public String getAmountUnits() {
		return units;
	}
	public void setAmountUnits(String units) {
		this.units = units;
	}
	
	@XmlValue
	private String value;
	
	public String getAmountValue() {
		return value;
	}
	public void setAmountValue(String value) {
		this.value = value;
	}
}
