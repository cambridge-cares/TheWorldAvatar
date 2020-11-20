package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.copyright;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class Copyright {
	@XmlValue
	private String value;
	
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
}
