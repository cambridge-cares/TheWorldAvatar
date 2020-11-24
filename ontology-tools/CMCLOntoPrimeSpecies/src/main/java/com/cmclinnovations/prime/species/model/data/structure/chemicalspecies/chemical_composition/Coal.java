package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "specifiedBy" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Coal {
	@XmlAttribute
	private String specifiedBy;

	public String getSpecifiedBy() {
		return specifiedBy;
	}

	public void setSpecifiedBy(String specifiedBy) {
		this.specifiedBy = specifiedBy;
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
