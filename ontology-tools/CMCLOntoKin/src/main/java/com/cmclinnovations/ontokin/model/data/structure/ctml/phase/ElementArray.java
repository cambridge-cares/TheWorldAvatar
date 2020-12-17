package com.cmclinnovations.ontokin.model.data.structure.ctml.phase;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class ElementArray {

	@XmlAttribute
	private String datasrc;

	@XmlValue
	private String value;
	
	public String getDatasrc() {
		return datasrc;
	}

	public void setDatasrc(String datasrc) {
		this.datasrc = datasrc;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
