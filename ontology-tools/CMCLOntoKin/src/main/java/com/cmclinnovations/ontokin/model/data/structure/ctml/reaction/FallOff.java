package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class FallOff {
	@XmlAttribute(name="type")
	private String type;
	@XmlAttribute(name="namedThirdBody")
	private String namedThirdBody;
	@XmlValue
	private String value;
	
	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getNamedThirdBody() {
		return namedThirdBody;
	}

	public void setNamedThirdBody(String namedThirdBody) {
		this.namedThirdBody = namedThirdBody;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
