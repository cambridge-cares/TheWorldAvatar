package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.content;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "source", "copyrighted", "bibliography" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Content {
	@XmlAttribute
	private String source;

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}

	@XmlAttribute
	private String copyrighted;

	public String getCopyrighted() {
		return copyrighted;
	}

	public void setCopyrighted(String copyrighted) {
		this.copyrighted = copyrighted;
	}

	@XmlAttribute
	private String bibliography;

	public String getBibliography() {
		return bibliography;
	}

	public void setBibliography(String bibliography) {
		this.bibliography = bibliography;
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
