package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

@XmlType(propOrder={"a", "m", "e"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Coverage {
	@XmlAttribute(name="species")
	private String species;
	@XmlElement
	private CoverageParameterA a;
	@XmlElement
	private CoverageParameterM m;
	@XmlElement
	private CoverageParameterE e;

	public String getSpecies() {
		return species;
	}

	public void setSpecies(String species) {
		this.species = species;
	}

	public CoverageParameterA getA() {
		return a;
	}

	public void setA(CoverageParameterA a) {
		this.a = a;
	}

	public CoverageParameterM getM() {
		return m;
	}

	public void setM(CoverageParameterM m) {
		this.m = m;
	}

	public CoverageParameterE getE() {
		return e;
	}

	public void setE(CoverageParameterE e) {
		this.e = e;
	}
}
