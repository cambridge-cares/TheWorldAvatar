package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.cheb;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class RateCoeffFloatArray {
	@XmlAttribute(name="name")
	private String name;
	@XmlAttribute(name="units")
	private String units;
	@XmlAttribute(name="degreeT")
	private String degreeT;
	@XmlAttribute(name="degreeP")
	private String degreeP;
	@XmlValue
	private String value;
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}

	public String getDegreeT() {
		return degreeT;
	}

	public void setDegreeT(String degreeT) {
		this.degreeT = degreeT;
	}

	public String getDegreeP() {
		return degreeP;
	}

	public void setDegreeP(String degreeP) {
		this.degreeP = degreeP;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
