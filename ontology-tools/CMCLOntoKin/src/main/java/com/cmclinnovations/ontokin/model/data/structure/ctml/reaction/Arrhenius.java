package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

@XmlType(propOrder={"A", "b", "E", "P", "coverage"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Arrhenius {
	@XmlAttribute(name="name")
	private String name;
	@XmlAttribute(name="type")
	private String type;
	@XmlAttribute(name="motz-wise")
	private String motzWise;
	@XmlAttribute(name="species")
	private String species;
	@XmlElement
	private ArrheniusCoefficientA A;
	@XmlElement
	private ArrheniusCoefficientb b;
	@XmlElement
	private ArrheniusCoefficientE E;
	@XmlElement
	private ArrheniusCoefficientP P;
	@XmlElement
	private List<Coverage> coverage;
	
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getMotzWise() {
		return motzWise;
	}

	public void setMotzWise(String motzWise) {
		this.motzWise = motzWise;
	}

	public String getSpecies() {
		return species;
	}

	public void setSpecies(String species) {
		this.species = species;
	}

	public ArrheniusCoefficientA getA() {
		return A;
	}

	public void setA(ArrheniusCoefficientA a) {
		A = a;
	}

	public ArrheniusCoefficientb getB() {
		return b;
	}

	public void setB(ArrheniusCoefficientb b) {
		this.b = b;
	}

	public ArrheniusCoefficientE getE() {
		return E;
	}

	public void setE(ArrheniusCoefficientE e) {
		E = e;
	}

	public ArrheniusCoefficientP getP() {
		return P;
	}

	public void setP(ArrheniusCoefficientP p) {
		P = p;
	}

	public List<Coverage> getCoverage() {
		return coverage;
	}

	public void setCoverage(List<Coverage> coverage) {
		this.coverage = coverage;
	}
}
