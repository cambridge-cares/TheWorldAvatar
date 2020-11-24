package com.cmclinnovations.ontokin.model.data.structure.ctml.species;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlAccessorType(XmlAccessType.FIELD)
public class SpeciesTransport {
	@XmlAttribute
	private String model;
	@XmlElement
	private TransportComment comment;
	@XmlPath("node[@name='string']")
	private STRING string;
	@XmlPath("node[@name='LJ_welldepth']")
	private LJWellDepth LJ_welldepth;
	@XmlPath("node[@name='LJ_diameter']")
	private LJDiameter LJ_diameter;
	@XmlPath("node[@name='dipoleMoment']")
	private DipoleMoment dipoleMoment;
	@XmlPath("node[@name='polarizability']")
	private Polarizability polarizability;
	@XmlPath("node[@name='rotRelax']")
	private RotRelax rotRelax;
	
	public String getModel() {
		return model;
	}
	public void setModel(String model) {
		this.model = model;
	}
	public TransportComment getComment() {
		return comment;
	}
	public void setComment(TransportComment comment) {
		this.comment = comment;
	}
	public STRING getString() {
		return string;
	}
	public void setString(STRING string) {
		this.string = string;
	}
	public LJWellDepth getLJ_welldepth() {
		return LJ_welldepth;
	}
	public void setLJ_welldepth(LJWellDepth lJ_welldepth) {
		LJ_welldepth = lJ_welldepth;
	}
	public LJDiameter getLJ_diameter() {
		return LJ_diameter;
	}
	public void setLJ_diameter(LJDiameter lJ_diameter) {
		LJ_diameter = lJ_diameter;
	}
	public DipoleMoment getDipoleMoment() {
		return dipoleMoment;
	}
	public void setDipoleMoment(DipoleMoment dipoleMoment) {
		this.dipoleMoment = dipoleMoment;
	}
	public Polarizability getPolarizability() {
		return polarizability;
	}
	public void setPolarizability(Polarizability polarizability) {
		this.polarizability = polarizability;
	}
	public RotRelax getRotRelax() {
		return rotRelax;
	}
	public void setRotRelax(RotRelax rotRelax) {
		this.rotRelax = rotRelax;
	}
}
