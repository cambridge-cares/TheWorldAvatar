package com.cmclinnovations.ontokin.model.data.structure.ctml.species;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlType(propOrder={"sourceComment", "comment", "note", "atomArray", "density", "size", "thermo", "transport"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Species {
	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	@XmlAttribute
	private String name;
	@XmlAttribute
	private String phase;
	@XmlPath("node[@name='comment']")
	private SpeciesComment comment;
	@XmlPath("node[@name='note']")
	private String note;
	@XmlPath("node[@name='density']")
	private Density density;
	@XmlPath("node[@name='size']")
	private Size size;
	@XmlPath("node[@name='atomArray']")
	private String atomArray;
	@XmlPath("node[@name='thermo']")
	private SpeciesThermo thermo;
	@XmlPath("node[@name='transport']")
	private SpeciesTransport transport;
	
	public String getSourceComment() {
		return sourceComment;
	}

	public void setSourceComment(String sourceComment) {
		this.sourceComment = sourceComment;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPhase() {
		return phase;
	}

	public void setPhase(String phase) {
		this.phase = phase;
	}

	public SpeciesComment getComment() {
		return comment;
	}

	public void setComment(SpeciesComment comment) {
		this.comment = comment;
	}

	public String getNote() {
		return note;
	}

	public void setNote(String note) {
		this.note = note;
	}

	public String getAtomArray() {
		return atomArray;
	}

	public void setAtomArray(String atomArray) {
		this.atomArray = atomArray;
	}

	public SpeciesThermo getThermo() {
		return thermo;
	}

	public void setThermo(SpeciesThermo thermo) {
		this.thermo = thermo;
	}

	public Density getDensity() {
		return density;
	}

	public void setDensity(Density density) {
		this.density = density;
	}

	public Size getSize() {
		return size;
	}

	public void setSize(Size size) {
		this.size = size;
	}

	public SpeciesTransport getTransport() {
		return transport;
	}

	public void setTransport(SpeciesTransport transport) {
		this.transport = transport;
	}
}
