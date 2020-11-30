package com.cmclinnovations.ontokin.model.data.structure.ctml.element;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlAccessorType(XmlAccessType.FIELD)
public class Element {
	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	@XmlAttribute
	private String name;
	@XmlAttribute
	private String atomicWt;
	@XmlAttribute
	private String units;
	@XmlElement
	private String comment;
	
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

	public String getAtomicWt() {
		return atomicWt;
	}

	public void setAtomicWt(String atomicWt) {
		this.atomicWt = atomicWt;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}
}
