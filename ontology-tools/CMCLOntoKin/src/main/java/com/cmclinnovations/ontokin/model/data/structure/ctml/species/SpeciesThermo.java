package com.cmclinnovations.ontokin.model.data.structure.ctml.species;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.persistence.oxm.annotations.XmlPath;
@XmlAccessorType(XmlAccessType.FIELD)
public class SpeciesThermo {
	@XmlElement
	private String comment;
	@XmlPath("node[@name='NASA']")
	private ArrayList<NASA> NASA;
	
	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public ArrayList<NASA> getNASA() {
		return NASA;
	}

	public void setNASA(ArrayList<NASA> nASA) {
		NASA = nASA;
	}
}
