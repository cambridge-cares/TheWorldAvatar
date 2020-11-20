package com.cmclinnovations.ontokin.model.data.structure.ctml.species;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.persistence.oxm.annotations.XmlPath;
@XmlAccessorType(XmlAccessType.FIELD)
public class SpeciesData {
	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String caseSensitive;
	@XmlPath("node[@name='species']")
	private ArrayList<Species> species;
	
	public String getSourceComment() {
		return sourceComment;
	}

	public void setSourceComment(String sourceComment) {
		this.sourceComment = sourceComment;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getCaseSensitive() {
		return caseSensitive;
	}

	public void setCaseSensitive(String caseSensitive) {
		this.caseSensitive = caseSensitive;
	}

	public ArrayList<Species> getSpecies() {
		return species;
	}

	public void setSpecies(ArrayList<Species> species) {
		this.species = species;
	}
}
