package com.cmclinnovations.ontokin.model.data.structure.ctml;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlAccessType;

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(propOrder={"reactions", "species"})
public class Validate {

	@XmlAttribute
	private String reactions;

	@XmlAttribute
	private String species;

	public String getReactions() {
		return reactions;
	}

	public void setReactions(String reactions) {
		this.reactions = reactions;
	}

	public String getSpecies() {
		return species;
	}

	public void setSpecies(String species) {
		this.species = species;
	}
}
