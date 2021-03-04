package com.cmclinnovations.ontokin.model.data.structure.ctml.reaction;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;
@XmlType(propOrder={"sourceComment", "id", "caseSensitive", "reaction"})
@XmlAccessorType(XmlAccessType.FIELD)
public class ReactionData {
	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String caseSensitive;
	@XmlPath("node[@name='reaction']")
	private ArrayList<Reaction> reaction;

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

	public ArrayList<Reaction> getReaction() {
		return reaction;
	}

	public void setReaction(ArrayList<Reaction> reaction) {
		this.reaction = reaction;
	}
}
