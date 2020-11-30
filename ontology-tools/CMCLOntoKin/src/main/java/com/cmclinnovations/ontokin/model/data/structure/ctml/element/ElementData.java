package com.cmclinnovations.ontokin.model.data.structure.ctml.element;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.persistence.oxm.annotations.XmlPath;
@XmlAccessorType(XmlAccessType.FIELD)
public class ElementData {
	@XmlPath("node[@name='sourceComment']")
	private String sourceComment;
	@XmlAttribute
	private String id;
	@XmlAttribute
	private String caseSensitive;
	@XmlPath("node[@name='element']")
	private ArrayList<Element> element;
	
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

	public ArrayList<Element> getElement() {
		return element;
	}

	public void setElement(ArrayList<Element> element) {
		this.element = element;
	}
}
