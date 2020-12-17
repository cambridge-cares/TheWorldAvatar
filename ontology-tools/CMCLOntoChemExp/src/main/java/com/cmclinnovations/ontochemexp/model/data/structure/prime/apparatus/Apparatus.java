package com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Holds the structure of the PrIMe apparatus block.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@XmlType(propOrder = { "kind", "mode", "property" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Apparatus {

	@XmlElement
	private Kind kind;

	@XmlElement
	private ArrayList<Mode> mode;

	@XmlElement
	@XmlPath("node[@name='property']")
	private ArrayList<ApparatusProperty> property;

	public Kind getKind() {
		return kind;
	}

	public void setKind(Kind kind) {
		this.kind = kind;
	}

	public ArrayList<Mode> getMode() {
		return mode;
	}

	public void setMode(ArrayList<Mode> mode) {
		this.mode = mode;
	}

	public ArrayList<ApparatusProperty> getProperty() {
		return property;
	}

	public void setProperty(ArrayList<ApparatusProperty> property) {
		this.property = property;
	}
}
