package com.cmclinnovations.ontokin.model.data.structure.ctml.species;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.persistence.oxm.annotations.XmlPath;

@XmlAccessorType(XmlAccessType.FIELD)
public class NASA {
	@XmlAttribute(name="Tmax")
	private String Tmax;
	@XmlAttribute(name="Tmin")
	private String Tmin;
	@XmlAttribute(name="P0")
	private String P0;
	@XmlPath("node[@name='floatArray']")	
	private FloatArray floatArray;

	public String getTmax() {
		return Tmax;
	}

	public void setTmax(String tmax) {
		Tmax = tmax;
	}

	public String getTmin() {
		return Tmin;
	}

	public void setTmin(String tmin) {
		Tmin = tmin;
	}

	public String getP0() {
		return P0;
	}

	public void setP0(String p0) {
		P0 = p0;
	}

	public FloatArray getFloatArray() {
		return floatArray;
	}

	public void setFloatArray(FloatArray floatArray) {
		this.floatArray = floatArray;
	}
}
