package com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.XmlAccessType;

/**
 * Holds the value of apparatus kind.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */

@XmlType(propOrder={"value"})
@XmlAccessorType(XmlAccessType.FIELD)
public class Kind {
	@XmlValue
	private String value;	

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
