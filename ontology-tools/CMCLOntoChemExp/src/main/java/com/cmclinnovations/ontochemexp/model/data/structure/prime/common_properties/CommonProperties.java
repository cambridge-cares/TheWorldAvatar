package com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlPath;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.ApparatusProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Holds the structure of the PrIMe commonProperties block.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
//@XmlType(propOrder={"property", "speciesLink"})
@XmlAccessorType(XmlAccessType.FIELD)
public class CommonProperties {
	@XmlElement
	@XmlPath("node[@name='property']")
	private ArrayList<CommonPropertiesProperty> property;
	
	public ArrayList<CommonPropertiesProperty> getProperty() {
		return property;
	}
	public void setProperty(ArrayList<CommonPropertiesProperty> property) {
		this.property = property;
	}
}
