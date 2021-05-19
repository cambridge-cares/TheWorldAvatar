package com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group;


import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Component;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.DerivedProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Property;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Holds the structure of the PrIMe DataGroup block.
 * 
 * @author Songyi Deng  (sd626@cam.ac.uk)
 *
 */
@XmlType(propOrder={ "component", "speciesLink", "derivedProperty" })
@XmlAccessorType(XmlAccessType.FIELD)
public class DataGroupProperty extends Property {
	@XmlElement
	private DataGroupPropertyComponent component;
	@XmlElement
	private SpeciesLink speciesLink;
	@XmlElement
	private DerivedProperty derivedProperty;
	
	public DataGroupPropertyComponent getPropertyComponent() {
		return component;
	}
	public void setPropertyComponent(DataGroupPropertyComponent component) {
		this.component = component;
	}
	
	public SpeciesLink getPropertySpeciesLink() {
		return speciesLink;
	}
	public void setPropertySpeciesLink(SpeciesLink speciesLink) {
		this.speciesLink = speciesLink;
	}
	
	public DerivedProperty getPropertyDerivedProperty() {
		return derivedProperty;
	}
	public void setPropertyDerivedProperty(DerivedProperty derivedProperty) {
		this.derivedProperty = derivedProperty;
	}
}
