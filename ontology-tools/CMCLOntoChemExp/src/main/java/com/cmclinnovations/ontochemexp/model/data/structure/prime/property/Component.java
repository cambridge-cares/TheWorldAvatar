package com.cmclinnovations.ontochemexp.model.data.structure.prime.property;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlValue;

@XmlAccessorType(XmlAccessType.FIELD)
public class Component {
	@XmlElement
	private SpeciesLink speciesLink;
	@XmlElement
	private Amount amount;
	@XmlElement
	private Uncertainty uncertainty;
	
	public SpeciesLink getComponentSpeciesLink() {
		return speciesLink;
	}
	public void setComponentSpeciesLink(SpeciesLink speciesLink) {
		this.speciesLink = speciesLink;
	}
	
	public Amount getComponentAmount() {
		return amount;
	}
	public void setComponentAmount(Amount amount) {
		this.amount = amount;
	}
	
	public Uncertainty getComponentUncertainty() {
		return uncertainty;
	}
	public void setComponentUncertainty(Uncertainty uncertainty) {
		this.uncertainty = uncertainty;
	}
}
