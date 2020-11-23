package com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

@XmlType(propOrder = { "symbol", "isotope" })
@XmlAccessorType(XmlAccessType.FIELD)
public class Atom {
	@XmlAttribute
	private String symbol;

	public String getSymbol() {
		return symbol;
	}

	public void setSymbol(String symbol) {
		this.symbol = symbol;
	}

	@XmlAttribute
	private String isotope;

	public String getIsotope() {
		return isotope;
	}

	public void setIsotope(String isotope) {
		this.isotope = isotope;
	}

	@XmlValue
	private String value;

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
}
