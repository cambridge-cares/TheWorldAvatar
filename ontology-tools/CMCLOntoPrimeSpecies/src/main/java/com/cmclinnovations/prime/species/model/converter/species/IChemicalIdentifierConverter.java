package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public interface IChemicalIdentifierConverter {
	public void parse(String qName, Attributes attributes);
}
