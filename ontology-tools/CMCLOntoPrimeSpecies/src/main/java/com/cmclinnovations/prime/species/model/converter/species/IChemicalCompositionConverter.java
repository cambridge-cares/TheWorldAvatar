package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public interface IChemicalCompositionConverter {
	public void parse(String qName, Attributes attributes);
}
