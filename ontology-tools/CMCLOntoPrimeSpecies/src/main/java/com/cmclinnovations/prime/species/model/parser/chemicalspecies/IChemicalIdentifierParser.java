package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

public interface IChemicalIdentifierParser {
	public void parse(String qName, Attributes attributes);
}
