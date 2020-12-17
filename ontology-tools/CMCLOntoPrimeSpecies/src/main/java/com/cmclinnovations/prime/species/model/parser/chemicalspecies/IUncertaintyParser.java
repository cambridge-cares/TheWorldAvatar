package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

public interface IUncertaintyParser {
	public void parse(String qName, Attributes attributes);
}
