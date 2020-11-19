package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

public interface IComponentParser {
	public void parse(String qName, Attributes attributes);
}
