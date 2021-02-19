package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

public interface IContentParser {
	public void parse(String qName, Attributes attributes);
}
