package com.cmclinnovations.ontochem.model.parser.ctml.speciesdata;

import org.xml.sax.Attributes;

public interface ISpeciesParser {
	public void parse(String qName, Attributes attributes);
}
