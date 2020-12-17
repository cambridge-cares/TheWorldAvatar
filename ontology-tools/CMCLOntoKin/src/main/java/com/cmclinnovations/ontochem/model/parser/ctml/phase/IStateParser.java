package com.cmclinnovations.ontochem.model.parser.ctml.phase;

import org.xml.sax.Attributes;

public interface IStateParser {
	public void parse(String qName, Attributes attributes);
}
