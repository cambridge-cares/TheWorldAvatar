package com.cmclinnovations.ontochem.model.parser.ctml.elementdata;

import org.xml.sax.Attributes;

public interface IElementParser {
	public void parse(String qName, Attributes attributes);
}
