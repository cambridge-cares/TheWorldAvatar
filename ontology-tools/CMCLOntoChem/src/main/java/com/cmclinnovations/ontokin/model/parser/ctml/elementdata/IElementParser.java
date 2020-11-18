package com.cmclinnovations.ontokin.model.parser.ctml.elementdata;

import org.xml.sax.Attributes;

public interface IElementParser {
	public void parse(String qName, Attributes attributes);
}
