package com.cmclinnovations.ontochem.model.parser.ctml.reactiondata;

import org.xml.sax.Attributes;

public interface IReactionParser {
	public void parse(String qName, Attributes attributes);
}
