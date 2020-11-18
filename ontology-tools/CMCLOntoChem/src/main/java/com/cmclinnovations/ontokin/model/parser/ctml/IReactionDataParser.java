package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;
/**
 * An interface to the class that implements the reaction data parser.
 * 
 * @author msff2
 *
 */
public interface IReactionDataParser {
	public void parse(String qName, Attributes attributes);
}
