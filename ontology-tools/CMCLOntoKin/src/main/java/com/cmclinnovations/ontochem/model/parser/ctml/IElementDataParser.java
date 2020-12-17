package com.cmclinnovations.ontochem.model.parser.ctml;

import org.xml.sax.Attributes;
/**
 * An interface to the class that implements the element data parser.
 * 
 * @author msff2
 *
 */
public interface IElementDataParser {
	public void parse(String qName, Attributes attributes);
}
