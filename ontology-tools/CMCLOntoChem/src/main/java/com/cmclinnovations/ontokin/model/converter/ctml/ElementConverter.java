package com.cmclinnovations.ontokin.model.converter.ctml;

import org.xml.sax.Attributes;

/**
 * Implements the method that forwards calls to the following parsers:
 * 1. The ElementData parser.
 * 2. The Element parser.
 * 
 * @author msff2
 *
 */
public class ElementConverter extends CtmlConverter implements IElementConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards the call to
		// the methods that parse the elementData block
		iElementDataParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the elementData block
		iElementParser.parse(qName, attributes);
	}
}
