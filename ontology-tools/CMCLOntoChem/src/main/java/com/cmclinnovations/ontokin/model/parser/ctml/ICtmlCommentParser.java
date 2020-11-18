package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;

/**
 * An interface to the class that implements the ctml comment parser.
 * 
 * @author msff2
 *
 */
public interface ICtmlCommentParser {
	public void parse(String qName, Attributes attributes);
}
