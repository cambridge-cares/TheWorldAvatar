package com.cmclinnovations.ontochem.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses reaction
 * data and attributes.
 * 
 * @author msff2
 *
 */
public interface IReactionConverter {
	public void parse(String qName, Attributes attributes);
}
