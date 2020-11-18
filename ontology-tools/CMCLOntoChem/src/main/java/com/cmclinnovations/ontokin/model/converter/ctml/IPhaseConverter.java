package com.cmclinnovations.ontokin.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses the 
 * phase data and attributes.
 * 
 * @author msff2
 *
 */
public interface IPhaseConverter {
	public void parse(String qName, Attributes attributes);
}
