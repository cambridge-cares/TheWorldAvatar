package com.cmclinnovations.ontokin.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses 
 * species data and attributes.
 * 
 * @author msff2
 *
 */
public interface ISpeciesConverter {
	public void parse(String qName, Attributes attributes);
}
