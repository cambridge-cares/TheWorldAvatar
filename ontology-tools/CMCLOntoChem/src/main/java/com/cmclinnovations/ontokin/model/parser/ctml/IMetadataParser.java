package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;
/**
 * An interface to the class that implements the metadata parser.
 * 
 * @author msff2
 *
 */
public interface IMetadataParser {
	public void parse(String qName, Attributes attributes);
}
