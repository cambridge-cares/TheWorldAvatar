package com.cmclinnovations.ontochem.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses the CTML metadata. 
 * 
 * @author msff2
 *
 */
public interface IMetadataConverter{
	public void parse(String qName, Attributes attributes);
}
