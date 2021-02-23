package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses PrIMe DataGroup data and metadata. 
 * 
 * @author Songyi Deng  (sd626@cam.ac.uk)
 *
 */

public interface IDataGroupConverter{
	public void parse(String qName, Attributes attributes);
}
