package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;

/**
 * Declares the method whose implementation parses PrIMe experiment's data and metadata. 
 * 
 * @author msff2
 *
 */
public interface IExperimentConverter{
	public void parse(String qName, Attributes attributes);
}
