package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;
/**
 * Declares the method whose implementation parses PrIMe apparatus data and metadata. 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IApparatusConverter{
	public void parse(String qName, Attributes attributes);
}
