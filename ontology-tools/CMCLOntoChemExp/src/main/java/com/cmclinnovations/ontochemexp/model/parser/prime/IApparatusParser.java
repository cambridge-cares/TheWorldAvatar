package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;
/**
 * An interface to the class that implements an apparatus parser.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IApparatusParser {
	public void parse(String qName, Attributes attributes);
}
