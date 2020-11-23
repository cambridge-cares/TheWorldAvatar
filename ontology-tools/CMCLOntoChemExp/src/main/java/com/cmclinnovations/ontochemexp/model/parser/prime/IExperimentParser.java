package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;
/**
 * An interface to the class that implements a PrIMe experiment parser.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IExperimentParser {
	public void parse(String qName, Attributes attributes);
}
