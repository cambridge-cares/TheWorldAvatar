package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

/**
 * An interface to the class that implements an dataGroup parser.
 * @author Songyi Deng (sd626@cam.ac.uk)
 *
 */

public interface IDataGroupParser {
	public void parse(String qName, Attributes attributes);
}
