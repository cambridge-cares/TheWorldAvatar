package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;
/**
 * An interface to the class that implements the phase parser.
 * 
 * @author msff2
 *
 */
public interface IPhaseParser {
	public void parse(String qName, Attributes attributes);
}
