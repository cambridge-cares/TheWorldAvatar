package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;
/**
 * Implements the method that forwards calls to the Apparatus parser.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ApparatusConverter extends PrimeConverter implements IApparatusConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards calls to
		// the methods that parse apparatus data and metadata.
		iApparatusParser.parse(qName, attributes);
		iKindParser.parse(qName, attributes);
		iModeParser.parse(qName, attributes);
		iApparatusPropertyParser.parse(qName, attributes);
		iApparatusPropertyValueParser.parse(qName, attributes);
		iApparatusPropertyUncertaintyParser.parse(qName, attributes);
	}
}
