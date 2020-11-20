package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

/**
 * This class parses a PrIMe experiment and stores its data and metadata in the 
 * apparatus data structure.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ApparatusParser extends PrimeConverter implements IApparatusParser {

	/**
	 * Parses PrIMe apparatus data.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the apparatus tag
		parseApparatus(qName, attributes);
	}

	/**
	 * Checks the appearance of the apparatus tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseApparatus(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemApparatus())) {
			apparatusParseStatus.setApparatus(true);
			inApparatus = true;
		}
	}
}
