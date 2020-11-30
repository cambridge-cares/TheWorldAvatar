package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

/**
 * This class parses a PrIMe experiment and stores its data and metadata in the 
 * commonProperties data structure.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class CommonPropertiesParser extends PrimeConverter implements ICommonPropertiesParser {

	/**
	 * Parses PrIMe commonProperties data.
	 * 
	 * @param qName
	 * @param attributes
	 */
	
	public void parse(String qName, Attributes attributes) {
		parseCommonProperties(qName, attributes);

	}

	/**
	 * Checks the appearance of the commonProperties tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCommonProperties(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemCommonProperties())) {
			commonPropertiesParseStatus.setCommonProperties(true);
			inCommonProperties = true;
		}
	}
}
