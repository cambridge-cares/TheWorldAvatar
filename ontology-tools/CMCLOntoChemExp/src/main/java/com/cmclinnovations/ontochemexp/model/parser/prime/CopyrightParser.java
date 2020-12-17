package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.CopyrightParseStatus;


public class CopyrightParser extends PrimeConverter implements ICopyrightParser {

	/**
	 * Parses PrIMe Copyright data.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the Copyright tag
		parseCopyright(qName, attributes);
	}

	/**
	 * Checks the appearance of the Copyright tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseCopyright(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemCopyright())) {
			copyrightParseStatus.setCopyright(true);
		}
	}
}
