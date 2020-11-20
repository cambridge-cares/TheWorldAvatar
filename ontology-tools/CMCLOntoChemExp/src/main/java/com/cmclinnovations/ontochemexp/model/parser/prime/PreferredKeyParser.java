package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class PreferredKeyParser extends PrimeConverter implements IPreferredKeyParser {
	public void parse(String qName, Attributes attributes) {
		parsePreferredKey(qName, attributes);
	}
	
	private void parsePreferredKey(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemPreferredKey())) {
			preferredKeyParseStatus.setPreferredKey(true);
		}
	}
}
