package com.cmclinnovations.ontochemexp.model.parser.prime.apparatus;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class ModeParser extends PrimeConverter implements IModeParser {
	public void parse(String qName, Attributes attributes) {
		parseMode(qName, attributes);
	}

	private void parseMode(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemMode()) && inApparatus) {
			modeParseStatus.setMode(true);
		}
	}
}
