package com.cmclinnovations.ontochemexp.model.parser.prime.apparatus;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class ApparatusPropertyValueParser extends PrimeConverter implements IApparatusPropertyValueParser {
	public void parse(String qName, Attributes attributes) {
		parseApparatusPropertyValue(qName, attributes);
	}
	
	private void parseApparatusPropertyValue(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemValue()) && inApparatus) {
			apparatusPropertyValueParseStatus.setValue(true);
		}
	}
}
