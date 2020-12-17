package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyValueParser extends PrimeConverter
		implements ICommonPropertiesPropertyValueParser {
	public void parse(String qName, Attributes attributes) {
		// Element Value
		parseCommonPropertiesPropertyValue(qName, attributes);
	}
	
	private void parseCommonPropertiesPropertyValue(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemValue()) && inCommonProperties) {
			commonPropertiesPropertyValueParseStatus.setValue(true);
		}
	}
}
