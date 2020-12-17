package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyComponentParser extends PrimeConverter
		implements ICommonPropertiesPropertyComponentParser {
	public void parse(String qName, Attributes attributes) {
		// Element Component
		parseCommonPropertiesPropertyComponent(qName, attributes);
	}
	
	private void parseCommonPropertiesPropertyComponent(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemComponent()) && inCommonProperties) {
			commonPropertiesPropertyComponentParseStatus.setComponent(true);
			inCommonPropertiesPropertyComponent = true;
		}
	}
}
