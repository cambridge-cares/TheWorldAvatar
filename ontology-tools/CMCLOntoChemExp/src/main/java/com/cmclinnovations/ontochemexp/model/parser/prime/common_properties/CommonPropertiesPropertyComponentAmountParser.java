package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyComponentAmountParser extends PrimeConverter implements ICommonPropertiesPropertyComponentAmountParser {
	public void parse(String qName, Attributes attributes) {
		parseCommonPropertiesPropertyComponentAmount(qName, attributes);
		parseCommonPropertiesPropertyComponentAmountUnits(qName, attributes);
	}
	
	private void parseCommonPropertiesPropertyComponentAmount(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAmount()) && inCommonProperties) {
			commonPropertiesPropertyComponentAmountParseStatus.setAmount(true);
		}
	}
	
	private void parseCommonPropertiesPropertyComponentAmountUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAmount()) && inCommonProperties) {
			String units = attributes.getValue(primeVocabulary.getAttribUnits());
			if (units != null) {
				commonPropertiesPropertyComponentAmount.setAmountUnits(units);
				commonPropertiesPropertyComponentAmountParseStatus.setUnits(true);
				commonPropertiesPropertyComponentAmountParseStatus.setAmount(true);
			}
		}
	}
}
