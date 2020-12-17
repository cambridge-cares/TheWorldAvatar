package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertyValueParser extends PrimeConverter implements IDataGroupPropertyValueParser {
	public void parse(String qName, Attributes attributes) {
		// Element Value
		parseDataGroupPropertyValue(qName, attributes);
	}

	private void parseDataGroupPropertyValue(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemValue()) && inDataGroup) {
			dataGroupPropertyValueParseStatus.setValue(true);
		}
	}
}
