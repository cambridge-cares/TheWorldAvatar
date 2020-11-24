package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;

public class AdditionalDataItemConverter extends PrimeConverter implements IAdditionalDataItemConverter {
	public void parse(String qName, Attributes attributes) {
		iAdditionalDataItemParser.parse(qName, attributes);
	}
}
