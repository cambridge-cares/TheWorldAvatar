package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;

public class PreferredKeyConverter extends PrimeConverter implements IPreferredKeyConverter{
	public void parse(String qName, Attributes attributes) {
		iPreferredKeyParser.parse(qName, attributes);
	}
}
