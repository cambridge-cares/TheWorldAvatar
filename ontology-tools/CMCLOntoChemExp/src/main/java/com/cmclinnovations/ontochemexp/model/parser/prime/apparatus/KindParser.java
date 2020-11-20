package com.cmclinnovations.ontochemexp.model.parser.prime.apparatus;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class KindParser extends PrimeConverter implements IKindParser {
	public void parse(String qName, Attributes attributes) {
		parseKind(qName, attributes);
	}

	private void parseKind(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemKind()) && inApparatus) {
			kindParseStatus.setKind(true);
		}
	}
}
