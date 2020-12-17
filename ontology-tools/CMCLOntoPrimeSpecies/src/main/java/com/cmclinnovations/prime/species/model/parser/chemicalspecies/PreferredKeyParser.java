package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class PreferredKeyParser extends PrimeSpeciesConverter implements IPreferredKeyParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemPreferredKey())) {
			preferredKeyParseStatus.setPreferredKey(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemPreferredKey())) {
			String type = attributes.getValue(primeSpeciesVocabulary.getAttribType());
			if (type != null) {
				preferredKey.setType(type);
				preferredKeyParseStatus.setType(true);
				preferredKeyParseStatus.setPreferredKey(true);
			}

			String group = attributes.getValue(primeSpeciesVocabulary.getAttribGroup());
			if (group != null) {
				preferredKey.setGroup(group);
				preferredKeyParseStatus.setGroup(true);
				preferredKeyParseStatus.setPreferredKey(true);
			}
		}
	}
}
