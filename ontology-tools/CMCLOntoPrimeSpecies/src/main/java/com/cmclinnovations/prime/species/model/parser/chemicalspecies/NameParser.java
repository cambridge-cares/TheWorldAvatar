package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class NameParser extends PrimeSpeciesConverter implements INameParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemName())) {
			nameParseStatus.setName(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemName())) {
			String source = attributes.getValue(primeSpeciesVocabulary.getAttribSource());
			if (source != null) {
				name.setSource(source);
				nameParseStatus.setSource(true);
				nameParseStatus.setName(true);
			}

			String type = attributes.getValue(primeSpeciesVocabulary.getAttribType());
			if (type != null) {
				name.setType(type);
				nameParseStatus.setType(true);
				nameParseStatus.setName(true);
			}

			String descriptor = attributes.getValue(primeSpeciesVocabulary.getAttribDescriptor());
			if (descriptor != null) {
				name.setDescriptor(descriptor);
				nameParseStatus.setDescriptor(true);
				nameParseStatus.setName(true);
			}
		}
	}
}
