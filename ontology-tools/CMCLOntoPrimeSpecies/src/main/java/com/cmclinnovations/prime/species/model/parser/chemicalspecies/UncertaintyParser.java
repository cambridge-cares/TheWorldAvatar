package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class UncertaintyParser extends PrimeSpeciesConverter implements IUncertaintyParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemUncertainty())) {
			uncertaintyParseStatus.setUncertainty(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemUncertainty())) {
			String bound = attributes.getValue(primeSpeciesVocabulary.getAttribBound());
			if (bound != null) {
				uncertainty.setBound(bound);
				uncertaintyParseStatus.setBound(true);
				uncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeSpeciesVocabulary.getAttribKind());
			if (kind != null) {
				uncertainty.setKind(kind);
				uncertaintyParseStatus.setKind(true);
				uncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeSpeciesVocabulary.getAttribTransformation());
			if (transformation != null) {
				uncertainty.setTransformation(transformation);
				uncertaintyParseStatus.setTransformation(true);
				uncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
}
