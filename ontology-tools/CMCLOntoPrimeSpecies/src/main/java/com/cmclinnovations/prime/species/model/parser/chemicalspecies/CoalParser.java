package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class CoalParser extends PrimeSpeciesConverter implements ICoalParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemCoal())) {
			coalParseStatus.setCoal(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemCoal())) {
			String specifiedBy = attributes.getValue(primeSpeciesVocabulary.getAttribSpecifiedBy());
			if (specifiedBy != null) {
				coal.setSpecifiedBy(specifiedBy);
				coalParseStatus.setSpecifiedBy(true);
				coalParseStatus.setCoal(true);
			}
		}
	}
}
