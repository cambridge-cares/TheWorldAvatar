package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class ChemicalIdentifierParser extends PrimeSpeciesConverter implements IChemicalIdentifierParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemChemicalIdentifier())) {
			chemicalIdentifierParseStatus.setChemicalIdentifier(true);
		}
	}
}
