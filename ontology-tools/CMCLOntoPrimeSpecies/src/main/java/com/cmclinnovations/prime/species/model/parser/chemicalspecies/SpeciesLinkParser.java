package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class SpeciesLinkParser extends PrimeSpeciesConverter implements ISpeciesLinkParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemSpeciesLink())) {
			speciesLinkParseStatus.setSpeciesLink(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemSpeciesLink())) {
			String preferredKey = attributes.getValue(primeSpeciesVocabulary.getAttribPreferredKey());
			if (preferredKey != null) {
				speciesLink.setPreferredKey(preferredKey);
				speciesLinkParseStatus.setPreferredKey(true);
				speciesLinkParseStatus.setSpeciesLink(true);
			}

			String primeID = attributes.getValue(primeSpeciesVocabulary.getAttribPrimeID());
			if (primeID != null) {
				speciesLink.setPrimeID(primeID);
				speciesLinkParseStatus.setPrimeID(true);
				speciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
}
