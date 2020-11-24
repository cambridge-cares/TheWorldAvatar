package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class BibliographyLinkParser extends PrimeSpeciesConverter implements IBibliographyLinkParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemBibliographyLink())) {
			bibliographyLinkParseStatus.setBibliographyLink(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemBibliographyLink())) {
			String preferredKey = attributes.getValue(primeSpeciesVocabulary.getAttribPreferredKey());
			if (preferredKey != null) {
				bibliographyLink.setPreferredKey(preferredKey);
				bibliographyLinkParseStatus.setPreferredKey(true);
				bibliographyLinkParseStatus.setBibliographyLink(true);
			}

			String primeID = attributes.getValue(primeSpeciesVocabulary.getAttribPrimeID());
			if (primeID != null) {
				bibliographyLink.setPrimeID(primeID);
				bibliographyLinkParseStatus.setPrimeID(true);
				bibliographyLinkParseStatus.setBibliographyLink(true);
			}
		}
	}
}
