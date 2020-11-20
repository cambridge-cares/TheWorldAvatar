package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.BibliographyLinkParseStatus;

public class BibliographyLinkParser extends PrimeConverter implements IBibliographyLinkParser {

	/**
	 * Parses PrIMe BibliographyLink data.
	 * 
	 * @param qName
	 * @param attributes
	 */
	public void parse(String qName, Attributes attributes) {
		parseBibliographyLink(qName, attributes);
		parseBibliPreferredKey(qName, attributes);
		parseBibliPrimeID(qName, attributes);
	}

	/**
	 * Checks the appearance of the BibliographyLink tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseBibliographyLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemBibliographyLink())) {
			bibliographyLinkParseStatus.setBibliographyLink(true);
		}
	}

	private void parseBibliPreferredKey(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemBibliographyLink())) {
			String bibliPreferredKey = attributes.getValue(primeVocabulary.getAttribPreferredKey());
			if (bibliPreferredKey != null) {
				bibliographyLink.setPreferredKey(bibliPreferredKey);
				bibliographyLinkParseStatus.setPreferredKey(true);
				bibliographyLinkParseStatus.setBibliographyLink(true);
			}
		}
	}

	private void parseBibliPrimeID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemBibliographyLink())) {
			String bibliPrimeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (bibliPrimeID != null) {
				bibliographyLink.setPrimeID(bibliPrimeID);
				bibliographyLinkParseStatus.setPrimeID(true);
				bibliographyLinkParseStatus.setBibliographyLink(true);
			}
		}
	}
}
