package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyComponentSpeciesLinkParser extends PrimeConverter implements ICommonPropertiesPropertyComponentSpeciesLinkParser {
	public void parse(String qName, Attributes attributes) {
		parseCommonPropertiesPropertyComponentSpeciesLink(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkPreferredKey(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkPrimeID(qName, attributes);
	}
	
	private void parseCommonPropertiesPropertyComponentSpeciesLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
		}
	}
	
	private void parseCommonPropertiesPropertyComponentSpeciesLinkPreferredKey(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			String preferredKey = attributes.getValue(primeVocabulary.getAttribPreferredKey());
			if (preferredKey != null) {
				commonPropertiesPropertyComponentSpeciesLink.setSpeciesLinkPreferredKey(preferredKey);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setPreferredKey(true);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseCommonPropertiesPropertyComponentSpeciesLinkPrimeID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			String primeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (primeID != null) {
				commonPropertiesPropertyComponentSpeciesLink.setSpeciesLinkPrimeID(primeID);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setPrimeID(true);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
}
