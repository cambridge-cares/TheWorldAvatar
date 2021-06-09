package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyComponentSpeciesLinkParser extends PrimeConverter implements ICommonPropertiesPropertyComponentSpeciesLinkParser {
	public void parse(String qName, Attributes attributes) {
		parseCommonPropertiesPropertyComponentSpeciesLink(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkPreferredKey(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkPrimeID(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkCAS(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkInChI(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkSMILES(qName, attributes);
		parseCommonPropertiesPropertyComponentSpeciesLinkChemName(qName, attributes);
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
	
	private void parseCommonPropertiesPropertyComponentSpeciesLinkCAS(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			String cas = attributes.getValue(primeVocabulary.getAttribCAS());
			if (cas != null) {
				commonPropertiesPropertyComponentSpeciesLink.setCas(cas);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setCas(true);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseCommonPropertiesPropertyComponentSpeciesLinkInChI(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			String inchi = attributes.getValue(primeVocabulary.getAttribInChI());
			if (inchi != null) {
				commonPropertiesPropertyComponentSpeciesLink.setInchi(inchi);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setInchi(true);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseCommonPropertiesPropertyComponentSpeciesLinkSMILES(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			String smiles = attributes.getValue(primeVocabulary.getAttribSMILES());
			if (smiles != null) {
				commonPropertiesPropertyComponentSpeciesLink.setSmiles(smiles);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSmiles(true);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseCommonPropertiesPropertyComponentSpeciesLinkChemName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inCommonProperties) {
			String chemName = attributes.getValue(primeVocabulary.getAttribChemName());
			if (chemName != null) {
				commonPropertiesPropertyComponentSpeciesLink.setChemName(chemName);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setChemName(true);
				commonPropertiesPropertyComponentSpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
}
