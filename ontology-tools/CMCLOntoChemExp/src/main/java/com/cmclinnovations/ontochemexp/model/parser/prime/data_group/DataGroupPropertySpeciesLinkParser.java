package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertySpeciesLinkParser extends PrimeConverter implements IDataGroupPropertySpeciesLinkParser {
	public void parse(String qName, Attributes attributes) {
		// Element SpeciesLink
		parseDataGroupPropertySpeciesLink(qName, attributes);
		parseDataGroupPropertySpeciesLinkPreferredKey(qName, attributes);
		parseDataGroupPropertySpeciesLinkPrimeID(qName, attributes);
		parseDataGroupPropertySpeciesLinkCAS(qName, attributes);
		parseDataGroupPropertySpeciesLinkInChI(qName, attributes);
		parseDataGroupPropertySpeciesLinkSMILES(qName, attributes);
		parseDataGroupPropertySpeciesLinkChemName(qName, attributes);
	}
	
	private void parseDataGroupPropertySpeciesLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
		}
	}
	
	private void parseDataGroupPropertySpeciesLinkPreferredKey(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			String preferredKey = attributes.getValue(primeVocabulary.getAttribPreferredKey());
			if (preferredKey != null && !preferredKey.isEmpty()) {
				dataGroupPropertySpeciesLink.setSpeciesLinkPreferredKey(preferredKey);
				dataGroupPropertySpeciesLinkParseStatus.setPreferredKey(true);
				dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertySpeciesLinkPrimeID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			String primeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (primeID != null) {
				dataGroupPropertySpeciesLink.setSpeciesLinkPrimeID(primeID);
				dataGroupPropertySpeciesLinkParseStatus.setPrimeID(true);
				dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertySpeciesLinkCAS(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			String cas = attributes.getValue(primeVocabulary.getAttribCAS());
			if (cas != null) {
				dataGroupPropertySpeciesLink.setCas(cas);;
				dataGroupPropertySpeciesLinkParseStatus.setCas(true);
				dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertySpeciesLinkInChI(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			String inchi = attributes.getValue(primeVocabulary.getAttribInChI());
			if (inchi != null) {
				dataGroupPropertySpeciesLink.setInchi(inchi);
				dataGroupPropertySpeciesLinkParseStatus.setInchi(true);
				dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertySpeciesLinkSMILES(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			String smiles = attributes.getValue(primeVocabulary.getAttribSMILES());
			if (smiles != null) {
				dataGroupPropertySpeciesLink.setSmiles(smiles);
				dataGroupPropertySpeciesLinkParseStatus.setSmiles(true);
				dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertySpeciesLinkChemName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && !dataGroupPropertyComponentParseStatus.isComponent()) {
			String chemName = attributes.getValue(primeVocabulary.getAttribChemName());
			if (chemName != null) {
				dataGroupPropertySpeciesLink.setChemName(chemName);
				dataGroupPropertySpeciesLinkParseStatus.setChemName(true);
				dataGroupPropertySpeciesLinkParseStatus.setSpeciesLink(true);
			}
		}
	}
}
