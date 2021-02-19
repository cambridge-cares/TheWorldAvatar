package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class ChemicalSpeciesParser extends PrimeSpeciesConverter implements IChemicalSpeciesParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}
	
	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemChemicalSpecies())) {
			chemicalSpeciesParseStatus.setChemicalSpecies(true);
		}
	}
	
	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemChemicalSpecies())) {
			String primeID = attributes.getValue(primeSpeciesVocabulary.getAttribPrimeID());
			if (primeID != null) {
				chemicalSpecies.setPrimeID(primeID);
				chemicalSpeciesParseStatus.setPrimeID(true);
				chemicalSpeciesParseStatus.setChemicalSpecies(true);
			}
			
			String xmlns = attributes.getValue(primeSpeciesVocabulary.getAttribXmlns());
			if (xmlns != null) {
				chemicalSpecies.setXmlns(xmlns);
				chemicalSpeciesParseStatus.setXmlns(true);
				chemicalSpeciesParseStatus.setChemicalSpecies(true);
			}
			
			String xmlnsXsi = attributes.getValue(primeSpeciesVocabulary.getAttribXmlnsXsi());
			if (xmlnsXsi != null) {
				chemicalSpecies.setXmlnsXsi(xmlnsXsi);
				chemicalSpeciesParseStatus.setXmlnsXsi(true);
				chemicalSpeciesParseStatus.setChemicalSpecies(true);
			}
			
			String xsiSchemaLocation = attributes.getValue(primeSpeciesVocabulary.getAttribXsiSchemaLocation());
			if(xsiSchemaLocation != null) {
				chemicalSpecies.setXsiSchemaLocation(xsiSchemaLocation);
				chemicalSpeciesParseStatus.setXsiSchemaLocation(true);
				chemicalSpeciesParseStatus.setChemicalSpecies(true);
			}
		}
	}
}
