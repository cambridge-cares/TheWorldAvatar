package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class AtomParser extends PrimeSpeciesConverter implements IAtomParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAtom())) {
			atomParseStatus.setAtom(true);
		}
	}
	
	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAtom())) {
			String symbol = attributes.getValue(primeSpeciesVocabulary.getAttribSymbol());
			if (symbol != null) {
				atom.setSymbol(symbol);
				atomParseStatus.setSymbol(true);
				atomParseStatus.setAtom(true);
			}

			String isotope = attributes.getValue(primeSpeciesVocabulary.getAttribIsotope());
			if (isotope != null) {
				atom.setIsotope(isotope);
				atomParseStatus.setIsotope(true);
				atomParseStatus.setAtom(true);
			}
		}
	}
}
