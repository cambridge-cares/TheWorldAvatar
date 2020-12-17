package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class ChemicalIdentifierConverter extends PrimeSpeciesConverter implements IChemicalIdentifierConverter {
	public void parse(String qName, Attributes attributes) {
		iChemicalIdentifierParser.parse(qName, attributes);
		iNameParser.parse(qName, attributes);
		
	}
}
