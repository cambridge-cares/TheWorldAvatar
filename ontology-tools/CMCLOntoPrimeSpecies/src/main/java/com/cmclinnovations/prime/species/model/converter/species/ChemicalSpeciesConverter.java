package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class ChemicalSpeciesConverter extends PrimeSpeciesConverter implements IChemicalSpeciesConverter {
	public void parse(String qName, Attributes attributes) {
		iChemicalSpeciesParser.parse(qName, attributes);
	}
}
