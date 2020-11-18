package com.cmclinnovations.prime.species.model.converter.species;

import org.xml.sax.Attributes;

public class ChemicalCompositionConverter extends PrimeSpeciesConverter implements IChemicalCompositionConverter {
	public void parse(String qName, Attributes attributes) {
		iChemicalCompositionParser.parse(qName, attributes);
		iAtomParser.parse(qName, attributes);
		iComponentParser.parse(qName, attributes);
		iCoalParser.parse(qName, attributes);
		iSpeciesLinkParser.parse(qName, attributes);
		iAmountParser.parse(qName, attributes);
		iUncertaintyParser.parse(qName, attributes);
	}
}
