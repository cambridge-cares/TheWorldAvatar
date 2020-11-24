package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class AmountParser extends PrimeSpeciesConverter implements IAmountParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAmount())) {
			amountParseStatus.setAmount(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAmount())) {
			String units = attributes.getValue(primeSpeciesVocabulary.getAttribUnits());
			if (units != null) {
				amount.setUnits(units);
				amountParseStatus.setUnits(true);
				amountParseStatus.setAmount(true);
			}
		}
	}
}
