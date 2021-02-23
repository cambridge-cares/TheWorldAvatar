package com.cmclinnovations.prime.species.model.parser.chemicalspecies;

import org.xml.sax.Attributes;

import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class AdditionalDataItemParser extends PrimeSpeciesConverter implements IAdditionalDataItemParser {
	public void parse(String qName, Attributes attributes) {
		parseElement(qName, attributes);
		parseAttributes(qName, attributes);
	}

	private void parseElement(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAdditionalDataItem())) {
			additionalDataItemParseStatus.setAdditionalDataItem(true);
		}
	}

	private void parseAttributes(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemAdditionalDataItem())) {
			String itemType = attributes.getValue(primeSpeciesVocabulary.getAttribItemType());
			if (itemType != null) {
				additionalDataItem.setItemType(itemType);
				additionalDataItemParseStatus.setItemType(true);
				additionalDataItemParseStatus.setAdditionalDataItem(true);
			}

			String description = attributes.getValue(primeSpeciesVocabulary.getAttribDescription());
			if (description != null) {
				additionalDataItem.setDescription(description);
				additionalDataItemParseStatus.setDescription(true);
				additionalDataItemParseStatus.setAdditionalDataItem(true);
			}

			String MIME = attributes.getValue(primeSpeciesVocabulary.getAttribMIME());
			if (MIME != null) {
				additionalDataItem.setMIME(MIME);
				additionalDataItemParseStatus.setMIME(true);
				additionalDataItemParseStatus.setAdditionalDataItem(true);
			}
		}
	}
}
