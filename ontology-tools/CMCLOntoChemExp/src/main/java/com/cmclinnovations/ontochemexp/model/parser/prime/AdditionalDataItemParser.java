package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class AdditionalDataItemParser extends PrimeConverter implements IAdditionalDataItemParser {
	public void parse(String qName, Attributes attributes) {
		parseAdditionalDataItem(qName, attributes);
		parseMIME(qName, attributes);
		parseItemType(qName, attributes);
		parseDescription(qName, attributes);
	}
	
	private void parseAdditionalDataItem(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAdditionalDataItem())) {
			//System.out.println("AdditionalDataItem parsed.");
			additionalDataItemParseStatus.setAdditionalDataItem(true);
		}
	}
	
	private void parseMIME(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAdditionalDataItem())) {
			String MIME = attributes.getValue(primeVocabulary.getAttribMIME());
			if (MIME != null) {
				additionalDataItem.setMIME(MIME);
				additionalDataItemParseStatus.setMIME(true);
				additionalDataItemParseStatus.setAdditionalDataItem(true);
			}
		}
	}
	
	private void parseItemType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAdditionalDataItem())) {
			String itemType = attributes.getValue(primeVocabulary.getAttribItemType());
			if (itemType != null) {
				additionalDataItem.setItemType(itemType);
				additionalDataItemParseStatus.setItemType(true);
				additionalDataItemParseStatus.setAdditionalDataItem(true);
			}
		}
	}
	
	private void parseDescription(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAdditionalDataItem())) {
			String description = attributes.getValue(primeVocabulary.getAttribDescription());
			if (description != null) {
				additionalDataItem.setDescription(description);
				additionalDataItemParseStatus.setDescription(true);
				additionalDataItemParseStatus.setAdditionalDataItem(true);
			}
		}
	}
}
