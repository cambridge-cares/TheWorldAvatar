package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertyDerivedPropertyFeatureParser extends PrimeConverter
		implements IDataGroupPropertyDerivedPropertyFeatureParser {
	public void parse(String qName, Attributes attributes) {
		// Itself
		parseDataGroupPropertyDerivedPropertyFeature(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeaturePrimeID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureType(qName, attributes);
	}

	private void parseDataGroupPropertyDerivedPropertyFeature(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemFeature()) && inDataGroup) {
			dataGroupPropertyDerivedPropertyFeatureParseStatus.setFeature(true);
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemFeature()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupPropertyDerivedPropertyFeature.setFeatureId(id);
				dataGroupPropertyDerivedPropertyFeatureParseStatus.setID(true);
				dataGroupPropertyDerivedPropertyFeatureParseStatus.setFeature(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeaturePrimeID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemFeature()) && inDataGroup) {
			String primeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (primeID != null) {
				dataGroupPropertyDerivedPropertyFeature.setFeaturePrimeID(primeID);
				dataGroupPropertyDerivedPropertyFeatureParseStatus.setPrimeID(true);
				dataGroupPropertyDerivedPropertyFeatureParseStatus.setFeature(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemFeature()) && inDataGroup) {
			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				dataGroupPropertyDerivedPropertyFeature.setFeatureType(type);
				dataGroupPropertyDerivedPropertyFeatureParseStatus.setType(true);
				dataGroupPropertyDerivedPropertyFeatureParseStatus.setFeature(true);
			}
		}
	}
}
