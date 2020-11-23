package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyParser extends PrimeConverter implements ICommonPropertiesPropertyParser {
	public void parse(String qName, Attributes attributes) {
		// Itself
		parseProperty(qName, attributes);
		parsePropertyName(qName, attributes);
		parsePropertyID(qName, attributes);
		parsePropertyLabel(qName, attributes);
		parsePropertyUnits(qName, attributes);
		parsePropertyDescription(qName, attributes);
	}

	private void parseProperty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			commonPropertiesPropertyParseStatus.setProperty(true);
		}
	}

	private void parsePropertyName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			String name = attributes.getValue(primeVocabulary.getAttribName());
			if (name != null) {
				commonPropertiesProperty.setPropertyName(name);
				commonPropertiesPropertyParseStatus.setName(true);
				commonPropertiesPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				commonPropertiesProperty.setPropertyId(id);
				commonPropertiesPropertyParseStatus.setID(true);
				commonPropertiesPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyLabel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			String label = attributes.getValue(primeVocabulary.getAttribLabel());
			if (label != null) {
				commonPropertiesProperty.setPropertyLabel(label);
				commonPropertiesPropertyParseStatus.setLabel(true);
				commonPropertiesPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			String units = attributes.getValue(primeVocabulary.getAttribUnits());
			if (units != null) {
				commonPropertiesProperty.setPropertyUnits(units);
				commonPropertiesPropertyParseStatus.setUnits(true);
				commonPropertiesPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyDescription(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			String description = attributes.getValue(primeVocabulary.getAttribDescription());
			if (description != null) {
				commonPropertiesProperty.setPropertyDescription(description);
				commonPropertiesPropertyParseStatus.setDescription(true);
				commonPropertiesPropertyParseStatus.setProperty(true);
			}
		}
	}	
}
