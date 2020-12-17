package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyUncertaintyParser extends PrimeConverter
		implements ICommonPropertiesPropertyUncertaintyParser {
	public void parse(String qName, Attributes attributes) {
		// Element Uncertainty
		parseCommonPropertiesPropertyUncertainty(qName, attributes);
		parseCommonPropertiesPropertyUncertaintyBound(qName, attributes);
		parseCommonPropertiesPropertyUncertaintyKind(qName, attributes);
		parseCommonPropertiesPropertyUncertaintyTransformation(qName, attributes);
		parseCommonPropertiesPropertyUncertaintyType(qName, attributes);
	}

	private void parseCommonPropertiesPropertyUncertainty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && !inCommonPropertiesPropertyComponent) {
			commonPropertiesPropertyUncertaintyParseStatus.setUncertainty(true);
		}
	}

	private void parseCommonPropertiesPropertyUncertaintyBound(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && !inCommonPropertiesPropertyComponent) {
			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				commonPropertiesPropertyUncertainty.setUncertaintyBound(bound);
				commonPropertiesPropertyUncertaintyParseStatus.setBound(true);
				commonPropertiesPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseCommonPropertiesPropertyUncertaintyKind(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && !inCommonPropertiesPropertyComponent) {
			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				commonPropertiesPropertyUncertainty.setUncertaintyKind(kind);
				commonPropertiesPropertyUncertaintyParseStatus.setKind(true);
				commonPropertiesPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseCommonPropertiesPropertyUncertaintyTransformation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && !inCommonPropertiesPropertyComponent) {
			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				commonPropertiesPropertyUncertainty.setUncertaintyTransformation(transformation);
				commonPropertiesPropertyUncertaintyParseStatus.setTransformation(true);
				commonPropertiesPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseCommonPropertiesPropertyUncertaintyType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && !inCommonPropertiesPropertyComponent) {
			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				commonPropertiesPropertyUncertainty.setUncertaintyType(type);
				commonPropertiesPropertyUncertaintyParseStatus.setType(true);
				commonPropertiesPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
}
