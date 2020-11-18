package com.cmclinnovations.ontochemexp.model.parser.prime.common_properties;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class CommonPropertiesPropertyComponentUncertaintyParser extends PrimeConverter implements ICommonPropertiesPropertyComponentUncertaintyParser {
	public void parse(String qName, Attributes attributes) {
		// Element Uncertainty
		parseCommonPropertiesPropertyComponentUncertainty(qName, attributes);
		parseCommonPropertiesPropertyComponentUncertaintyBound(qName, attributes);
		parseCommonPropertiesPropertyComponentUncertaintyKind(qName, attributes);
		parseCommonPropertiesPropertyComponentUncertaintyTransformation(qName, attributes);
		parseCommonPropertiesPropertyComponentUncertaintyType(qName, attributes);
	}
	
	private void parseCommonPropertiesPropertyComponentUncertainty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && inCommonPropertiesPropertyComponent) {
			commonPropertiesPropertyComponentUncertaintyParseStatus.setUncertainty(true);
		}
	}

	private void parseCommonPropertiesPropertyComponentUncertaintyBound(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && inCommonPropertiesPropertyComponent) {
			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				commonPropertiesPropertyComponentUncertainty.setUncertaintyBound(bound);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setBound(true);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseCommonPropertiesPropertyComponentUncertaintyKind(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && inCommonPropertiesPropertyComponent) {
			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				commonPropertiesPropertyComponentUncertainty.setUncertaintyKind(kind);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setKind(true);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseCommonPropertiesPropertyComponentUncertaintyTransformation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && inCommonPropertiesPropertyComponent) {
			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				commonPropertiesPropertyComponentUncertainty.setUncertaintyTransformation(transformation);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setTransformation(true);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseCommonPropertiesPropertyComponentUncertaintyType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inCommonProperties && inCommonPropertiesPropertyComponent) {
			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				commonPropertiesPropertyComponentUncertainty.setUncertaintyType(type);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setType(true);
				commonPropertiesPropertyComponentUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
}
