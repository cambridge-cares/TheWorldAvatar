package com.cmclinnovations.ontochemexp.model.parser.prime.apparatus;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class ApparatusPropertyUncertaintyParser extends PrimeConverter implements IApparatusPropertyUncertaintyParser {
	public void parse(String qName, Attributes attributes) {
		// Element Uncertainty
		parseApparatusPropertyUncertainty(qName, attributes);
		parseApparatusPropertyUncertaintyBound(qName, attributes);
		parseApparatusPropertyUncertaintyKind(qName, attributes);
		parseApparatusPropertyUncertaintyTransformation(qName, attributes);
		parseApparatusPropertyUncertaintyType(qName, attributes);
	}

	private void parseApparatusPropertyUncertainty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inApparatus) {
			apparatusPropertyUncertaintyParseStatus.setUncertainty(true);
		}
	}

	private void parseApparatusPropertyUncertaintyBound(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inApparatus) {
			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null && !bound.trim().isEmpty()) {
				apparatusPropertyUncertainty.setUncertaintyBound(bound);
				apparatusPropertyUncertaintyParseStatus.setBound(true);
				apparatusPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseApparatusPropertyUncertaintyKind(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inApparatus) {
			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				apparatusPropertyUncertainty.setUncertaintyKind(kind);
				apparatusPropertyUncertaintyParseStatus.setKind(true);
				apparatusPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseApparatusPropertyUncertaintyTransformation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inApparatus) {
			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				apparatusPropertyUncertainty.setUncertaintyTransformation(transformation);
				apparatusPropertyUncertaintyParseStatus.setTransformation(true);
				apparatusPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}

	private void parseApparatusPropertyUncertaintyType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inApparatus) {
			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				apparatusPropertyUncertainty.setUncertaintyType(type);
				apparatusPropertyUncertaintyParseStatus.setType(true);
				apparatusPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
}
