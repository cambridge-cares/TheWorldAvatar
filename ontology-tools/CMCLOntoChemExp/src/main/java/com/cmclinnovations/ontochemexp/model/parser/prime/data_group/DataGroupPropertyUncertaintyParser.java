package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertyUncertaintyParser extends PrimeConverter implements IDataGroupPropertyUncertaintyParser {
	public void parse(String qName, Attributes attributes) {
		// Element Uncertainty
		parseDataGroupPropertyUncertainty(qName, attributes);
		parseDataGroupPropertyUncertaintyBound(qName, attributes);
		parseDataGroupPropertyUncertaintyKind(qName, attributes);
		parseDataGroupPropertyUncertaintyTransformation(qName, attributes);
		parseDataGroupPropertyUncertaintyType(qName, attributes);
	}
	
	private void parseDataGroupPropertyUncertainty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inDataGroup
				&& !(x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			dataGroupPropertyUncertaintyParseStatus.setUncertainty(true);
		}
	}
	
	private void parseDataGroupPropertyUncertaintyBound(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inDataGroup
				&& !(x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				dataGroupPropertyUncertainty.setUncertaintyBound(bound);
				dataGroupPropertyUncertaintyParseStatus.setBound(true);
				dataGroupPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
	
	private void parseDataGroupPropertyUncertaintyKind(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inDataGroup
				&& !(x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				dataGroupPropertyUncertainty.setUncertaintyKind(kind);
				dataGroupPropertyUncertaintyParseStatus.setKind(true);
				dataGroupPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
	
	private void parseDataGroupPropertyUncertaintyTransformation(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inDataGroup
				&& !(x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				dataGroupPropertyUncertainty.setUncertaintyTransformation(transformation);
				dataGroupPropertyUncertaintyParseStatus.setTransformation(true);
				dataGroupPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
	
	private void parseDataGroupPropertyUncertaintyType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && inDataGroup
				&& !(x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				dataGroupPropertyUncertainty.setUncertaintyType(type);
				dataGroupPropertyUncertaintyParseStatus.setType(true);
				dataGroupPropertyUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
}
