package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.hp.hpl.jena.rdf.arp.states.StartStateRDForDescription;

public class DataGroupDataPointXParser extends PrimeConverter implements IDataGroupDataPointXParser {
	public void parse(String qName, Attributes attributes) {
		parseDataPointX(qName, attributes);
		parseDataPointXUncertainty(qName, attributes);
	}

	private void parseDataPointX(String qName, Attributes attributes) {
		if (qName.toLowerCase().startsWith((primeVocabulary.getElemDataPointX())) && inDataGroup) {

			switch (qName.toLowerCase()) {
			case "x1":
				x1ParseStatus.setX(true);
				x1ParseStatus.setParsed(false);
				break;

			case "x2":
				x2ParseStatus.setX(true);
				x2ParseStatus.setParsed(false);
				break;

			case "x3":
				x3ParseStatus.setX(true);
				x3ParseStatus.setParsed(false);
				break;

			case "x4":
				x4ParseStatus.setX(true);
				x4ParseStatus.setParsed(false);
				break;

			case "x5":
				x5ParseStatus.setX(true);
				x5ParseStatus.setParsed(false);
				break;

			case "x6":
				x6ParseStatus.setX(true);
				x6ParseStatus.setParsed(false);
				break;

			case "x7":
				x7ParseStatus.setX(true);
				x7ParseStatus.setParsed(false);
				break;

			case "x8":
				x8ParseStatus.setX(true);
				x8ParseStatus.setParsed(false);
				break;

			case "x9":
				x9ParseStatus.setX(true);
				x9ParseStatus.setParsed(false);
				break;

			case "x10":
				x10ParseStatus.setX(true);
				x10ParseStatus.setParsed(false);
				break;

			case "x11":
				x11ParseStatus.setX(true);
				x11ParseStatus.setParsed(false);
				break;
			}
		}
	}

	private void parseDataPointXUncertainty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x1ParseStatus.isX()) {
			if (!x1ParseStatus.isParsed()) {
				iDataGroupWriter.writeX1ToOwl();
			}

			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x2ParseStatus.isX()) {
			if (!x2ParseStatus.isParsed()) {
				iDataGroupWriter.writeX2ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x3ParseStatus.isX()) {
			if (!x3ParseStatus.isParsed()) {
				iDataGroupWriter.writeX3ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x4ParseStatus.isX()) {
			if (!x4ParseStatus.isParsed()) {
				iDataGroupWriter.writeX4ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x5ParseStatus.isX()) {
			if (!x5ParseStatus.isParsed()) {
				iDataGroupWriter.writeX5ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x6ParseStatus.isX()) {
			if (!x6ParseStatus.isParsed()) {
				iDataGroupWriter.writeX6ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x7ParseStatus.isX()) {
			if (!x7ParseStatus.isParsed()) {
				iDataGroupWriter.writeX7ToOwl();
			}
			

			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x8ParseStatus.isX()) {
			if (!x8ParseStatus.isParsed()) {
				iDataGroupWriter.writeX8ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x9ParseStatus.isX()) {
			if (!x9ParseStatus.isParsed()) {
				iDataGroupWriter.writeX9ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x10ParseStatus.isX()) {
			if (!x10ParseStatus.isParsed()) {
				iDataGroupWriter.writeX10ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) && x11ParseStatus.isX()) {
			if (!x11ParseStatus.isParsed()) {
				iDataGroupWriter.writeX11ToOwl();
			}
			
			xUncertaintyParseStatus.setUncertainty(true);

			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				xUncertainty.setUncertaintyBound(bound);
				xUncertaintyParseStatus.setBound(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				xUncertainty.setUncertaintyKind(kind);
				xUncertaintyParseStatus.setKind(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				xUncertainty.setUncertaintyTransformation(transformation);
				xUncertaintyParseStatus.setTransformation(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}

			String type = attributes.getValue(primeVocabulary.getAttribType());
			if (type != null) {
				xUncertainty.setUncertaintyType(type);
				xUncertaintyParseStatus.setType(true);
				xUncertaintyParseStatus.setUncertainty(true);
			}
		}
	}
}
