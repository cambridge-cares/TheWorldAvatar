package com.cmclinnovations.ontochemexp.model.parser.prime.apparatus;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class ApparatusPropertyParser extends PrimeConverter implements IApparatusPropertyParser {
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the apparatus tag
		parseProperty(qName, attributes);
		parsePropertyName(qName, attributes);
		parsePropertyID(qName, attributes);
		parsePropertyLabel(qName, attributes);
		parsePropertyUnits(qName, attributes);
		parsePropertyDescription(qName, attributes);
		parsePropertySourceType(qName, attributes);
		parsePropertyReference(qName, attributes);
		parsePropertyBound(qName, attributes);
		parsePropertyKind(qName, attributes);
		parsePropertyMethod(qName, attributes);
	}

	/**
	 * Checks the appearance of the apparatus tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseProperty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			apparatusPropertyParseStatus.setProperty(true);
		}
	}
	
	private void parsePropertyName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String name = attributes.getValue(primeVocabulary.getAttribName());
			if (name != null) {
				apparatusProperty.setPropertyName(name);
				apparatusPropertyParseStatus.setName(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				apparatusProperty.setPropertyId(id);
				apparatusPropertyParseStatus.setID(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyLabel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String label = attributes.getValue(primeVocabulary.getAttribLabel());
			if (label != null) {
				apparatusProperty.setPropertyLabel(label);
				apparatusPropertyParseStatus.setLabel(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String units = attributes.getValue(primeVocabulary.getAttribUnits());
			if (units != null) {
				apparatusProperty.setPropertyUnits(units);
				apparatusPropertyParseStatus.setUnits(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyDescription(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String description = attributes.getValue(primeVocabulary.getAttribDescription());
			if (description != null) {
				apparatusProperty.setPropertyDescription(description);
				apparatusPropertyParseStatus.setDescription(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}
	
	private void parsePropertySourceType(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String sourceType = attributes.getValue(primeVocabulary.getAttribSourceType());
			if (sourceType != null) {
				apparatusProperty.setSourceType(sourceType);
				apparatusPropertyParseStatus.setSourceType(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}
	
	private void parsePropertyReference(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String reference = attributes.getValue(primeVocabulary.getAttribReference());
			if (reference != null) {
				apparatusProperty.setReference(reference);
				apparatusPropertyParseStatus.setReference(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}
	
	private void parsePropertyBound(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String bound = attributes.getValue(primeVocabulary.getAttribBound());
			if (bound != null) {
				apparatusProperty.setBound(bound);
				apparatusPropertyParseStatus.setBound(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}
	
	private void parsePropertyKind(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String kind = attributes.getValue(primeVocabulary.getAttribKind());
			if (kind != null) {
				apparatusProperty.setKind(kind);;
				apparatusPropertyParseStatus.setKind(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}
	
	private void parsePropertyMethod(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inApparatus) {
			String method = attributes.getValue(primeVocabulary.getAttribMethod());
			if (method != null) {
				apparatusProperty.setMethod(method);
				apparatusPropertyParseStatus.setMethod(true);
				apparatusPropertyParseStatus.setProperty(true);
			}
		}
	}
}
