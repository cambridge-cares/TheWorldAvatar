package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertyDerivedPropertyParser extends PrimeConverter
		implements IDataGroupPropertyDerivedPropertyParser {
	public void parse(String qName, Attributes attributes) {
		// Element DerivedProperty
		parseDataGroupPropertyDerivedProperty(qName, attributes);
		parseDataGroupPropertyDerivedPropertyName(qName, attributes);
		parseDataGroupPropertyDerivedPropertyID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyLabel(qName, attributes);
		parseDataGroupPropertyDerivedPropertyUnits(qName, attributes);
		parseDataGroupPropertyDerivedPropertyDescription(qName, attributes);
	}
	
	private void parseDataGroupPropertyDerivedProperty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDerivedProperty()) && inDataGroup) {
			dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(true);
		}
	}

	private void parseDataGroupPropertyDerivedPropertyName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDerivedProperty()) && inDataGroup) {
			String name = attributes.getValue(primeVocabulary.getAttribName());
			if (name != null) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyName(name);
				dataGroupPropertyDerivedPropertyParseStatus.setName(true);
				dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDerivedProperty()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyId(id);
				dataGroupPropertyDerivedPropertyParseStatus.setID(true);
				dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyLabel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDerivedProperty()) && inDataGroup) {
			String label = attributes.getValue(primeVocabulary.getAttribLabel());
			if (label != null) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyLabel(label);
				dataGroupPropertyDerivedPropertyParseStatus.setLabel(true);
				dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDerivedProperty()) && inDataGroup) {
			String units = attributes.getValue(primeVocabulary.getAttribUnits());
			if (units != null) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyUnits(units);
				dataGroupPropertyDerivedPropertyParseStatus.setUnits(true);
				dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyDescription(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDerivedProperty()) && inDataGroup) {
			String description = attributes.getValue(primeVocabulary.getAttribDescription());
			if (description != null) {
				dataGroupPropertyDerivedProperty.setDerivedPropertyDescription(description);
				dataGroupPropertyDerivedPropertyParseStatus.setDescription(true);
				dataGroupPropertyDerivedPropertyParseStatus.setDerivedProperty(true);
			}
		}
	}
}
