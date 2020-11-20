package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertyDerivedPropertyFeatureObservableParser extends PrimeConverter
		implements IDataGroupPropertyDerivedPropertyFeatureObservableParser {
	public void parse(String qName, Attributes attributes) {
		// Itself
		parseDataGroupPropertyDerivedPropertyFeatureObservable(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureObservableID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureObservableVariableID(qName, attributes);
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureObservable(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemObservable()) && inDataGroup) {
			dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.setObservable(true);
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureObservableID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemObservable()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupPropertyDerivedPropertyFeatureObservable.setObservableId(id);
				dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.setID(true);
				dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.setObservable(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureObservableVariableID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemObservable()) && inDataGroup) {
			String variableID = attributes.getValue(primeVocabulary.getAttribVariableID());
			if (variableID != null) {
				dataGroupPropertyDerivedPropertyFeatureObservable.setObservableVariableID(variableID);
				dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.setVariableID(true);
				dataGroupPropertyDerivedPropertyFeatureObservableParseStatus.setObservable(true);
			}
		}
	}
}
