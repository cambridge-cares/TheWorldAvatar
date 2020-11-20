package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupPropertyDerivedPropertyFeatureIndicatorParser extends PrimeConverter
		implements IDataGroupPropertyDerivedPropertyFeatureIndicatorParser {
	public void parse(String qName, Attributes attributes) {
		// Itself
		parseDataGroupPropertyDerivedPropertyFeatureIndicator(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorTransformation(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorVariableID(qName, attributes);

		// Element PropertyLink
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkPropertyID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkDataGroupID(qName, attributes);

		// Element DataAttributeLink
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkID(qName, attributes);
		parseDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkPrimeID(qName, attributes);
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureIndicator(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemIndicator()) && inDataGroup) {
			dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setIndicator(true);
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemIndicator()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicator.setIndicatorId(id);
				dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setID(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setIndicator(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorTransformation(String qName,
			Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemIndicator()) && inDataGroup) {
			String transformation = attributes.getValue(primeVocabulary.getAttribTransformation());
			if (transformation != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicator.setIndicatorTransformation(transformation);
				dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setTransformation(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setIndicator(true);
			}
		}
	}

	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorVariableID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemIndicator()) && inDataGroup) {
			String variableID = attributes.getValue(primeVocabulary.getAttribVariableID());
			if (variableID != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicator.setIndicatorVariableID(variableID);
				dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setVariableID(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorParseStatus.setIndicator(true);
			}
		}
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemPropertyLink()) && inDataGroup) {
			dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.setPropertyLink(true);
		}
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkPropertyID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemPropertyLink()) && inDataGroup) {
			String propertyID = attributes.getValue(primeVocabulary.getAttribPropertyID());
			if (propertyID != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.setPropertyId(propertyID);
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.setPropertyID(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.setPropertyLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkDataGroupID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemPropertyLink()) && inDataGroup) {
			String dataGroupID = attributes.getValue(primeVocabulary.getAttribDataGroupID());
			if (dataGroupID != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLink.setDataGroupID(dataGroupID);
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.setDataGroupID(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorPropertyLinkParseStatus.setPropertyLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataAttributeLink()) && inDataGroup) {
			dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.setDataAttributeLink(true);
		}
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataAttributeLink()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.setDataAttributeLinkId(id);
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.setID(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.setDataAttributeLink(true);
			}
		}
	}
	
	private void parseDataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkPrimeID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataAttributeLink()) && inDataGroup) {
			String primeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (primeID != null) {
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLink.setDataAttributeLinkPrimeID(primeID);
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.setPrimeID(true);
				dataGroupPropertyDerivedPropertyFeatureIndicatorDataAttributeLinkParseStatus.setDataAttributeLink(true);
			}
		}
	}
}
