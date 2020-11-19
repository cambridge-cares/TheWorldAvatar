package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;

public class DataGroupDataGroupLinkParser extends PrimeConverter implements IDataGroupDataGroupLinkParser {
	public void parse(String qName, Attributes attributes) {
		// Itself
		parseDataGroupDataGroupLink(qName, attributes);
		parseDataGroupDataGroupLinkDataGroupID(qName, attributes);
		parseDataGroupDataGroupLinkDataPointID(qName, attributes);
	}
	
	private void parseDataGroupDataGroupLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroupLink()) && inDataGroup) {
			dataGroupDataGroupLinkParseStatus.setDataGroupLink(true);
		}
	}
	
	private void parseDataGroupDataGroupLinkDataGroupID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroupLink()) && inDataGroup) {
			String dataGroupID = attributes.getValue(primeVocabulary.getAttribDataGroupID());
			if (dataGroupID != null) {
				dataGroupDataGroupLink.setDataGroupID(dataGroupID);
				dataGroupDataGroupLinkParseStatus.setDataGroupLinkDataGroupID(true);
				dataGroupDataGroupLinkParseStatus.setDataGroupLink(true);
			}
		}
	}
	
	private void parseDataGroupDataGroupLinkDataPointID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroupLink()) && inDataGroup) {
			String dataPointID = attributes.getValue(primeVocabulary.getAttribDataPointID());
			if (dataPointID != null) {
				dataGroupDataGroupLink.setDataPointID(dataPointID);
				dataGroupDataGroupLinkParseStatus.setDataGroupLinkDataPointID(true);
				dataGroupDataGroupLinkParseStatus.setDataGroupLink(true);
			}
		}
	}
}
