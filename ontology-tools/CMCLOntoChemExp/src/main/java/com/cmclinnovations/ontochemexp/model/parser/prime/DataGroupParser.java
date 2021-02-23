package com.cmclinnovations.ontochemexp.model.parser.prime;

import org.apache.commons.lang3.text.WordUtils;
import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.DataGroupParseStatus;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * This class parses a PrIMe dataGroup and stores its data and metadata in the
 * apparatus data structure.
 * 
 * @author Songyi Deng (sd626@cam.ac.uk)
 *
 */
public class DataGroupParser extends PrimeConverter implements IDataGroupParser {
	/**
	 * Parses PrIMe DataGroup data.
	 * 
	 * @param qName
	 * @param attributes
	 */

	public void parse(String qName, Attributes attributes) {
		parseDataGroup(qName, attributes);
		parseDataGroupID(qName, attributes);
		parseDataGroupLabel(qName, attributes);
		parseDataGroupDataPointForm(qName, attributes);
	}

	/**
	 * Checks the appearance of the DataGroup tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDataGroup(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup())) {
			dataGroupParseStatus.setDataGroup(true);
			inDataGroup = true;
		}
	}

	private void parseDataGroupLabel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup())) {
			String label = attributes.getValue(primeVocabulary.getAttribLabel());
			if (label != null) {
				dataGroup.setLabel(label);
				dataGroupParseStatus.setLabel(true);
				dataGroupParseStatus.setDataGroup(true);
			}
		}
	}

	private void parseDataGroupID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup())) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroup.setId(id);
				dataGroupParseStatus.setID(true);
				dataGroupParseStatus.setDataGroup(true);
			}
		}
	}

	private void parseDataGroupDataPointForm(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup())) {
			String dataPointForm = attributes.getValue(primeVocabulary.getAttribDataPointForm());
			if (dataPointForm != null) {
				dataGroup.setDataPointForm(dataPointForm);
				dataGroupParseStatus.setDataPointForm(true);
				dataGroupParseStatus.setDataGroup(true);
			}
		}
	}
}
