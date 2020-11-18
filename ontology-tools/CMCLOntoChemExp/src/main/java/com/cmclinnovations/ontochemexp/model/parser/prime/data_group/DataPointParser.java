package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.DataGroupDataPointX;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.data_group.DataPointParseStatus;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;


public class DataPointParser extends PrimeConverter implements IDataPointParser {

	/**
	 * Parses PrIMe DataPoint data.
	 * 
	 * @param qName
	 * @param attributes
	 */

	public void parse(String qName, Attributes attributes) {
		parseDataPoint(qName, attributes);
		parseDataPointID(qName, attributes);
	}

	/**
	 * Checks the appearance of the DataPoint tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseDataPoint(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataPoint()) && inDataGroup) {
			dataGroupDataPointParseStatus.setDataPoint(true);
		}
	}
	
	private void parseDataPointID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataPoint()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupDataPoint.setId(id);
				dataGroupDataPointParseStatus.setID(true);
				dataGroupDataPointParseStatus.setDataPoint(true);
			}
		}
	}
}
