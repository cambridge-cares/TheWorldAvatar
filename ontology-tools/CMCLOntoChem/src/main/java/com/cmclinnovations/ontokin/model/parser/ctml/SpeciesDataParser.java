package com.cmclinnovations.ontokin.model.parser.ctml;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

public class SpeciesDataParser extends CtmlConverter implements ISpeciesDataParser{
	public void parse(String qName, Attributes attributes){
	parseSpeciesData(qName, attributes);
	parseId(qName, attributes);
	parseCaseSensitive(qName, attributes);
	}
	
	private void parseSpeciesData(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesData())) {
			// Initialises all the objects maintaining the structure of 
			// a species
			initCtmlConverter.initSpecies();
			if(lastComment!=null){
				speciesData.setSourceComment(lastComment);
				lastComment = null;
			}
			speciesDataParseStatus.setSpeciesData(true);
		}
	}
	
	private void parseId(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesData())) {
			String id = attributes.getValue(appConfigCtml.getCtmlSpeciesDataId());
			if (id != null) {
				speciesData.setId(id);
				speciesDataParseStatus.setSpeciesDataId(true);
				speciesDataParseStatus.setSpeciesData(true);
			}
		}
	}

	private void parseCaseSensitive(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesData())) {
			String caseSensitive = attributes.getValue(appConfigCtml.getCtmlSpeciesDataCaseSensitive());
			if (caseSensitive != null) {
				speciesData.setCaseSensitive(caseSensitive);
				speciesDataParseStatus.setSpeciesDataCaseSensitive(true);
				speciesDataParseStatus.setSpeciesData(true);
			}
		}
	}

}
