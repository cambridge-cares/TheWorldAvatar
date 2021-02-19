package com.cmclinnovations.ontochem.model.parser.ctml.speciesdata;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

public class SpeciesParser extends CtmlConverter implements ISpeciesParser{

	public void parse(String qName, Attributes attributes){
	parseSpecies(qName, attributes);
	parseSpeciesName(qName, attributes);
	parseSpeciesPhase(qName, attributes);
	parseSpeciesComment(qName, attributes);
	parseSpeciesNote(qName, attributes);
	parseSpeciesAtomArray(qName, attributes);
	}
	
	private void parseSpecies(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpecies())) {
			if(lastComment!=null){
				species.setSourceComment(lastComment);
				lastComment = null;
			}
			speciesParseStatus.setSpeciesDataSpecies(true);
		}
	}
	
	private void parseSpeciesName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpecies())) {
			String name = attributes.getValue(appConfigCtml.getCtmlSpeciesDataSpeciesName());
			if (name != null) {
				species.setName(name);
				speciesParseStatus.setSpeciesName(true);
				speciesParseStatus.setSpeciesDataSpecies(true);
			}
		}
	}

	private void parseSpeciesPhase(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpecies())) {
			String phase = attributes.getValue(appConfigCtml.getCtmlSpeciesDataSpeciesPhase());
			if (phase != null) {
				species.setPhase(phase);
				speciesParseStatus.setSpeciesPhase(true);
				speciesParseStatus.setSpeciesDataSpecies(true);
			}
		}
	}
	
	/**
	 * Parses and reads the comment about a species.
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSpeciesComment(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies()) {
			if ((!speciesThermoParseStatus.isSpeciesThermo() && !speciesTransportParseStatus.isTransport())
					&& qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpeciesComment())) {
				speciesParseStatus.setSpeciesComment(true);
			}
		}
	}
	
	/**
	 * Parses and reads the note about a species
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSpeciesNote(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies() && qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpeciesNote())) {
			speciesParseStatus.setSpeciesNote(true);
		}
	}
	
	/**
	 * Parses and reads the atom array of a species
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void parseSpeciesAtomArray(String qName, Attributes attributes) {
		if (speciesParseStatus.isSpeciesDataSpecies() && qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpeciesAtomArray())) {
			speciesParseStatus.setSpeciesAtomArray(true);
		}
	}
}


