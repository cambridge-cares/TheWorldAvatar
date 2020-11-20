package com.cmclinnovations.ontochem.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochem.model.converter.ctml.CtmlConverter;

/**
 * This class parses efficiencies of species in a reaction from a CTML file.
 * 
 * @author msff2
 *
 */
public class EfficiencyParser extends CtmlConverter implements IEfficiencyParser{

	public void parse(String qName, Attributes attributes) {
		parseEfficiency(qName, attributes);
		parseDefaultValue(qName, attributes);
	}

	private void parseEfficiency(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffEfficiencies())) {
				efficiencyParseStatus.setEfficiencies(true);
			}
		}
	}

	private void parseDefaultValue(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffEfficiencies())) {
				String defaultValue = attributes.getValue(appConfigCtml.getRateCoeffEfficienciesDefaultValue());
				if (defaultValue != null) {
					efficiencies.setDefault(defaultValue);
					efficiencyParseStatus.setEfficiencyDefaultValue(true);
					efficiencyParseStatus.setEfficiencies(true);
				}
			}
		}
	}
}