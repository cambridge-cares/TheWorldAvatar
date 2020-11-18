package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the maximum temperature of a Chebyshev reaction from a CTML file.
 * 
 * @author msff2
 *
 */
public class RateCoeffTempMaxParser extends CtmlConverter implements IRateCoeffTempMaxParser{

	public void parse(String qName, Attributes attributes) {
		parseMaximumTemperature(qName, attributes);
		parseMaximumTemperatureUnits(qName, attributes);
	}

	private void parseMaximumTemperature(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffTMax())) {
				rateCoeffParseStatus.settMax(true);
			}
		}
	}

	private void parseMaximumTemperatureUnits(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffTMax())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffTMaxUnits());
				if (units != null) {
					reactionTMax.setUnits(units);
					rateCoeffParseStatus.settMaxUnits(true);
					rateCoeffParseStatus.settMax(true);
				}
			}
		}
	}
}
