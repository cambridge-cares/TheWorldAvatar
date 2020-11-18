package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the minimum temperature of a Chebyshev reaction from a CTML file.
 * 
 * @author msff2
 *
 */
public class RateCoeffTempMinParser extends CtmlConverter implements IRateCoeffTempMinParser{

	public void parse(String qName, Attributes attributes) {
		parseMinimumTemperature(qName, attributes);
		parseMinimumTemperatureUnits(qName, attributes);
	}

	private void parseMinimumTemperature(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffTMin())) {
				rateCoeffParseStatus.settMin(true);
			}
		}
	}

	private void parseMinimumTemperatureUnits(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffTMin())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffTMinUnits());
				if (units != null) {
					reactionTMin.setUnits(units);
					rateCoeffParseStatus.settMinUnits(true);
					rateCoeffParseStatus.settMin(true);
				}
			}
		}
	}
}
