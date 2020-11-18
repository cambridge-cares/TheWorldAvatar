package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the maximum pressure of a Chebyshev reaction from a CTML file.
 * 
 * @author msff2
 *
 */
public class RateCoeffPressureMaxParser extends CtmlConverter implements IRateCoeffPressureMaxParser{

	public void parse(String qName, Attributes attributes) {
		parseMaximumPressure(qName, attributes);
		parseMaximumPressureUnits(qName, attributes);
	}

	private void parseMaximumPressure(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffPMax())) {
				rateCoeffParseStatus.setpMax(true);
			}
		}
	}

	private void parseMaximumPressureUnits(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffPMax())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffPMaxUnits());
				if (units != null) {
					reactionPMax.setUnits(units);
					rateCoeffParseStatus.setpMaxUnits(true);
					rateCoeffParseStatus.setpMax(true);
				}
			}
		}
	}
}
