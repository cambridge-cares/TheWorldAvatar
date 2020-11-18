package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;

/**
 * This class parses the minimum pressure of a Chebyshev reaction from a CTML file.
 * 
 * @author msff2
 *
 */
public class RateCoeffPressureMinParser extends CtmlConverter implements IRateCoeffPressureMinParser{

	public void parse(String qName, Attributes attributes) {
		parseMinimumPressure(qName, attributes);
		parseMinimumPressureUnits(qName, attributes);
	}

	private void parseMinimumPressure(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffPMin())) {
				rateCoeffParseStatus.setpMin(true);
			}
		}
	}

	private void parseMinimumPressureUnits(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffPMin())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffPMinUnits());
				if (units != null) {
					reactionPMin.setUnits(units);
					rateCoeffParseStatus.setpMinUnits(true);
					rateCoeffParseStatus.setpMin(true);
				}
			}
		}
	}
}
