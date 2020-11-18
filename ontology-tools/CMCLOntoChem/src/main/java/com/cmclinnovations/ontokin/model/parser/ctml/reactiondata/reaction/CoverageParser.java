package com.cmclinnovations.ontokin.model.parser.ctml.reactiondata.reaction;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontokin.model.converter.ctml.CtmlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.reaction.Coverage;

/**
 * This class parses the coverage dependent part(s) of a reaction rate coefficient
 * from a CTML file.
 * 
 * @author msff2
 *
 */
public class CoverageParser extends CtmlConverter implements ICoverageParser{
	public void parse(String qName, Attributes attributes) {
		parseCoverage(qName, attributes);
		parseSpecies(qName, attributes);
		parseCoeffa(qName, attributes);
		parseCoeffaUnits(qName, attributes);
		parseCoeffm(qName, attributes);
		parseCoeffmUnits(qName, attributes);
		parseCoeffe(qName, attributes);
		parseCoeffeUnits(qName, attributes);
	}
	
	private void parseCoverage(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffArrheniusCoverage())) {
				coverage = new Coverage();
				coverageParseStatus.setCoverage(true);
			}
		}
	}
	
	private void parseSpecies(String qName, Attributes attributes) {
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffArrheniusCoverage())) {
				String species = attributes.getValue(appConfigCtml.getRateCoeffArrheniusCoverageSpecies());
				if (species != null) {
					coverage.setSpecies(species);
					coverageParseStatus.setSpecies(true);
					coverageParseStatus.setCoverage(true);
				}
			}
		}
	}
	
	private void parseCoeffa(String qName, Attributes attributes) {
		if (coverageParseStatus.isCoverage()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageA())) {
				coverageParseStatus.setA(true);
			}
		}
	}

	private void parseCoeffaUnits(String qName, Attributes attributes) {
		if (coverageParseStatus.isCoverage()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageA())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusCoverageAUnits());
				if (units != null) {
					coverageA.setUnits(units);
					coverageParseStatus.setaUnits(true);
					coverageParseStatus.setCoverage(true);
				}
			}
		}
	}

	private void parseCoeffm(String qName, Attributes attributes) {
		if (coverageParseStatus.isCoverage()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageM())) {
				coverageParseStatus.setM(true);
			}
		}
	}

	private void parseCoeffmUnits(String qName, Attributes attributes) {
		if (coverageParseStatus.isCoverage()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageM())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusCoverageMUnits());
				if (units != null) {
					coverageM.setUnits(units);
					coverageParseStatus.setmUnits(true);
					coverageParseStatus.setCoverage(true);
				}
			}
		}
	}

	private void parseCoeffe(String qName, Attributes attributes) {
		if (coverageParseStatus.isCoverage()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageE())) {
				coverageParseStatus.setE(true);
			}
		}
	}

	private void parseCoeffeUnits(String qName, Attributes attributes) {
		if (coverageParseStatus.isCoverage()) {
			if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageE())) {
				String units = attributes.getValue(appConfigCtml.getRateCoeffArrheniusCoverageEUnits());
				if (units != null) {
					coverageE.setUnits(units);
					coverageParseStatus.seteUnits(true);
					coverageParseStatus.setCoverage(true);
				}
			}
		}
	}
}
