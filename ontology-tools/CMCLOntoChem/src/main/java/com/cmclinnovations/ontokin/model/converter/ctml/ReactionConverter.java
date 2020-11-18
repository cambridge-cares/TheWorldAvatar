package com.cmclinnovations.ontokin.model.converter.ctml;

import org.xml.sax.Attributes;

public class ReactionConverter extends CtmlConverter implements IReactionConverter{
	public void parse(String qName, Attributes attributes) {
		// Calls the method that forwards the call to
		// the methods that parse the reaction data metadata 
		iReactionDataParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse a reaction.
		iReactionParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the species size block.
		iReactionOrderParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the minimum temperature and its units
		// for Chebyshev coefficients.
		iRateCoeffTMinParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the maximum temperature and its units 
		// for Chebyshev coefficients. 
		iRateCoeffTMaxParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the minimum pressure and its units 
		// for Chebyshev coefficients.
		iRateCoeffPMinParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the maximum pressure and its units 
		// for Chebyshev coefficients.
		iRateCoeffPMaxParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the name, units, degreeT and degreeP 
		// attributes and data of a Chebyshev rate coefficient array.
		iRateCoeffArrayParser.parse(qName, attributes);
		// Calls the method that forwards the call to the methods that parse
		// the attributes of Arrhenius rate coefficients.
		iArrheniusParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the species, a, m and e 
		// attributes of coverage dependency coefficients.
		iCoverageParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the terms and units 
		// that can be used in a Landau-Teller rate expression.
		iLandauTellerParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the reaction 
		// efficiencies of species.
		iEfficiencyParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the fall of model 
		// coefficients.
		iFallOffParser.parse(qName, attributes);
	}
}
