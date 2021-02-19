package com.cmclinnovations.ontochem.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Implements the method that forwards calls to the following parsers:
 * 1. Phase attributes (e.g. dimension and id) parser.
 * 2. Phase ElementArray parser.
 * 3. Phase SpeciesArray parser.
 * 4. Phase ReactionArray parser.
 * 5. Phase State parser.
 * 6. Phase Thermo(dynamic) model parser.
 * 7. Phase Kinetics model parser.
 * 8. Phase Transport parser.
 * 9. PhaseArray parser. 
 * 
 * @author msff2
 *
 */
public class PhaseConverter extends CtmlConverter implements IPhaseConverter {
	public void parse(String qName, Attributes attributes) {
		// Calls the method that forwards the call to
		// the methods that parse the phases of a
		// reaction mechanism
		iPhaseParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the element array belonging
		// to a phase contained in a reaction mechanism
		// reaction mechanism
		iElementArrayParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the species array belonging
		// to a phase contained in a reaction mechanism
		// reaction mechanism
		iSpeciesArrayParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the reaction array metadata
		// for those reactions which belong to a phase
		// contained in a reaction mechanism reaction mechanism
		iReactionArrayParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the state of a phase
		// contained in a reaction mechanism
		iPhaseStateParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the state of a phase
		// contained in a reaction mechanism
		iThermoParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the site density of a phase
		// contained in a reaction mechanism
		iSiteDensityParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the kinetics of a phase
		// contained in a reaction mechanism
		iKineticsParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the transport property of
		// a phase contained in a reaction mechanism
		iTransportParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the phase array property of
		// a phase contained in a reaction mechanism
		iPhaseArrayParser.parse(qName, attributes);
	}
}
