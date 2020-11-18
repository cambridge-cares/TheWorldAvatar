package com.cmclinnovations.ontokin.model.converter.ctml;

import org.xml.sax.Attributes;
/**
 * Implements the method that forwards calls to the following parsers:
 * 1. The SpeciesData parser.
 * 2. The Species parser.
 * 3. The Species Size parser.
 * 4. The Species Density parser.
 * 
 * @author msff2
 *
 */
public class SpeciesConverter extends CtmlConverter implements ISpeciesConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards the call to
		// the methods that parse the speciesData block
		iSpeciesDataParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the speciesData block
		iSpeciesParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the species size block
		iSpeciesSizeParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the species density block
		iSpeciesDensityParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the species density block
		iSpeciesDensityParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the thermodynamic properties
		iSpeciesThermoParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the NASA Polynomial metadata
		iNasaPolyParser.parse(qName, attributes);	
		// Calls the method that forwards the call to
		// the methods that parse the NASA Polynomial coefficients
		iCoeffArrayParser.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the methods that parse the transport parameters of a species
		iSpeciesTransportParser.parse(qName, attributes);
	}
}
