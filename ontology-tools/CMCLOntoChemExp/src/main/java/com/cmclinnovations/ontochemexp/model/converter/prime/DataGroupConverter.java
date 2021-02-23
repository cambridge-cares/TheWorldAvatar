package com.cmclinnovations.ontochemexp.model.converter.prime;

import org.xml.sax.Attributes;
/**
 * Implements the method that forwards calls to the Apparatus parser.
 * 
 * @author Songyi Deng  (sd626@cam.ac.uk)
 *
 */

public class DataGroupConverter extends PrimeConverter implements IDataGroupConverter{
	public void parse(String qName, Attributes attributes){
		// Calls the method that forwards calls to
		// the methods that parse DataGroup   data and metadata.
		iDataGroupParser.parse(qName, attributes);
		iDataGroupDataGroupLinkParser.parse(qName, attributes);
		iDataGroupPropertyParser.parse(qName, attributes);
		iDataGroupPropertySpeciesLinkParser.parse(qName, attributes);
		iDataGroupPropertyComponentParser.parse(qName, attributes);
		iDataGroupPropertyValueParser.parse(qName, attributes);
		iDataGroupPropertyUncertaintyParser.parse(qName, attributes);
		iDataGroupPropertyDerivedPropertyParser.parse(qName, attributes);
		iDataGroupPropertyDerivedPropertyFeatureParser.parse(qName, attributes);
		iDataGroupPropertyDerivedPropertyFeatureIndicatorParser.parse(qName, attributes);
		iDataGroupPropertyDerivedPropertyFeatureObservableParser.parse(qName, attributes);
		iDataPointParser.parse(qName, attributes);
		iDataGroupDataPointXParser.parse(qName, attributes);
	}
}
