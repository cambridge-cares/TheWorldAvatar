package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

/**
 * Functions to generate triple/graph patterns that can be reused across all domains
 * @author Kok Foong Lee
 *
 */

public class SparqlPatternGenerator {
	private static Iri numericalValue = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	private static Iri hasValue = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
	private static Iri ScalarValue = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
	private static Iri hasUnitOfMeasure = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
	
	public static TriplePattern[] GetScalarTP(Iri Property, Iri PropertyValue, double value, Iri unit) {
    	TriplePattern Property_tp = Property.has(hasValue,PropertyValue);
    	TriplePattern PropertyValue_tp = PropertyValue.isA(ScalarValue)
    			.andHas(numericalValue,value)
    			.andHas(hasUnitOfMeasure,unit);
    	TriplePattern [] combined_tp = {Property_tp,PropertyValue_tp};
    	return combined_tp;
    }
	
	public static void GetQueryGraphPattern (SelectQuery Query, Iri[] Predicates, Iri[] RdfType) {
    	
    }
}
