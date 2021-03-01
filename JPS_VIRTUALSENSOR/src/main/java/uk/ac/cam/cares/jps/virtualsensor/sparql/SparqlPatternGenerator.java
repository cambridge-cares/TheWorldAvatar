package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
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
	
	/** 
	 * Sizes of predicates and RdfType need to be equal
	 * @param Query
	 * @param Predicates
	 * @param RdfType
	 * @param FirstNode
	 * @param LastNode
	 * @return
	 */
	
	public static GraphPattern GetQueryGraphPattern (SelectQuery Query, Iri[] Predicates, Iri[] RdfType, Variable FirstNode, Variable LastNode) {
    	GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length-1];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length; i++) {
    		Variables[i] = Query.var();
    	}
    	
    	// first triple
    	GraphPattern firstTriple = FirstNode.has(Predicates[0],Variables[0]);
    	if (RdfType != null) {
    		if (RdfType[0] != null) {
    			CombinedGP = GraphPatterns.and(firstTriple,FirstNode.isA(RdfType[0]));
    		} else {
    			CombinedGP = GraphPatterns.and(firstTriple);
    		}
    	} else {
    		CombinedGP = GraphPatterns.and(firstTriple);
    	}
    	
    	// first and last triple are manually defined, hence length-1
    	for (int i=0; i < Variables.length-1; i++) {
    		GraphPattern triple = Variables[i].has(Predicates[i+1],Variables[i+1]);
    		if (RdfType != null) {
    			if (RdfType[i+1] != null) {
    				CombinedGP.and(triple,Variables[i].isA(RdfType[i+1]));
    			} else {
    				CombinedGP.and(triple);
    			}
    		} else {
    			CombinedGP.and(triple);
    		}
    	}
    	
    	// last triple
    	GraphPattern lastTriple = Variables[Variables.length-1].has(Predicates[Predicates.length-1],LastNode);
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(lastTriple,Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		} else {
    			CombinedGP.and(lastTriple);
    		}
    	} else {
    		CombinedGP.and(lastTriple);
    	}
    	
    	return CombinedGP;
    }
	
	public static GraphPattern GetQueryGraphPattern (SelectQuery Query, Iri[] Predicates, Iri[] RdfType, Iri FirstNode, Variable LastNode) {
    	GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length-1];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length; i++) {
    		Variables[i] = Query.var();
    	}
    	
    	// first triple
    	GraphPattern firstTriple = FirstNode.has(Predicates[0],Variables[0]);
    	if (RdfType != null) {
    		if (RdfType[0] != null) {
    			CombinedGP = GraphPatterns.and(firstTriple,FirstNode.isA(RdfType[0]));
    		} else {
    			CombinedGP = GraphPatterns.and(firstTriple);
    		}
    	} else {
    		CombinedGP = GraphPatterns.and(firstTriple);
    	}
    	
    	// first and last triple are manually defined, hence length-1
    	for (int i=0; i < Variables.length-1; i++) {
    		GraphPattern triple = Variables[i].has(Predicates[i+1],Variables[i+1]);
    		if (RdfType != null) {
    			if (RdfType[i+1] != null) {
    				CombinedGP.and(triple,Variables[i].isA(RdfType[i+1]));
    			} else {
    				CombinedGP.and(triple);
    			}
    		} else {
    			CombinedGP.and(triple);
    		}
    	}
    	
    	// last triple
    	GraphPattern lastTriple = Variables[Variables.length-1].has(Predicates[Predicates.length-1],LastNode);
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(lastTriple,Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		} else {
    			CombinedGP.and(lastTriple);
    		}
    	} else {
    		CombinedGP.and(lastTriple);
    	}
    	
    	return CombinedGP;
    }
}
