package uk.ac.cam.cares.jps.virtualsensor.sparql;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;
import uk.ac.cam.cares.jps.virtualsensor.configuration.SparqlAuthentication;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

/**
 * Functions to generate triple/graph patterns that can be reused across all domains
 * @author Kok Foong Lee
 *
 */

public class SparqlGeneral {
	private static String endpoint = KeyValueManager.get(IKeys.URL_VIRTUALSENSOR);
	
	private static Iri numericalValue = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	private static Iri hasValue = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
	private static Iri ScalarValue = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
	private static Iri hasUnitOfMeasure = iri("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
	
	public static void performUpdate(ModifyQuery query) {
        RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQuery(query.getQueryString());
        kbClient.executeUpdate();
    }
	
	public static JSONArray performQuery(SelectQuery query) {
        RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient();
        kbClient.setUser(SparqlAuthentication.getUser());
        kbClient.setPassword(SparqlAuthentication.getPassword());
        kbClient.setQueryEndpoint(endpoint);
        kbClient.setQuery(query.getQueryString());
        JSONArray result = null;
        result = kbClient.executeQuery(); 
        return result;
    }

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
    	
    	Variable[] Variables = new Variable[Predicates.length];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length-1; i++) {
    		Variables[i] = Query.var();
    	}
    	Variables[Variables.length-1] = LastNode;
    	
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
    	
    	// the remaining
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
    	
    	// type for the final node, if given
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		}
    	}
    	
    	return CombinedGP;
    }
	
	public static GraphPattern GetQueryGraphPattern (SelectQuery Query, Iri[] Predicates, Iri[] RdfType, Iri FirstNode, Variable LastNode) {
        GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length-1; i++) {
    		Variables[i] = Query.var();
    	}
    	Variables[Variables.length-1] = LastNode;
    	
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
    	
    	// the remaining
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
    	
    	// type for the final node, if given
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		}
    	}
    	
    	return CombinedGP;
    }
	
	public static GraphPattern GetQueryGraphPattern (SelectQuery Query, Iri[] Predicates, Iri[] RdfType, Variable FirstNode) {
        GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length];
    	
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
    	
    	// the remaining
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
    	
    	// type for the final node, if given
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		}
    	}
    	
    	return CombinedGP;
    }
	
	public static GraphPattern GetQueryGraphPattern (SubSelect Query, Iri[] Predicates, Iri[] RdfType, Iri FirstNode, Variable LastSecondNode, Variable LastNode) {
    	GraphPattern CombinedGP = null;
    	
    	Variable[] Variables = new Variable[Predicates.length];
    	
    	// initialise intermediate nodes
    	for (int i=0; i < Variables.length-2; i++) {
    		Variables[i] = Query.var();
    	}
    	Variables[Variables.length-2] = LastSecondNode;
    	Variables[Variables.length-1] = LastNode;
    	
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
    	
    	// the remaining
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
    	
    	// type for the final node, if given
    	if (RdfType != null) {
    		if (RdfType[RdfType.length-1] != null) {
    			CombinedGP.and(Variables[Variables.length-1].isA(RdfType[RdfType.length-1]));
    		}
    	}
    	
    	return CombinedGP;
    }
	
	public static ModifyQuery UpdateValue(Prefix[] Prefixes,Iri[] Predicates, Iri[] RdfType, Iri FirstNode, double newvalue,Iri NamedGraph) {
		SubSelect Sub = GraphPatterns.select();
		
		Variable datavalue_iri = SparqlBuilder.var("datavalue_iri");
    	Variable oldvalue = SparqlBuilder.var("oldvalue");
    	
    	GraphPattern queryPattern = GetQueryGraphPattern(Sub,Predicates,RdfType,FirstNode,datavalue_iri,oldvalue);
    	
    	// triple to delete
    	TriplePattern delete_tp = datavalue_iri.has(Predicates[Predicates.length-1],oldvalue);
    	// new triple to add
    	TriplePattern replace_tp = datavalue_iri.has(Predicates[Predicates.length-1],newvalue);
    	
    	Sub.select(datavalue_iri,oldvalue).where(queryPattern).from(NamedGraph);
    	ModifyQuery modify = Queries.MODIFY();
        modify.prefix(Prefixes).delete(delete_tp).insert(replace_tp).where(Sub).with(NamedGraph);
        return modify;
	}
}
