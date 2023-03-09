package uk.ac.cam.cares.jps.base.query.fed;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.impl.SimpleIRI;
import org.eclipse.rdf4j.query.algebra.BindingSetAssignment;
import org.eclipse.rdf4j.query.algebra.QueryModelNode;
import org.eclipse.rdf4j.query.algebra.Service;
import org.eclipse.rdf4j.query.algebra.StatementPattern;
import org.eclipse.rdf4j.query.algebra.Var;
import org.eclipse.rdf4j.query.algebra.helpers.AbstractQueryModelVisitor;

import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

/**
 * This class visits all tree nodes of a parsed SPARQL query 
 * and retrieves information about SERVICE and VALUES clauses.
 * It collects 
 * (1) names and bounded values (i.e. endpoint URLs) of SERVICE variables
 * (2) IRIs of relations (used as predicates in basic triple patterns) and
 * (3) IRIs of classes (used as objects in basic triple patterns with predicate "rdf:type")
 *
 */
public class ParsedQueryTreeVisitor extends AbstractQueryModelVisitor<RuntimeException> {
	
	static final Logger LOGGER = LogManager.getLogger(ParsedQueryTreeVisitor.class);
	
	/**
	 * A helper class to store the information related to a query's SERVICE clause. 
	 *
	 */
	public class ServiceGraphPatternSummary {
		public String serviceVarName = null;
		public String serviceVarValue = null;
		public List<String> keys = null;
	}
	
	private static final String BLAZEGRAPH_GEO_PREFIX = PrefixToUrlMap.getPrefixUrl(Prefixes.BLAZEGRAPH_GEO);
	private List<ServiceGraphPatternSummary> summaries = new ArrayList<ServiceGraphPatternSummary>();
	private List<String> allKeys = new ArrayList<String>();
	/**
	 * Relations and classes found in basic triple patterns of the currently visited SERVICE clause. 
	 */
	private List<String> currentKeys = null;
	private Set<String> bindingNames = new HashSet<String>();
	
	public List<ServiceGraphPatternSummary> getSummariesWithoutBindings() {
		List<ServiceGraphPatternSummary> result = new ArrayList<ServiceGraphPatternSummary>();
		for (ServiceGraphPatternSummary current : summaries) {
			if (!bindingNames.contains(current.serviceVarName)) {
				result.add(current);
			}
		}
		
		return result;
	}
	
	/**
	 * Delegates if the node type is relevant for extracting information 
	 * (e.g. in case of StatementPattern node containing a basic triple pattern). 
	 */
	@Override
	protected void meetNode(QueryModelNode node) {
	
		LOGGER.debug("node=" + node.getSignature());
		
		if (node instanceof Service) {
			meetServiceNode((Service) node);
		} else if (node instanceof StatementPattern){
			meetStatementPatternNode((StatementPattern) node);
		} else if (node instanceof BindingSetAssignment) {
			meetBindingSetAssignmentNode((BindingSetAssignment) node);
		} else {
			super.meetNode(node);
		}
	}
	
	/**
	 * Starts and stops to collect information for a SERVICE clause and its enclosed triple patterns. 
	 * @param node
	 */
	protected void meetServiceNode(Service node) {
				
		Var serviceRef = node.getServiceRef();
		Value value = serviceRef.getValue();
		String svalue = (value == null)? null : value.toString();
		if (serviceRef.isAnonymous() && (value instanceof SimpleIRI)) {
			// i.e. the endpoint IRI for SERVICE is already given
			super.meetNode(node);
			LOGGER.debug("Service node: No keys for data source selection are collected since endpoint IRI is given!");
			return; 
		} else if (svalue != null && svalue.startsWith(BLAZEGRAPH_GEO_PREFIX)) {
			// i.e. SERVICE keyword marks a SPARQL subquery
			super.meetNode(node);
			LOGGER.debug("Service node: No keys for data source selection are collected since Blazegraph's special geo service was identified!");
			return; 
		}	
		// else:
		// i.e. SERVICE keyword is used to mark Blazegraph's geospatial service
		// triple patterns within the SERVICE body should not be considered for data source selection
		// (the geo vocabulary is not part of the service description)
		// nothing to do, only remove the geo vocabulary from collected keys 
		

		ServiceGraphPatternSummary summary = new ServiceGraphPatternSummary();
		summary.serviceVarName = node.getServiceRef().getName();
		if (svalue != null) {
			summary.serviceVarValue = svalue;
		}
		currentKeys = new ArrayList<String>();
		
		super.meetNode(node);
		
		summary.keys = currentKeys;
		currentKeys = null;
		
		summaries.add(summary);
		LOGGER.debug("The following keys were collected: " + summary.keys);
	}
	
	private String addKey(Var variable) {
		Value value = variable.getValue();
		if (value != null) {
			String key = value.stringValue();
			if (key.startsWith(BLAZEGRAPH_GEO_PREFIX))
				// don't consider Blazegraph special vocabulary for each geo service
				return null;
			allKeys.add(key);
			if (currentKeys != null) {
				currentKeys.add(key);
				//LOGGER.debug("collected new key=" + key);
				return key;
			}
		}
		return null;
	}
	
	/**
	 * Extracts relation and class IRIs from bounded predicates and objects of a basic triple pattern.
	 * 
	 * @param node
	 */
	protected void meetStatementPatternNode(StatementPattern node) {
		// Only add predicate and class IRIs as keys. 
	    // If getSubjectVar() is bounded to a value, the value is usually
		// not a class IRI and should not be added.
		// Similarly, íf getObjectVar() is bounded to a value and the predicate is not rdf:type.		
		String key = addKey(node.getPredicateVar());
		if ((key != null) && (key == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")) {
			addKey(node.getObjectVar());
		}
		
		super.meetNode(node);
	}
	
	/**
	 * Collects bounded values (i.e. endpoint URLs) from a VALUES clause.
	 * 
	 * @param node
	 */
	protected void meetBindingSetAssignmentNode(BindingSetAssignment node) {
		LOGGER.debug("binding names=" + node.getBindingNames());		
		bindingNames.addAll(node.getBindingNames());		
		super.meetNode(node);
	}
}
