package uk.ac.cam.cares.jps.base.query.fed;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.query.parser.ParsedQuery;
import org.eclipse.rdf4j.query.parser.sparql.SPARQLParser;

import uk.ac.cam.cares.jps.base.query.fed.ParsedQueryTreeVisitor.ServiceGraphPatternSummary;
import uk.ac.cam.cares.jps.base.query.fed.ServiceDescriptionIndexer.ServiceDescriptionSummary;

/**
 * This class selects automatically datasets 
 * that are relevant for a SERVICE clause with an unbounded SERVICE variable.
 * For a given SPARQL query with unbounded SERVICE variable, 
 * it adds the corresponding endpoint URLs as part of a VALUES clause.  
 * The selection relies on the injected schema-level indexer.
 * Only those datasets can be selected 
 * (that means only those endpoints can be considered as federation members)
 * that have been indexed previously. 
 */
public class DataSourceSelector {
	
	static final Logger LOGGER = LogManager.getLogger(DataSourceSelector.class);
	
	private ServiceDescriptionIndexer indexer = null;
	private Map<String,String> host2host = null;
	
	/**
	 * @param indexer a schema-level indexer containing relevant information from all federation members
	 */
	public DataSourceSelector(ServiceDescriptionIndexer indexer) {
		this.indexer = indexer;
	}
	
	/**
	 * @param indexer a schema-level indexer containing relevant information from all federation members
	 * @param host2host if not null, host2host is used to convert host addresses in endpoint URLs
	 */
	public DataSourceSelector(ServiceDescriptionIndexer indexer, Map<String,String> host2host) {
		this.indexer = indexer;
		this.host2host = host2host;
	}
	
	/**
	 * Replaces a host in the given URL as specified in the host conversion map.
	 * Returns the original URL if no host is found or no conversion map is given.
	 * 
	 * @param url
	 * @return
	 */
	private String replaceHost(String url) {
		if (host2host == null) {
			return url;
		}
		for (String host : host2host.keySet()) {
			if (url.contains(host)) {
				String newhost = host2host.get(host);
				url = url.replace(host, newhost);
				LOGGER.info("replace host in endpoint url, host="  + host + ", newhost=" + newhost + ", new url=" + url);
				return url;
			}
		}
		return url; 
	}
	
	/**
	 * Checks whether the given SPARQL query contains any SERVICE clause with unbounded SERVICE variable.
	 * For each such SERVICE clause, the relation and class IRIs from its corresponding enclosed 
	 * basic triple patterns are extracted to identify datasets that might be relevant for the SERVICE clause.
	 * Finally, the datasets are added as VALUES clause for the unbounded SERVICE variable. 
	 * See {@link FederatedQueryFactory#createForQueriesWithGivenEndpoints(String)} for an example with
	 * added VALUES clauses. 
	 * 
	 * @param sparql a SPARQL query with SERVICE clauses
	 * @return the SPARQL query with additional VALUES clauses for unbounded SERVICE variables
	 */
	public String addValuesClauses(String sparql) {
		
		String result = sparql;
		
		List<ServiceGraphPatternSummary> summaries = extractKeysForSubqueries(sparql);
		for (ServiceGraphPatternSummary summary : summaries) {
			List<String> uris = summary.keys;
			List<ServiceDescriptionSummary> serviceDescrSummaries = indexer.conjunctiveQuery(uris);
			List<String> endpointsURLs = new ArrayList<String>();
			for (ServiceDescriptionSummary sdSummary : serviceDescrSummaries) {
				String url = replaceHost(sdSummary.endpointURL);
				endpointsURLs.add(url);
			}
			
			result = DataSourceSelector.addValuesClause(result, summary.serviceVarName, endpointsURLs); 
		}
		
		return result;
	}

	/**
	 * Adds a VALUES clause to the SPARQL query for the given SERVICE variable. 
	 * The VALUES clause contains the given values (endpoint URLs).
	 * 
	 * @param sparql
	 * @param varName
	 * @param varValues
	 * @return the SPARQL query with added VALUES clause
	 */
	public static String addValuesClause(String sparql, String varName, List<String> varValues) {
		
		int minIndex = - 1;
		// find the minimum position for LIMIT, ORDER BY etc.´, see https://www.w3.org/TR/sparql11-query/#rSolutionModifier 
		String[] clauses = new String[] {"LIMIT", "OFFSET", "ORDER", "HAVING", "GROUP"};
		for (String clause : clauses) {
			int index = sparql.lastIndexOf(clause);
			if (minIndex == -1 || index < minIndex) {
				minIndex = index;
			}
		}
		String firstPart = sparql;
		if (minIndex > -1) {
			firstPart = sparql.substring(0, minIndex);
		}
		
		int index = firstPart.lastIndexOf("}");
		StringBuffer b = new StringBuffer("  VALUES ?").append(varName).append(" { ");
		for (String value : varValues) {
			b.append("<").append(value).append("> ");
		}
		b.append("}\r\n");
		
		LOGGER.info("added VALUES clause = " + b);
		
		String newSparql = sparql.substring(0, index)
				+ b
				+ sparql.substring(index, sparql.length());

	 	return newSparql;
	}
	
	/**
	 * Extracts all SERVICE clauses from the given SPARQL query
	 * and converts them into a list of SERVICE clause summaries.
	 * Each summary contains the SERVICE variable, 
	 * its assigned endpoint URL (if specified) 
	 * and the relation and class IRIs in the enclosed basic triple patterns. 
	 * 
	 * @param sparql
	 * @return a list with summarized SERVICE clauses
	 */
	public static List<ServiceGraphPatternSummary> extractKeysForSubqueries(String sparql) {
	 	SPARQLParser parser = new SPARQLParser();
	 	String baseURI = null;
	 	ParsedQuery parsedQuery = parser.parseQuery(sparql, baseURI);
	 	
		//QueryModelTreePrinter treePrinter = new QueryModelTreePrinter();
		//parsedQuery.getTupleExpr().visit(treePrinter);
		//LOGGER.debug(treePrinter.getTreeString() + "/n");

		LOGGER.debug("Visiting nodes on parsed query tree");
		ParsedQueryTreeVisitor visitor = new ParsedQueryTreeVisitor();
		parsedQuery.getTupleExpr().visit(visitor);
		List<ServiceGraphPatternSummary> result = visitor.getSummariesWithoutBindings();
		String serviceVars = "";
		for (ServiceGraphPatternSummary summary : result) {
			serviceVars += "?" + summary.serviceVarName + ", ";
		}
		LOGGER.info("Visited nodes on parsed query tree, found services variables=" + serviceVars);
		return result;
	}
	
}
