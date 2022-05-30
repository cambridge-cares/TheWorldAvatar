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

public class DataSourceSelector {
	
	static final Logger LOGGER = LogManager.getLogger(DataSourceSelector.class);
	
	private ServiceDescriptionIndexer indexer = null;
	private Map<String,String> host2host = null;
	
	public DataSourceSelector(ServiceDescriptionIndexer indexer) {
		this.indexer = indexer;
	}
	
	public DataSourceSelector(ServiceDescriptionIndexer indexer, Map<String,String> host2host) {
		this.indexer = indexer;
		this.host2host = host2host;
	}
	
	private String replaceHost(String s) {
		if (host2host == null) {
			return s;
		}
		for (String host : host2host.keySet()) {
			if (s.contains(host)) {
				String newhost = host2host.get(host);
				s = s.replace(host, newhost);
				LOGGER.info("replace host in endpoint url, host="  + host + ", newhost=" + newhost + ", new url=" + s);
				return s;
			}
		}
		return s; 
	}
	
	public String addValuesClauses(String sparql) {
		
		String result = sparql;
		
		List<ServiceGraphPatternSummary> summaries = DataSourceSelector.extractKeysForSubqueries(sparql);
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
