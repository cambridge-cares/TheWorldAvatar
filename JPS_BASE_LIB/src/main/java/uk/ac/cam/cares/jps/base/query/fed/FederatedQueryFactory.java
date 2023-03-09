package uk.ac.cam.cares.jps.base.query.fed;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.federated.FedXConfig;
import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.federated.monitoring.MonitoringUtil;
import org.eclipse.rdf4j.federated.monitoring.QueryPlanLog;
import org.eclipse.rdf4j.federated.repository.FedXRepository;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.resultio.sparqljson.SPARQLResultsJSONWriter;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * A factory for creating different engines / implementations to execute federated queries 
 * by using the common interface method {@link FederatedQueryInterface#executeFederatedQuery(String)}. 
 *
 */
public class FederatedQueryFactory {
	
	static final Logger LOGGER = LogManager.getLogger(FederatedQueryFactory.class);
	private static ServiceDescriptionIndexer indexer = null;

	/**
	 * FedX is a federated query engine and part of the RDF4J project. 
	 * Given a SPARQL 1.0 query, FedX sends ASK queries for each basic triple pattern 
	 * to determine on-the-fly which of the federation members may contribute to the query result set. 
	 * See {@link https://rdf4j.org/documentation/programming/federation/} for details.
	 * 
	 * @param federationMembers a list of URLs of SPARQL endpoints considered for query 
	 * @return
	 */
	public static FederatedQueryInterface createFedX(List<String> federationMembers) {
		
		LOGGER.debug("create FedX with federation members=" + federationMembers.size());
		
		final FedXConfig config = new FedXConfig()
				.withEnableMonitoring(true)
				.withLogQueryPlan(true)
				.withDebugQueryPlan(true)
				.withLogQueries(true);
		
		final FedXRepository repository = FedXFactory.newFederation()
				.withSparqlEndpoints(federationMembers)
				.withConfig(config)
				.create();
		
		FederatedQueryInterface impl = new FederatedQueryInterface() {
			
			@Override
			public String executeFederatedQuery(String sparql) {
				
				LOGGER.debug("FedX executeQuery=\n" + sparql);
				
				String resultJson = null;
				try (RepositoryConnection conn = repository.getConnection()) {

					TupleQuery query = conn.prepareTupleQuery(sparql);			
					try (TupleQueryResult result = query.evaluate()) {
						
						try (OutputStream out = new ByteArrayOutputStream()) {

							SPARQLResultsJSONWriter writer = new SPARQLResultsJSONWriter(out);
							List<String> columnHeaders = new LinkedList<String>();
					        columnHeaders.addAll(result.getBindingNames());
					        writer.startQueryResult(columnHeaders);
							while (result.hasNext()) {
								BindingSet b = result.next();
					            writer.handleSolution(b);
							}
							writer.endQueryResult();
							resultJson = out.toString();
							resultJson = JenaResultSetFormatter.convertToSimplifiedJsonArray(resultJson).toString();	
							
						} catch (IOException exc) {
							throw new JPSRuntimeException(exc);
						}
					}
				}
				LOGGER.debug(QueryPlanLog.getQueryPlan());
				MonitoringUtil.printMonitoringInformation(repository.getFederationContext());
				
				repository.shutDown();
				return resultJson;
			}
		};
		
		return impl;
	}
	
	/**
	 * Executes a SPARQL 1.1 query with specified endpoints, e.g. 
	 * <pre>
	 * PREFIX ...
	 * SELECT ?exp ?yield_value ?yield_unit
	 * WHERE {
	 *   SERVICE ?service1 {
	 *     ?exp ontorxn:hasYield ?yield.
	 *     ?yield om:hasValue ?measure.
	 *     ?measure om:hasNumericalValue ?yield_value; om:hasUnit ?yield_unit.
	 *   }
	 *   SERVICE ?service2 {
	 *      &lt;https://www.example.com/triplestore/ontodoe/DoE_1/DoE_1&gt; ontodoe:utilisesHistoricalData ?historical_data.
	 *     ?historical_data ontodoe:refersTo ?exp.
	 *   }
	 *   VALUES ?service1 { &lt;http://172.17.0.3:8080/blazegraph/namespace/lab_1/sparql&gt; &lt;http://172.17.0.4:8080/blazegraph/namespace/lab_2/sparql&gt; }
	 *   VALUES ?service2 { &lt;http://172.17.0.3:8080/blazegraph/namespace/doe_chemrxn/sparql&gt; }
	 * }</pre>
	 * Such a query can be executed either on RDF4J server or Blazegraph server. 
	 * Both cases require any dataset or namespace, resp. as a starting point for the query request. 
	 * This is an endpoint URL specified by fedEngineUrl, e.g.
	 * <pre>
	 * http://www.theworldavatar.com/blazegraph/namespace/somenamesapce/sparql
	 * http://localhost:8080/rdf4j-server/repositories/somenamespace
	 * </pre> 
	 * 
	 * @param fedEngineUrl endpoint URL as starting point for the request
	 * @return
	 */
	public static FederatedQueryInterface createForQueriesWithGivenEndpoints(String fedEngineUrl) {
		return create(fedEngineUrl, null);
	}
	
	/**
	 * Executes a SPARQL 1.1 query with and without specified endpoints. 
	 * If data sources should be selected automatically, 
	 * the query must contain SERVICE clauses with service variables but without VALUES clauses. 
	 * As an examples, see the query in {@link #createForQueriesWithGivenEndpoints(String)} without the VALUES. 
	 * <p>
	 * The code selects automatically the relevant data sources for the basic triple patterns within a SERVICE clause 
	 * and adds a VALUES clause with corresponding endpoint URLs for each SERVICE clause. 
	 * Finally, it executes the completed SPARQL 1.1 query as in {@link #createForQueriesWithGivenEndpoints(String)}.
	 * 
	 * @param fedEngineUrl endpoint URL as starting point for the request
	 * @param servDescrIndexer contains the schema-level index for all federation members to allow automated data source selection
	 * @param host2host if not null, host2host is used to convert host addresses; 
	 * conversion may necessary e.g. in a test environment with Docker containers where Blazegraph servers return endpoint URLs
	 * with wrong host addresses (such as "localhost:8080")  
	 * @return
	 */
	public static FederatedQueryInterface createWithEndpointSelection(
			String fedEngineUrl, ServiceDescriptionIndexer servDescrIndexer, Map<String, String> host2host) {
		
		DataSourceSelector selector =  new DataSourceSelector(servDescrIndexer, host2host);
		return create(fedEngineUrl, selector);
	}
	
	/**
	 * A wrapper for four engine configurations:<br> 
	 * (1) Execution on RDFJ server (not FedX):<br>
	 * (1a) SPARQL 1.1 with specified endpoints<br> 
	 * (1b) SPARQL 1.1 with SERVICE clause without specified endpoints and automated data source selection<br>
	 * (2) Execution on Blazegraph server:<br>
	 * (2a) SPARQL 1.1 with specified endpoints<br> 
	 * (2b) SPARQL 1.1 with SERVICE clause without specified endpoints and automated data source selection<br>
	 * 
	 * @param fedEngineUrl 
	 * @param selector
	 * @return
	 */
	private static FederatedQueryInterface create(String fedEngineUrl, DataSourceSelector selector) {
		
		boolean rdf4j = fedEngineUrl.contains("rdf4j"); 
		final RemoteStoreClient client = rdf4j? new RemoteStoreClient(fedEngineUrl) : null;
		final BlazegraphRepositoryWrapper wrapper = rdf4j? null : new BlazegraphRepositoryWrapper(fedEngineUrl);
		
		FederatedQueryInterface impl = new FederatedQueryInterface() {
			
			@Override
			public String executeFederatedQuery(String sparql)  {
				
				LOGGER.debug("executeQuery=\n" + sparql);
				
				if (selector != null) {
					sparql = selector.addValuesClauses(sparql);
					LOGGER.debug("executeQuery after data source selection=\n" + sparql);
				}
				
				if (wrapper != null) {
					return wrapper.query(sparql, null);
				} 
				return client.executeQuery(sparql).toString();
			}
		};
		
		return impl;
	}
	
	/**
	 * Creates a schema-level indexer which allows automated data source selection 
	 * for federated queries without specifying endpoints.
	 * All endpoint URLs are added to the set of federation members. 
	 * For each given service URL, all its existing datasets (endpoint URLs) are requested and added to the set of federation members.
	 * The service descriptions of all federation members are collected and used for index creation.
	 * Each federation member must implement the recommendation on
	 * <a href="https://www.w3.org/TR/2013/REC-sparql11-service-description-20130321/">service descriptions</a>.
	 * Currently, this is the case for Blazegraph server but not for RDF4J server
	 * 
	 * @param cached if true and the indexer was created before, the same indexer is returned;
	 * otherwise an indexer is created from scratch 
	 * @param serviceUrls
	 * @param endpointUrls
	 * @return
	 */
	public static ServiceDescriptionIndexer getIndexer(boolean cached, List<String> serviceUrls, List<String> endpointUrls) {
		if (cached && (indexer != null)) {
			return indexer;
		}
		
		// collect all endpoint Urls
		List<String> allEndpoints = new ArrayList<String>();
		if (serviceUrls != null) {
			for (String serviceUrl : serviceUrls) {
				BlazegraphRepositoryWrapper wrapper = new BlazegraphRepositoryWrapper(serviceUrl);
				List<String> endpoints = wrapper.queryNamespaces(true);
				allEndpoints.addAll(endpoints);
			}
		}
		if (endpointUrls != null) {
			allEndpoints.addAll(endpointUrls);
		}
		
		if (allEndpoints.isEmpty()) {
			throw new JPSRuntimeException("At least one endpoint is required, serviceUrls=" + serviceUrls + ", endpointUrls=" + endpointUrls);
		}
		
		// read all service descriptions
		indexer = new ServiceDescriptionIndexer();
		for (String url : allEndpoints) {
			indexer.addServiceDescription(url);
		}
		
		return indexer;
	}
}
