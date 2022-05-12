package uk.ac.cam.cares.jps.base.router;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.cache.LRUCache;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class is designed to get the URL of a requested agent from the ontoagent triple store:
 * <pre>
 * AgentRouter.getInstance().get(agent_name)
 * </pre>
 * This class extends the {@link uk.ac.cam.cares.jps.base.router.AbstractCachedRouter AbstractCachedRouter} 
 * class and uses a {@link uk.ac.cam.cares.jps.base.cache.LRUCache LRUCache} to cache results. 
 * If a result is not in the cache, the router will query the ontoagent triple store for the data.
 * <p>
 * Note: if multiple instances exist for a given agentID, only the first will be returned.
 * 
 * @author csl37
 *
 */
public class AgentRouter extends AbstractCachedRouter<String, String> {

	private static final Logger LOGGER = LogManager.getLogger(AgentRouter.class);
	
	private final static int CACHE_SIZE = 100;
	
	private String agentRouterEndpoint;
	
	//Variables for sparql query
	final static Var VAR_S = Var.alloc("s");
	final static String STR_O = "o";
	final static Var VAR_O = Var.alloc(STR_O);
	final static String MSM_HAS_HTTP_URL = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>";
	final static String MSM_OPERATION = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>";
	final static String RDF_TYPE = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";

	//Singleton
	private static AgentRouter instance = null;
	
	private AgentRouter() {
		//Instantiate router with LRUcache 
		super(new LRUCache<String,String>(CACHE_SIZE));
		//Get default ontoagent endpoint from jps.properties file
		agentRouterEndpoint = KeyValueMap.getInstance().get(IKeys.URL_AGENTROUTER_ENDPOINT);
		LOGGER.info("Agent router instantiated with router endpoint: "+agentRouterEndpoint);
	}
	
	public static synchronized AgentRouter getInstance() {
		if (instance == null) {
			instance = new AgentRouter();
		}
		return instance;	
	}
	
	/**
	 * Change the AgentRouter endpoint
	 * @param endpoint
	 */
	public void setRouterEndpoint(String endpoint) {
		agentRouterEndpoint = endpoint;
		LOGGER.info("Agent router endpoint set to: "+agentRouterEndpoint);
	}
	
	/**
	 * Get the URL of an agent matching agentID from the store client
	 * Note: if multiple instances exist for a given agentID, only the first will be returned.
	 * @param agentID
	 * @return url
	 */
	@Override
	public String getFromStore(String agentID, StoreClientInterface storeClient) {
		
		LOGGER.debug("Get URL from triple store. AgentID="+agentID);
		
		String query = getQuery(agentID);
		
		JSONArray result = storeClient.executeQuery(query);
		
		//TODO add logic for multiple results
		if(!result.isEmpty()) {
			String firstURL = result.getJSONObject(0).getString(STR_O);
			LOGGER.debug("URL="+firstURL);
			return firstURL;
		}else {
			LOGGER.info("URL not found for AgentID="+agentID);
			return null;
		}
	}
	
	@Override
	public StoreClientInterface getStoreClient() {
		return new RemoteStoreClient(agentRouterEndpoint);
	}
	
	/**
	 * Build SPARQL query to get URL for given agent
	 * The query gets objects with the ontoagent predicate hasHttpUrl
	 * @param agentName
	 * @return
	 */
	private String getQuery(String agentName) {

		// 	SELECT ?o
		// 	WHERE {
		//		?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>.
		//		?s <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl> ?o.
		//		FILTER(CONTAINS(STR(?s), agentName))
		//	}
		
		// match agent name in rdf subject
		ExprFactory exprFactory = new ExprFactory();
		ExprVar exprS = new ExprVar(VAR_S);
		Expr exprMatch = exprFactory.asExpr(agentName);
		Expr SContains = exprFactory.contains(exprFactory.str(exprS), exprMatch);
		
		WhereBuilder where = new WhereBuilder()
				.addWhere(VAR_S, RDF_TYPE, MSM_OPERATION)
				.addWhere(VAR_S, MSM_HAS_HTTP_URL, VAR_O)
				.addFilter(SContains);
		
		SelectBuilder select = new SelectBuilder()
				.addVar(VAR_O)
				.addWhere(where);
		
		return select.buildString();
	}
}
