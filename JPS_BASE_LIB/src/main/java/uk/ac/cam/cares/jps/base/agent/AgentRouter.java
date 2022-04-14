package uk.ac.cam.cares.jps.base.agent;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.expr.Expr;
import org.apache.jena.sparql.expr.ExprVar;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class is designed to get the URL of a requested agent
 * from the ontoagentrouter triple store.
 * 
 * @author CLIN01
 *
 */
public class AgentRouter {
	
	private static final Logger LOGGER = LogManager.getLogger(AgentRouter.class);

	String AGENTROUTER_ENDPOINT;
	
	//Variables for sparql query
	final Var varS = Var.alloc("s");
	final String strO = "o";
	final Var varO = Var.alloc(strO);
	final String MSMhasHttpUrl = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl>";
	final String MSMOperation = "<http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation>";
	final String rdfType = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>";

	public AgentRouter() {
		// Default endpoint from jps.properties file
		AGENTROUTER_ENDPOINT = KeyValueMap.getInstance().get(IKeys.URL_AGENTROUTER_ENDPOINT);
	}
	
	public AgentRouter(String endpoint) {
		AGENTROUTER_ENDPOINT = endpoint;
	}
	
	/**
	 * Get the URL of the agent matching agentID from the ontoagentrouter 
	 * @param agentID
	 * @return
	 */
	public String getUrl(String agentID) {

		StoreClientInterface storeClient = new RemoteStoreClient(AGENTROUTER_ENDPOINT);
		
		return getUrl(storeClient, agentID);
	}
	
	/**
	 * Get the URL of an agent matching agentID from the supplied store client
	 * @param storeClient
	 * @param agentID
	 * @return
	 */
	public String getUrl(StoreClientInterface storeClient, String agentID) {
	
		String query = getQuery(agentID);
		JSONArray result = storeClient.executeQuery(query);
		
		//TODO parse multiple results?
		String firstURL = result.getJSONObject(0).getString(strO);
		return firstURL;
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
		
		// match agent name in subject
		ExprFactory exprFactory = new ExprFactory();
		ExprVar exprS = new ExprVar(varS);
		Expr exprMatch = exprFactory.asExpr(agentName);
		Expr SContains = exprFactory.contains(exprFactory.str(exprS), exprMatch);
		
		WhereBuilder where = new WhereBuilder()
				.addWhere(varS, rdfType, MSMOperation)
				.addWhere(varS, MSMhasHttpUrl, varO)
				.addFilter(SContains);
		
		SelectBuilder select = new SelectBuilder()
				.addVar(varO)
				.addWhere(where);
		
		return select.buildString();
	}
}
