package uk.ac.cam.cares.jps.base.query;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.Iterator;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.update.UpdateRequest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

/**
 * Local in-memory implementation of the {@link uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface TripleStoreClientInterface},
 * designed to serve as a temporary store.
 * 
 * This class uses {@link org.apache.jena.rdfconnection.RDFConnection RDFConnection} to provide SPARQL access to an in-memory dataset. 
 * 
 * @see uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface TripleStoreClientInterface
 * @see org.apache.jena.rdfconnection.RDFConnection RDFConnection
 * 
 * @author csl37
 *
 */
public class LocalStoreClient implements TripleStoreClientInterface {

	private static Logger LOGGER = LogManager.getLogger(LocalStoreClient.class);
	
	protected Dataset dataset;
	protected RDFConnection conn;

	protected String query;
	
	public LocalStoreClient() {
		init();
		LOGGER.info("LocalStoreClient instantiated.");
	}
	
	public LocalStoreClient(String query) {
		init();
		this.query = query;
		LOGGER.info("LocalStoreClient instantiated with query.");
	}
	
	/**
	 * Initialize Dataset and RDFConnection 
	 */
	protected void init() {
		dataset = DatasetFactory.create();
		conn = RDFConnection.connect(dataset);
	}
	
	/**
	 * Checks the connection is active.
	 * @return
	 */
	public boolean isConnected() {
		return !conn.isClosed();
	}
	
	/**
	 * Checks the model contains data.
	 * @return
	 */
	public boolean isEmpty() {
		
		if(!isConnected()) {
			return true;
		}else {
			return dataset.isEmpty();
		}
	}	
	
	///////////////////////////
	// Sparql query and update
	///////////////////////////
	
	/**
	 * Executes the update operation using update supplied
	 * through the constructor or setter methods.
	 * 
	 * @param
	 * @return
	 */
	@Override
	public int executeUpdate() {
		LOGGER.info("SPARQL update with query variable.");
		return executeUpdate(this.query);
	}

	/**
	 * Executes the update operation supplied by the calling method.
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate(String update) {
		try {
			LOGGER.info("Executing SPARQL update.");
			conn.begin( TxnType.WRITE );
			conn.update(update);
			conn.commit();
		} finally {
			conn.end();
		}
		return 0;
	}
	
	/**
	 * Executes the update operation supplied by the calling method.
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate(UpdateRequest update) {
		LOGGER.info("SPARQL update with argument.");
		return executeUpdate(update.toString());		
	}

	/**
	 * Execute sparql query using the query variable
	 * set by the setter method
	 * 
	 * @return JSONArray as String 
	 */
	@Override
	public String execute(){
		LOGGER.info("SPARQL query with query variable.");
		return execute(this.query);
	}

	/**
	 * Excute sparql query supplied as argument
	 * 
	 * @param sparql
	 * @return JSONArray as String
	 */
	@Override
	public String execute(String query){
		LOGGER.info("SPARQL query with query as argument.");
		JSONArray result = executeQuery(query);
		return result.toString();
	}

	/**
	 * Excute sparql query supplied as argument
	 * 
	 * @param sparql
	 * @return JSONArray
	 */
	@Override
	public JSONArray executeQuery(String sparql) {
		LOGGER.info("SPARQL query with query as argument.");
		
		Query query = QueryFactory.create(sparql);
		if(query.isAskType()) {
			//
			JSONObject obj = new JSONObject();
			obj.put(JPSConstants.ASK_RESULT_KEY, executeAsk(sparql));
			
			JSONArray result = new JSONArray();
			result.put(obj);
			
			return result;
		}else {
			ResultSet results = executeSelect(sparql);
			return convert(results);
		}
	}	

	/**
	 * Executes the query supplied by the calling method and returns results
	 * as a JSONArray.
	 */
	@Override
	public JSONArray executeQuery() {
		LOGGER.info("SPARQL query with query variable.");
		return executeQuery(this.query);
	}

	/**
	 * Performs ASK query execution
	 * @param sparql
	 * @return
	 */
	protected boolean executeAsk(String sparql) {
		try {
			LOGGER.info("Executing SPARQL ASK query.");
			conn.begin( TxnType.READ );
			QueryExecution queryExec = conn.query(sparql);	
			return queryExec.execAsk();
		} finally {
			conn.end();
		}	
	}
	
	/**
	 * Performs SELECT query execution
	 * @param sparql
	 */
	protected ResultSet executeSelect(String sparql) {		
		try {
			LOGGER.info("Executing SPARQL query.");
			conn.begin( TxnType.READ );
			QueryExecution queryExec = conn.query(sparql);
			ResultSet results = null;
			results = queryExec.execSelect();
			return results;
		} finally {
			conn.end();
		}	
	}

	/**
	 * Convert query results to JSONArray
	 */
	protected JSONArray convert(ResultSet resultSet) {
	
		JSONArray json = new JSONArray();
		
		while (resultSet.hasNext()) {
			QuerySolution qs = resultSet.next();
			JSONObject obj = new JSONObject();
			Iterator<String> it = qs.varNames(); 
			while(it.hasNext()) {
				String var = it.next(); 
				RDFNode node = qs.get(var);
				if(node.isLiteral()) {
					obj.put(var, node.asLiteral().getValue());	
				}else {
					obj.put(var, node);
				}
			}
			json.put(obj);
		}
		return json;
	}

	/**
	 * Execute sparql construct query
	 */
	@Override
	public Model executeConstruct(Query sparql) {
		return executeConstruct(sparql.toString());
	}

	/**
	 * Execute sparql construct query
	 */
	@Override
	public Model executeConstruct(String sparql) {
		try {
			LOGGER.info("Executing SPARQL construct query.");
			conn.begin( TxnType.READ );
			QueryExecution queryExec = conn.query(sparql);
			Model results = queryExec.execConstruct();
			return results;
		} finally {
			conn.end();
		}
	}
	
	/**
	 * Get rdf content from store.
	 * Performs a construct query on the store and returns the model as a string.
	 * @param graphName (if any)
	 * @param accept
	 * @return String
	 */
	@Override
	public String get(String resourceUrl, String accept) {
		
		LOGGER.info("Executing GET resourceURL="+resourceUrl);
		
		Var varS = Var.alloc("s");
		Var varP = Var.alloc("p");
		Var varO = Var.alloc("o");
		
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct( varS, varP, varO);
		
		if (resourceUrl == null) {
			//Default graph
			builder.addWhere(varS, varP, varO);
		}else {	
			//Named graph
			String graphURI = "<" + resourceUrl + ">";
			builder.addGraph(graphURI, varS, varP, varO);	
		}
		
		Model model = executeConstruct(builder.build());
		
		Lang syntax;
		if (accept != null) {
			syntax = RDFLanguages.contentTypeToLang(accept);
		}else {
			//default to application/rdf+xml
			syntax = Lang.RDFXML; 
		}
		LOGGER.info("GET accept lang="+syntax.toString());
		
		
		StringWriter out = new StringWriter();
		model.write(out, syntax.getName());
		return out.toString();
	}

	/**
	 * Insert rdf content into store. 
	 * @param graphName (if any)
	 * @param content
	 * @param contentType
	 */
	@Override
	public void insert(String graphName, String content, String contentType) {
		
		LOGGER.info("Executing INSERT graph="+graphName+" , contentType="+contentType);
		
		Model model = ModelFactory.createDefaultModel();
		
		InputStream in = new ByteArrayInputStream(content.getBytes());
		
		if (contentType == null) {
			LOGGER.info("Assuming default content type RDF/XML");
			//RDF/XML default
			//base=null, assume all uri are absolute
			model.read(in, null); 
		} else {
			Lang syntax = RDFLanguages.contentTypeToLang(contentType);
			LOGGER.debug("Content type: "+syntax.getName());
			model.read(in,null,syntax.getName());
		}
		
		UpdateBuilder builder = new UpdateBuilder();
		
		if (graphName == null) {
			LOGGER.debug("Create insert for default graph");
			builder.addInsert(model);
		} else {
			LOGGER.debug("Create insert for namde graph: "+graphName);
			String graphURI = "<" + graphName + ">";
			builder.addInsert(graphURI, model);
		}
		
		executeUpdate(builder.buildRequest());	
	}

	///////////////////////////
	// Get and Set methods
	///////////////////////////
	
	/**
	 * Set query variable
	 */
	@Override
	public String setQuery(String query) {
		this.query = query;
		return query;
	}

	/**
	 * Get query variable
	 */
	@Override
	public String getQuery() {
		return query;
	}

	@Override
	public String getQueryEndpoint() {
		return null;
	}

	@Override
	public String setQueryEndpoint(String queryEndpoint) {
		return null;
	}

	@Override
	public String getUpdateEndpoint() {
		return null;
	}

	@Override
	public String setUpdateEndpoint(String updateEndpoint) {
		return null;
	}

	@Override
	public String getUser() {
		return null;
	}

	@Override
	public void setUser(String userName) {
	}

	@Override
	public String getPassword() {
		return null;
	}

	@Override
	public void setPassword(String password) {
	}

	@Override
	public CloseableHttpResponse executeUpdateByPost(String update) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("Unimplemented method 'executeUpdateByPost'");
	}
}
