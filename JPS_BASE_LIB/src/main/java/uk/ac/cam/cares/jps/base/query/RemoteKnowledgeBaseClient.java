package uk.ac.cam.cares.jps.base.query;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;

import org.apache.jena.jdbc.JenaDriver;
import org.apache.jena.jdbc.remote.RemoteEndpointDriver;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionRemote;
import org.apache.jena.rdfconnection.RDFConnectionRemoteBuilder;
import org.apache.jena.update.UpdateRequest;
import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * This class allows to establish connection with remote knowledge repositories<p>
 * to perform SPARQL query and update operations. It supports many triple stores<p>
 * such as Blazegraph and RDF4J. It requires to set the end point URL for the<p>
 * intended type of operation. See some example end point URLS:<p>
 * <p>
 * Blazegraph query end point URL: http://localhost:8080/blazegraph/namespace/kb/sparql
 * <p>
 * Note that this is for the namespace called "kb". If you have a different namespace,<p>
 * e.g, "ontokin", replace "kb" with "ontokin" in the above URL<p>
 * <p>
 * RDF4J query end point URL: http://localhost:8080/rdf4j-server/repositories/ontospecies
 * <p>
 * Note that this is for the repository called ontospecies.<p>
 * <p>
 * Namespace in Blazegraph and repository in RDF4J refer to the same thing.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class RemoteKnowledgeBaseClient implements KnowledgeBaseClientInterface {

	private static final String HTTP_PROTOCOL= "http:";
	private static final String HTTPS_PROTOCOL = "https:";

	private String queryEndpoint;
	private String updateEndpoint;
	private String query;
	// Authentication properties
	private String userName;
	private String password;
	
	///////////////////////////
	// Constructors
	///////////////////////////
	
	/**
	 * The default constructor.
	 */
	public RemoteKnowledgeBaseClient(){
		
	}
	
	/**
	 * A constructor defined to initialise only the query EndPoint URL.
	 * 
	 * @param queryEndpoint
	 */
	public RemoteKnowledgeBaseClient(String queryEndpoint){
		this.queryEndpoint = queryEndpoint;
	}
	
	/**
	 * A constructor defined to initialise the query EndPoint URL, update<p>
	 * EndPoint URL and a set of graphs to send a data retrieval or update<p>
	 * query.    
	 * 
	 * @param queryEndpoint
	 * @param updateEndpoint
	 */
	public RemoteKnowledgeBaseClient(String queryEndpoint, String updateEndpoint){
		this.queryEndpoint = queryEndpoint;
		this.updateEndpoint = updateEndpoint;
	}
	
	/**
	 * A constructor defined to initialise the query EndPoint URL, user name and password.
	 * 
	 * @param queryEndpoint
	 * @param user
	 * @param password
	 */
	public RemoteKnowledgeBaseClient(String queryEndpoint, String user, String password){
		this.queryEndpoint = queryEndpoint;
	}
	
	/**
	 * A constructor defined to initialise the query EndPoint URL, update<p>
	 * EndPoint URL, user name and password.    
	 * 
	 * @param queryEndpoint
	 * @param updateEndpoint
	 * @param user
	 * @param password
	 */
	public RemoteKnowledgeBaseClient(String queryEndpoint, String updateEndpoint, String user, String password){
		this.queryEndpoint = queryEndpoint;
		this.updateEndpoint = updateEndpoint;
	}
	
	/**
	 * A constructor defined to initialise the query EndPoint URL, update<p>
	 * EndPoint URL, user name and password and a data retrieval or update<p>
	 * query.
	 * 
	 * @param queryEndpoint
	 * @param updateEndpoint
	 * @param query
	 * @param user
	 * @param password 
	 */
	public RemoteKnowledgeBaseClient(String queryEndpoint, String updateEndpoint, String query, String user, String password){
		this.query = query;
		this.queryEndpoint = queryEndpoint;
		this.updateEndpoint = updateEndpoint;
	}
	
	///////////////////////////
	// Read and write methods
	///////////////////////////

	public void load() {
		// do nothing
		// connection established during query/update execution
	}
	
	public void end() {
		// do nothing
	}
		
	///////////////////////////
	// Read and write methods
	///////////////////////////

	/**
	 * Returns the available query.
	 * 
	 * @return
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * Sets a query if provided.
	 * 
	 * @param query
	 * @return
	 */
	public String setQuery(String query) {
		this.query = query;
		return this.query;
	}
	
	/**
	 * Can return the URL of the query EndPoint.  
	 * 
	 * @return
	 */
	public String getQueryEndpoint() {
		return queryEndpoint;
	}

	/**
	 * Sets the URL of the query EndPoint if provided. 
	 * 
	 * @param queryEndpoint
	 * @return
	 */
	public String setQueryEndpoint(String queryEndpoint) {
		this.queryEndpoint = queryEndpoint;
		return this.queryEndpoint;
	}
	
	/**
	 * Returns the URL of the update EndPoint if available.
	 * 
	 * @return
	 */
	public String getUpdateEndpoint() {
		return updateEndpoint;
	}
	
	/**
	 * Set the URL of the update EndPoint if provided.
	 * 
	 * @param updateEndpoint
	 * @return
	 */
	public String setUpdateEndpoint(String updateEndpoint) {
		this.updateEndpoint = updateEndpoint;
		return this.updateEndpoint;
	}
	
	/**
	 * Returns the user name to access the current EndPoint.
	 * 
	 * @return
	 */
	public String getUser() {
		return userName;
	}

	/**
	 * Sets the user name to access the current EndPoint.
	 * 
	 * @param userName
	 */
	public void setUser(String userName) {
		this.userName = userName;
	}
	
	/**
	 * Returns the user password to access the current EndPoint.
	 * 
	 * @return
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * Sets the user password to access the current EndPoint.
	 * 
	 * @param password
	 */
	public void setPassword(String password) {
		this.password = password;
	}
	
	///////////////////////////
	// Sparql query and update
	///////////////////////////
	
	/**
	 * Executes the update operation that is provided through the constructors or setter<p>
	 * method.
	 * 
	 * @return
	 */
	@Override
	public int executeUpdate(){
		String connectionUrl = getConnectionUrl();
		if(connectionUrl.isEmpty()){
			throw new JPSRuntimeException("KnowledgeBaseClient: connection URL for the update operation is empty.");
		}
		if(isConnectionUpdateUrlValid(connectionUrl)){
			return executeUpdate(this.query);
		}else{
			throw new JPSRuntimeException("KnowledgeBaseClient: connection URL for the update operation is not valid.");
		}
	}
	
	/**
	 * Executes the update operation supplied by the calling method and returns results.
	 * 
	 * @param update as UpdateRequest
	 * @return
	 */
	@Override
	public int executeUpdate(UpdateRequest update) {
		return executeUpdate(update.toString());
	}
	
	/**
	 * Executes the update operation supplied by the calling method and returns results.
	 * 
	 * @param query
	 * @return
	 */
	@Override
	public int executeUpdate(String query) {
		Connection conn = null;
		Statement stmt = null;
		try {
			RemoteEndpointDriver.register();
			System.out.println(getConnectionUrl());
			conn = DriverManager.getConnection(getConnectionUrl());
			stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY);
			System.out.println(query);
			return stmt.executeUpdate(query);
		} catch (SQLException e) {
			throw new JPSRuntimeException(e.getMessage(),e);
		}
	}
	
	/**
	 * Execute sparql query using the query variable
	 * 
	 * @return JSONArray as String 
	 */
	public String execute(){
		return execute(this.query);
	}
	
	/**
	 * Excute sparql query
	 * 
	 * @param sparql
	 * @return JSONArray as String
	 */
	public String execute(String query){
		JSONArray result = executeQuery(query);
		if(result==null){
			throw new JPSRuntimeException("KnowledgeBaseClient: sparql query result is null.");
		}else{
			return result.toString();
		}
	}
	
	/**
	 * Executes the query that is provided through the constructors or setter<p>
	 * method.
	 * 
	 * @return
	 */
	@Override
	public JSONArray executeQuery(){
		String connectionUrl = getConnectionUrl();
		if(connectionUrl.isEmpty()){
			throw new JPSRuntimeException("KnowledgeBaseClient: the URL to connect to the endpoint is empty");
		}
		if(isConnectionQueryUrlValid(connectionUrl)){
			return executeQuery(this.query);
		}else{
			throw new JPSRuntimeException("KnowledgeBaseClient: the URL to connect to the endpoint is not valid");
		}
	}
	
	/**
	 * Executes the query supplied by the calling method and returns results<p>
	 * as a JSONArray.
	 * 
	 * @param query
	 * @return
	 */
	@Override
	public JSONArray executeQuery(String query) {
		JSONArray results = new JSONArray();
		Connection conn = null;
		Statement stmt = null;
		try {
			RemoteEndpointDriver.register();
			System.out.println(getConnectionUrl());
			conn = DriverManager.getConnection(getConnectionUrl());
			stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY);
			System.out.println(query);
			java.sql.ResultSet rs = stmt.executeQuery(query);
			results = convert(rs);
		} catch (SQLException e) {
			throw new JPSRuntimeException(e.getMessage(),e);
		}
		return results;
	}
		
	/**
	 * Perform a sparql construct query
	 * @return RDF model
	 */
	@Override
	public Model executeConstruct(Query sparql) {
		return executeConstruct(sparql.toString());
	}
	
	/**
	 * Perform a sparql construct query
	 * @return RDF model
	 */
	@Override
	public Model executeConstruct(String sparql) {
		
		RDFConnection conn = connectQuery();
		
		if (conn != null) {
			conn.begin( TxnType.READ );	
			try {
				return conn.queryConstruct(sparql);
			} finally {
				conn.end();
			}
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}
	
	/**
	 * Return RDFConnection to sparql query endpoint
	 * @return
	 */
	private RDFConnection connectQuery() {
		
		RDFConnectionRemoteBuilder builder = null;
		if(queryEndpoint != null) {
			builder = RDFConnectionRemote.create().destination(queryEndpoint);
		}else {
			throw new JPSRuntimeException("RemoteKnowledgeBaseClient: update endpoint not specified.");
		}
		
		return builder.build();
	}
	
	/**
	 * Generates the URL of the remote data repository's EndPoint, which<br>
	 * might require authentication either to perform a data retrieval or<br>
	 * an update query.  
	 * 
	 * @return
	 */
	public String getConnectionUrl() {
		StringBuilder sb = new StringBuilder();
		boolean queryFlag = false;
		boolean updateFlag = false;
		sb.append(JenaDriver.DRIVER_PREFIX);
		sb.append(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX);
		if (this.queryEndpoint != null) {
			queryFlag = true;
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT, this.queryEndpoint));
		}
		if (this.updateEndpoint != null) {
			updateFlag = true;
			if(queryFlag){
				sb.append("&");
			}
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT, this.updateEndpoint));
		}
		if (this.userName != null) {
			if(queryFlag || updateFlag){
				sb.append("&");
			}
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_USERNAME, this.userName));
		}
		if (this.password != null) {
			if(queryFlag || updateFlag){
				sb.append("&");
			}
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_PASSWORD, this.password));
		}
		return sb.toString();
	}

	/**
	 * Puts the type of an endpoint (e.g. query and update), equal to symbol<p>
	 * and end point URL in a string and returns the string.
	 * 
	 * @param endpointType
	 * @param endpointURL
	 * @return
	 */
	private String generateEndpointProperty(String endpointType, String endpointURL){
		StringBuilder sb = new StringBuilder();
		sb.append(endpointType);
		sb.append("=");
		sb.append(endpointURL);
		return sb.toString();
	}	
	
	/**
	 * Converts query results into JSON.
	 * 
	 * @param rs
	 * @return results in JSON format
	 * @throws SQLException
	 * @throws JSONException
	 */
	private JSONArray convert(java.sql.ResultSet rs) throws SQLException, JSONException {
		JSONArray json = new JSONArray();
		java.sql.ResultSetMetaData rsmd = rs.getMetaData();
		while (rs.next()) {
			int numColumns = rsmd.getColumnCount();
			JSONObject obj = new JSONObject();
			for (int i = 1; i < numColumns + 1; i++) {
				String column_name = rsmd.getColumnName(i);
				if (rsmd.getColumnType(i) == java.sql.Types.ARRAY) {
					obj.put(column_name, rs.getArray(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.BIGINT) {
					obj.put(column_name, rs.getInt(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.BOOLEAN) {
					obj.put(column_name, rs.getBoolean(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.BLOB) {
					obj.put(column_name, rs.getBlob(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.DOUBLE) {
					obj.put(column_name, rs.getDouble(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.FLOAT) {
					obj.put(column_name, rs.getFloat(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.INTEGER) {
					obj.put(column_name, rs.getInt(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.NVARCHAR) {
					obj.put(column_name, rs.getNString(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.VARCHAR) {
					obj.put(column_name, rs.getString(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.TINYINT) {
					obj.put(column_name, rs.getInt(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.SMALLINT) {
					obj.put(column_name, rs.getInt(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.DATE) {
					obj.put(column_name, rs.getDate(column_name));
				} else if (rsmd.getColumnType(i) == java.sql.Types.TIMESTAMP) {
					obj.put(column_name, rs.getTimestamp(column_name));
				} else {
					obj.put(column_name, rs.getObject(column_name));
				}
			}
			json.put(obj);
		}
		return json;
	}

	/**
	 * Checks the validity of the URL generated for connecting to a remote<p>
	 * repository based on user provided inputs via one of the parameterised<p>
	 * constructors or setter methods.
	 * 
	 * @param connectionUrl provided URL that is to be used for establishing<p>
	 * a connection with the remote repository to perform a query operation<p>
	 * @return
	 */
	public boolean isConnectionQueryUrlValid(String connectionUrl){
		if (connectionUrl.startsWith(getQueryEndpointConnectionPrfixes()
				.concat(HTTP_PROTOCOL)) 
				|| connectionUrl.startsWith(getQueryEndpointConnectionPrfixes()
						.concat(HTTPS_PROTOCOL))) {
			return isConnectionUrlValid(connectionUrl);
		}
		return false;
	}
	
	/**
	 * Checks the validity of the URL generated for connecting to a remote<p>
	 * repository based on user provided inputs via one of the parameterised<p>
	 * constructors or setter methods.
	 *  
	 * @param connectionUrl provided URL that is to be used for establishing<p>
	 * a connection with the remote repository to perform an update operation<p>
	 * @return
	 */
	public boolean isConnectionUpdateUrlValid(String connectionUrl) {
		// One might set both the query and update endpoint URLs, but calls
		// the executeUpdate() method. This is why it is crucial to check
		// if the provided update URL is correctly encoded.
		if (isConnectionQueryUrlValid(connectionUrl)) {
			if (connectionUrl.contains(getUpdateEndpointConnectionParameter().concat(HTTP_PROTOCOL))
					|| connectionUrl.contains(
							getUpdateEndpointConnectionParameter().concat(HTTPS_PROTOCOL))) {
				return true;
			}
		}
		if (connectionUrl.startsWith(getUpdateEndpointConnectionPrefixes().concat(HTTP_PROTOCOL))
				|| connectionUrl
						.startsWith(getUpdateEndpointConnectionPrefixes().concat(HTTPS_PROTOCOL))) {
			return isConnectionUrlValid(connectionUrl);
		}
		return false;
	}
	
	/**
	 * Checks the validity of a connection URL.
	 * 
	 * @param connectionUrl
	 * @return
	 */
	private boolean isConnectionUrlValid(String connectionUrl){
		String[] tokens = new String[]{""};
		if(connectionUrl.contains(HTTP_PROTOCOL)){
			tokens = connectionUrl.split(HTTP_PROTOCOL);			
		} else if(connectionUrl.contains(HTTPS_PROTOCOL)){
			tokens = connectionUrl.split(HTTPS_PROTOCOL);
		}
		for(String token: tokens){
			if(token.isEmpty()){
				return false;
			}
			if(token.length()<3){
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Returns the connection prefix and query endpoint parameter literals concatenated.
	 * 
	 * @return
	 */
	private String getQueryEndpointConnectionPrfixes(){
		return JenaDriver.DRIVER_PREFIX
				.concat(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX)
				.concat(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT)
				.concat("=");
	}
	
	/**
	 * Returns the connection prefix and update endpoint parameter literals concatenated.
	 * 
	 * @return
	 */
	private String getUpdateEndpointConnectionPrefixes(){
		return JenaDriver.DRIVER_PREFIX
				.concat(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX)
				.concat(RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT)
				.concat("=");
	}
	
	/**
	 * Returns the update endpoint parameter.
	 * 
	 * @return
	 */
	private String getUpdateEndpointConnectionParameter(){
		return RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT
				.concat("=");
	}
	
	/**
	 * If a list of SPARQL endpoints and a query are passed to this method, it evaluates<br>
	 * the query against all the endpoints and returns the result in JSON format.<br>
	 * <br>
	 * Endpoints should be provided as shown in the following two examples:<br>
	 * 1. OntoSpecies KB endpoint: http://www.theworldavatar.com/blazegraph/namespace/ontospecies/sparql
	 * 2. OntoCompChem KB endpoint: http://www.theworldavatar.com/blazegraph/namespace/ontocompchem/sparql
	 * 
	 * @param endpoints a list of endpoints.
	 * @param query a SPARQL query.
	 * @return
	 * @throws Exception
	 */
	public JSONArray executeFederatedQuery(List<String> endpoints, String query) throws Exception {
		// Declares a JSONArray and JSONObject to produce the query output in JSON format. 
		JSONArray json = new JSONArray();
		JSONObject obj;
		BindingSet bSet;
		// Creates a federation with all provided endpoints.
		Repository repository = FedXFactory.createSparqlFederation(endpoints);
		try {
			// Establishes a connection with all the endpoints.
			RepositoryConnection conn = repository.getConnection();
			// Prepares the query for execution against all the endpoints.
			TupleQuery tq = conn.prepareTupleQuery(query);
			try {
				// Evaluates the query against all the endpoints.
				TupleQueryResult tqRes = tq.evaluate();
				// Processes the result 
				while (tqRes.hasNext()) {
					obj = new JSONObject();
					bSet = tqRes.next();
					for (String bindingName : bSet.getBindingNames()) {
						// If the value of a variable provided in the SPARQL query is found
						// within double quotes, they are removed as the JSON Object also adds
						// double quotes to the value.
						if (bSet.getValue(bindingName).toString().startsWith("\"")
								&& bSet.getValue(bindingName).toString().endsWith("\"")) {
							obj.put(bindingName, bSet.getValue(bindingName).toString().substring(1,
									bSet.getValue(bindingName).toString().length() - 1));
						} 
						// A value found without double quotes is codified as a JSON Object
					    // without modification.
						else { 
							obj.put(bindingName, bSet.getValue(bindingName).toString());

						}
					}
					json.put(obj);
				}
			} catch (TupleQueryResultHandlerException e) {
				e.printStackTrace();
			}
			conn.close();
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		repository.shutDown();
		return json;
	}
}