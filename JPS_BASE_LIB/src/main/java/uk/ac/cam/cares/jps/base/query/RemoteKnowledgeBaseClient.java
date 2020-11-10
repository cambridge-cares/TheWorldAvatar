package uk.ac.cam.cares.jps.base.query;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import org.apache.jena.jdbc.JenaDriver;
import org.apache.jena.jdbc.remote.RemoteEndpointDriver;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

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
 * RDF4J query end point URL: http://localhost:8080/rdf4j-server/repositories/ontospecieskb
 * <p>
 * Note that this is for the repository called ontospecieskb.<p>
 * <p>
 * Namespace in Blazegraph and repository in RDF4J refer to the same thing.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk), Casper Lindberg
 *
 */
public class RemoteKnowledgeBaseClient extends KnowledgeBaseClient {

	private static final String HTTP_PROTOCOL_PREFIX = "http:";

	private String queryEndpoint;
	private String updateEndpoint;
	private String query;
	
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
	 * A constructor defined to initialise the query EndPoint URL, update<p>
	 * EndPoint URL and a set of graphs to send a data retrieval or update<p>
	 * query.    
	 * 
	 * @param queryEndpoint
	 * @param updateEndpoint
	 * @param query
	 */
	public RemoteKnowledgeBaseClient(String queryEndpoint, String updateEndpoint, String query){
		this.query = query; // query variable in super class
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
	public int executeUpdate() throws SQLException{
		String connectionUrl = getConnectionUrl();
		if(connectionUrl.isEmpty()){
			throw new SQLException("KnowledgeBaseClient: connection URL for the update operation is empty.");
		}
		if(isConnectionUpdateUrlValid(connectionUrl)){
			return executeUpdate(this.query);
		}else{
			throw new SQLException("KnowledgeBaseClient: connection URL for the update operation is not valid.");
		}
	}
	
	/**
	 * Executes the update operation supplied by the calling method and returns results.
	 * 
	 * @param update as UpdateRequest
	 * @return
	 */
	@Override
	public int executeUpdate(UpdateRequest update) throws SQLException {
		return executeUpdate(update.toString());
	}
	
	/**
	 * Executes the update operation supplied by the calling method and returns results.
	 * 
	 * @param query
	 * @return
	 */
	@Override
	public int executeUpdate(String query) throws SQLException {
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
			throw new SQLException(e.getMessage());
		}
	}
	
	/**
	 * Execute sparql query using the query variable
	 * 
	 * @return JSONArray as String 
	 * @throws SQLException
	 */
	public String execute() throws SQLException{
		return execute(this.query);
	}
	
	/**
	 * Excute sparql query
	 * 
	 * @param sparql
	 * @return JSONArray as String
	 * @throws SQLException
	 */
	public String execute(String query) throws SQLException{
		JSONArray result = executeQuery(query);
		if(result==null){
			throw new SQLException();
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
	public JSONArray executeQuery() throws SQLException{
		String connectionUrl = getConnectionUrl();
		if(connectionUrl.isEmpty()){
			throw new SQLException("KnowledgeBaseClient: the URL to connect to the endpoint is empty");
		}
		if(isConnectionQueryUrlValid(connectionUrl)){
			return executeQuery(this.query);
		}else{
			throw new SQLException("KnowledgeBaseClient: the URL to connect to the endpoint is not valid");
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
	public JSONArray executeQuery(String query) throws SQLException {
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
			throw new SQLException(e.getMessage());
		}
		return results;
	}
	
	/**
	 * Generates the URL of the remote data repository's EndPoint either to<p>
	 * perform a data retrieval or an update query.  
	 * 
	 * @return
	 */
	public String getConnectionUrl() {
		StringBuilder sb = new StringBuilder();
		boolean queryFlag = false;
		sb.append(JenaDriver.DRIVER_PREFIX);
		sb.append(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX);
		if (this.queryEndpoint != null) {
			queryFlag = true;
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT, this.queryEndpoint));
		}
		if (this.updateEndpoint != null) {
			if(queryFlag){
				sb.append("&");
			}
			sb.append(generateEndpointProperty(RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT, this.updateEndpoint));
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
		if (!connectionUrl.startsWith(JenaDriver.DRIVER_PREFIX
				.concat(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX)
				.concat(RemoteEndpointDriver.PARAM_QUERY_ENDPOINT)
				.concat("=")
				.concat(HTTP_PROTOCOL_PREFIX))) {
			return false;
		}
		return isConnectionUrlValid(connectionUrl);
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
	private boolean isConnectionUpdateUrlValid(String connectionUrl){
		if (!(isConnectionQueryUrlValid(connectionUrl) || connectionUrl.startsWith(JenaDriver.DRIVER_PREFIX
						.concat(RemoteEndpointDriver.REMOTE_DRIVER_PREFIX)
						.concat(RemoteEndpointDriver.PARAM_UPDATE_ENDPOINT)
						.concat("=")
						.concat(HTTP_PROTOCOL_PREFIX)))) {
			return false;
		}
		return isConnectionUrlValid(connectionUrl);
	}
	
	private boolean isConnectionUrlValid(String connectionUrl){
		String[] tokens = connectionUrl.split(HTTP_PROTOCOL_PREFIX);
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
}