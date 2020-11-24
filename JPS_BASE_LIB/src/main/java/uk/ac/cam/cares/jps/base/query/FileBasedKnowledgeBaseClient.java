package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.sql.SQLException;
import java.util.Iterator;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.TxnType;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * File Based Knowledge Base Client. This class uses RDFConnection to load
 * and provide SPARQL access to file based datasets.
 * 
 * @author Casper Lindberg
 *
 */
public class FileBasedKnowledgeBaseClient extends KnowledgeBaseClient {

	private Dataset dataset;
	private RDFConnection conn;
	private String filePath;
	private String query;
	private Lang langOut = Lang.RDFXML; // RDFXML by default
	
	///////////////////////////
	// Constructors
	///////////////////////////
	
	/**
	 * Default constructor. Creates a file-based client without loading a file. 	
	 */
	public FileBasedKnowledgeBaseClient() {
		init();
	}
	
	/**
	 * Constructor loads the file.
	 * @param filePath
	 */
	public FileBasedKnowledgeBaseClient(String filePath) {
		this.filePath = filePath;
		init();
		load();
	}

	/**
	 * Constructor loads the file and set the sparql query/update.
	 * @param filePath
	 * @param sparql query/update
	 */
	public FileBasedKnowledgeBaseClient(String filePath, String query) {
		this.query = query; 
		this.filePath = filePath;
		init();
		load();
	}
	
	/**
	 * Initialise RDFConnection. 
	 */
	private void init() {
		dataset = DatasetFactory.create();
		conn = RDFConnectionFactory.connect(dataset);
	}
	
	///////////////////////////
	// Read and write methods
	///////////////////////////
	
	/**
	 * Set filePath variable and load file to memory.
	 * @param filePath
	 */
	public void load(String filePath) {
		this.filePath = filePath;
		load();
	}
	
	/**
	 * Load the file to memory.
	 */
	@Override
	public void load() {
		
		//If not connected then initialise a connection 
		if (!isConnected()) {
			init();
		}
		
		if (filePath == null) {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: no file path specified.");
		}
			
		//Load the file
		File f= new File(filePath);
		if ( f.exists() ) {
			
			//Set file output language to input language
			langOut = RDFLanguages.filenameToLang(filePath);
			System.out.println("File language is: " + langOut);
			
			conn.load(filePath);
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: cannot load " + filePath + ". File does not exist.");
		}
	}
	
	/**
	 * Writes the model to file and closes the connection.
	 */
	@Override
	public void end() {
		
		try {
			writeToFile();
			conn.close();
			dataset.close();
		}catch(IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Write the model back to file.
	 * @throws IOException
	 */
	public void writeToFile() throws IOException {
		
		writeToFile(this.filePath, this.langOut);
	}
	
	/**
	 * Write the default graph to file. File path and serialization language specified.
	 * @param filePath
	 * @param langOut 
	 * @throws IOException
	 */
	public void writeToFile(String filePath, Lang langOut) throws IOException {
		
		try (OutputStream out = new FileOutputStream(filePath)){
			
			RDFDataMgr.write(out, dataset.getDefaultModel(), langOut);
			
			out.flush();
		}
	}
	
	///////////////////////////
	// Variable access methods
	///////////////////////////
	
	/**
	 * Set filePath variable
	 * @param filePath
	 */
	public void setFilePath(String filePath) {
		this.filePath = filePath;
	}
	
	/**
	 * Get filePath variable
	 */
	public String getFilePath() {
		return filePath;
	}
	
	/**
	 * Set output serialization language
	 * @param langOut
	 */
	public void setOutputLang(Lang langOut) {
		this.langOut = langOut;
	}
	
	/**
	 * Sets a query if provided.
	 * 
	 * @param query
	 * @return
	 */
	@Override
	public String setQuery(String query) {
		this.query = query;
		return this.query;
	}
	
	/**
	 * Returns the available query.
	 * 
	 * @return
	 */
	@Override
	public String getQuery() {
		return query;
	}	
	
	/**
	 * Sets filePath. Query and and update file paths are the same.
	 * @param updateEndpoint
	 */
	@Override 
	public String setUpdateEndpoint(String updateEndpoint) {
		this.filePath = updateEndpoint;
		return filePath;
	}
	
	/**
	 * Return the file path. Query and update file paths are the same.
	 */
	@Override
	public String getUpdateEndpoint() {
		return filePath;
	}
	
	/**
	 * Sets filePath. Query and and update file paths are the same.
	 * @param queryEndpoint
	 */
	@Override 
	public String setQueryEndpoint(String queryEndpoint) {
		this.filePath = queryEndpoint;
		return filePath;
	}
	
	/**
	 * Return the file path. Query and update file paths are the same.
	 */
	@Override
	public String getQueryEndpoint() {
		return filePath;
	}
	
	//// 
	
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
	 * through constructor or setter methods
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate() {
		return executeUpdate(this.query);
	}
	
	/**
	 * Executes the update operation supplied by the calling method and returns results.
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate(String update) {

		if( conn != null) {
			conn.begin( TxnType.WRITE );
			try {
				conn.update(update);
				conn.commit();
			} finally {
				conn.end();
			}
			return 0; //return a useful integer?
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
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
		
		if( conn != null) {
			conn.begin( TxnType.WRITE );
			try {
				conn.update(update);
				conn.commit();
			}finally {
				conn.end();
			}
			return 0; //return a useful integer?
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}
	
	/**
	 * Execute sparql query using the query variable
	 * 
	 * @return JSONArray as String 
	 * @throws SQLException
	 */
	@Override
	public String execute(){
		return execute(this.query);
	}
	
	/**
	 * Excute sparql query
	 * 
	 * @param sparql
	 * @return JSONArray as String
	 * @throws SQLException
	 */
	@Override
	public String execute(String query){
		JSONArray result = executeQuery(query);
		if(result==null){
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: sparql query result is null.");
		}else{
			return result.toString();
		}
	}
	
	/**
	 * Executes the query supplied by the calling method and returns results<p>
	 * as a JSONArray.
	 */
	@Override
	public JSONArray executeQuery(String sparql) {
		ResultSet results = perfromExecuteQuery(sparql);
		return convert(results);
	}	
	
	/**
	 * Executes the query that is provided through the constructors or setter<p>
	 * method.
	 */
	@Override
	public JSONArray executeQuery() {
		return executeQuery(this.query);
	}
	
	/**
	 * Performs sparql query execution.
	 * @param sparql
	 * @return
	 */
	private ResultSet perfromExecuteQuery(String sparql) {
		
		if (conn != null) {
			conn.begin( TxnType.READ );	
			try {
				QueryExecution queryExec = conn.query(sparql);
				ResultSet results = queryExec.execSelect();
				return results;
			} finally {
				conn.end();
			}
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: client not initialised.");
		}
	}
	
	/**
	 * Convert query results to JSONArray.
	 * @param resultSet
	 * @return
	 */
	private JSONArray convert(ResultSet resultSet) {
		
		JSONArray json = new JSONArray();
		
		while (resultSet.hasNext()) {
			QuerySolution qs = resultSet.next();
			JSONObject obj = new JSONObject();
			Iterator<String> it = qs.varNames(); 
			while(it.hasNext()) {
				String var = it.next(); 
				obj.put(var, qs.get(var));
			}
			json.put(obj);
		}
		return json;
	}
}
