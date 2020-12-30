package uk.ac.cam.cares.jps.base.query;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
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
 * The file must first be loaded using the constructor or load methods.
 * If sparql update operations have been performed, the data must be written
 * to file using the writeToFile or end methods. 
 * 
 * @author Casper Lindberg
 *
 */
public class FileBasedKnowledgeBaseClient extends KnowledgeBaseClient {

	private Dataset dataset;
	private RDFConnection conn;
	
	private String query;
	//Default graph
	private Lang defaultLangOut = Lang.RDFXML; // RDFXML by default
	private String defaultFilePath;
	//Named graphs
	private ArrayList<String> graphs = new ArrayList<String>();
	private ArrayList<String> graphFilePaths = new ArrayList<String>();
	private ArrayList<Lang> graphLangs = new ArrayList<Lang>(); 
		
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
	 * Constructor loads a file to the default graph.
	 * @param filePath
	 */
	public FileBasedKnowledgeBaseClient(String filePath) {
		this.defaultFilePath = filePath;
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
		this.defaultFilePath = filePath;
		init();
		load();
	}
	
	/**
	 * Constructor loads multiple file/contexts.
	 * @param grpahs
	 * @param filePaths
	 */
	public FileBasedKnowledgeBaseClient(String[] graphs, String[] filePaths) {
		
		//graphs and filepaths should be the same length
		if(graphs.length == filePaths.length) {
			
			this.graphs.addAll(Arrays.asList(graphs));
			this.graphFilePaths.addAll(Arrays.asList(filePaths));
			
		}else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: file path or graph name missing (graphs.length != flePaths.length).");
		}
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
	 * Load the file to memory.
	 */
	@Override
	public void load() {
		
		//No file path set
		if(defaultFilePath == null && graphs.size() == 0) {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: no file path specified.");
		}
				
		//Load multiple files if provided
		if( graphs.size() > 0 ) {
			
			for(int i=0; i<graphs.size(); i++) {
				loadGraph(graphs.get(i), graphFilePaths.get(i));
			}
		}
		
		//Load single file to default graph		
		if(defaultFilePath != null){
			loadGraph(null, defaultFilePath);
		}
	}

	/**
	 * Set filePath variable and load file to default graph.
	 * @param filePath
	 */
	public void load(String filePath) {

		//Load file
		load(null, filePath);
	}
	
	/**
	 * Load a named graph
	 * @param graph
	 * @param filePath
	 */
	public void load(String graph, String filePath) {
	
		if(graph == null) {
			//Set default file path
			this.defaultFilePath = filePath;	
		}else {
			//Add graph and filePath to array
			graphs.add(graph);
			graphFilePaths.add(filePath);
		}
		
		loadGraph(graph, filePath);
	}
	
	/**
	 * Loads a file to the dataset
	 * @param graph
	 * @param filePath
	 */
	private void loadGraph(String graph, String filePath) {
	
		//If not connected then initialise a connection 
		if (!isConnected()) {
			init();
		}
			
		//Check that the file exists
		File f= new File(filePath);
		if ( f.exists() ) {			
			
			//Get serialisation language
			Lang lang = RDFLanguages.filenameToLang(filePath);
			System.out.println("FileBasedKnowledgeBaseClient: File language is: " + lang);
			
			//Set file output language to input language
			if(graph == null) {
				defaultLangOut = lang;
			}else {
				graphLangs.add(lang);	
			}
			
			//Data is triples
			if(RDFLanguages.isTriples(lang)) {
	
				if(!dataset.containsNamedModel(graph)) { //TODO test for null
					//Load the triples to graph in dataset
					conn.load(graph, filePath);
				}else {
					throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: " + graph + " already exists!");
				}
			//Data is quads
			}else {
				
				//Load quads to separate dataset first 
				Dataset tempDataset = RDFDataMgr.loadDataset(filePath);
				
				//TODO: check how default graph is treated
				
				Iterator<String> it = tempDataset.listNames();
				
				int contextCount = 0;
				while(it.hasNext()) {
					
					contextCount ++;
					
					//error: multiple contexts in file
					if(contextCount > 1) {
						throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: multiple contexts in file not supported!");
					}
					
					String context = it.next();
					
					//context does not match supplied graph name					
					if(!tempDataset.containsNamedModel(graph)) { //TODO test for null
						//change graph name
						graphs.set(graphs.indexOf(graph), context);
						System.out.println("FileBasedKnowledgeBaseClient: graph name " + graph + " changed to " + context);
					}
						
					//error: context already exists in dataset
					if(dataset.containsNamedModel(context)) { //TODO test for null
						throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: " + context + " already exists!");
					}
				}		
			
				//add the data to the connection dataset
				conn.putDataset(tempDataset);
				
				//clear and close temporary resource
				tempDataset.asDatasetGraph().clear(); 
				tempDataset.close();
			}
		} else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: cannot load " + filePath + ". File does not exist.");
		}
	}	
	
	/*
	private void checkGraphs(String graph) {
		
		//Check if graph name has changed
		//If loading quads, the graph name will be the context
		Iterator<String> graphNames = dataset.listNames();
		while (graphNames.hasNext()) {
			
			//TODO
			//issue if NQuads loaded as default
			
			String graphName = graphNames.next();
		    //if a graph is not contained, replace the name of the newly added graph
		    if(!graphs.contains(graphName)){
		    	
		    	if(graph == null){
		    		//default graph changed to named graph
		    		graphs.add(graphName);
		    		graphFilePaths.add(defaultFilePath);
		    		defaultFilePath = null;
		    		graphLangs.add(defaultLangOut);
		    	}else if(graphs.contains(graph)){
		    		graphs.set(graphs.indexOf(graph), graphName);
		    		System.out.println("FileBasedKnowledgeBaseClient: graph name " + graph + " changed to " + graphName);
		    	}else {
		    		//already replaced
		    		throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: Multiple contexts in file. Not currently supported.");		
		    	}
		    }
		}
	}
	*/
	
	/**
	 * Writes the model to file and closes the connection.
	 */
	@Override
	public void end() {
		
		writeToFile();
		conn.close();
		dataset.close();
	}
	
	/**
	 * Write the model(s) back to file.
	 * @throws IOException
	 */
	public void writeToFile(){
		
		if( graphFilePaths.size() > 0 ) { //named graphs
			for(int i = 0; i < graphFilePaths.size(); i++) {
				writeToFile(graphs.get(i), graphFilePaths.get(i), graphLangs.get(i));
			}
		}else {	//default graph
			writeToFile(this.defaultFilePath, this.defaultLangOut);
		}
	}
	
	/**
	 * Write the default graph to file. File path and serialization language specified.
	 * @param filePath
	 * @param langOut 
	 * @throws IOException
	 */
	public void writeToFile(String filePath, Lang langOut) {
		
		try (OutputStream out = new FileOutputStream(filePath)){
			
			RDFDataMgr.write(out, dataset.getDefaultModel(), langOut);
			
			out.flush();
		}catch(IOException e) {
			throw new JPSRuntimeException(e);
		}
	}
	
	/**
	 * Write a named graph to file. Graph URI, file path and serialization language specified.
	 * @param graph uri
	 * @param filePath
	 * @param langOut 
	 * @throws IOException
	 */
	public void writeToFile(String graph, String filePath, Lang langOut) {
		
		if (dataset.containsNamedModel(graph)) {
			try (OutputStream out = new FileOutputStream(filePath)){
				
				RDFDataMgr.write(out, dataset.getNamedModel(graph), langOut);
				
				out.flush();
			}catch(IOException e) {
				throw new JPSRuntimeException(e);
			}
		}else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: " + graph + " does not exist.");
		}
	}
	
	///////////////////////////
	// Variable access methods
	///////////////////////////
	
	/**
	 * Set default graph file path variable
	 * @param filePath
	 */
	public void setFilePath(String filePath) {
		this.defaultFilePath = filePath;
	}
	
	/**
	 * Get default graph file path variable
	 */
	public String getFilePath() {
		return defaultFilePath;
	}
	
	/**
	 * Set output serialization language for default grpah
	 * @param langOut
	 */
	public void setOutputLang(Lang langOut) {
		this.defaultLangOut = langOut;
	}
	
	/**
	 * Set output serialization language for named graphs
	 * @param langOut
	 */
	public void setOutputLang(Lang[] langOut) {
		
		//replace contents of langsOut
		if(langOut.length == graphLangs.size() ) {
			for(int i = 0; i < graphLangs.size(); i++) {
				graphLangs.set(i, langOut[i]);
			}
		}else {
			throw new JPSRuntimeException("FileBasedKnowledgeBaseClient: length of array does equal number of named graphs");
		}
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
	 * Sets the default graph file path. Query and and update file paths are the same.
	 * @param updateEndpoint
	 */
	@Override 
	public String setUpdateEndpoint(String updateEndpoint) {
		this.defaultFilePath = updateEndpoint;
		return defaultFilePath;
	}
	
	/**
	 * Return the default graph file path. Query and update file paths are the same.
	 */
	@Override
	public String getUpdateEndpoint() {
		return defaultFilePath;
	}
	
	/**
	 * Sets the default graph file path. Query and and update file paths are the same.
	 * @param queryEndpoint
	 */
	@Override 
	public String setQueryEndpoint(String queryEndpoint) {
		this.defaultFilePath = queryEndpoint;
		return defaultFilePath;
	}
	
	/**
	 * Return the default graph file path. Query and update file paths are the same.
	 */
	@Override
	public String getQueryEndpoint() {
		return defaultFilePath;
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
	 * through the constructor or setter methods.
	 * writeToFile() or end() must be called to save changes to file.
	 * 
	 * @param update as String
	 * @return
	 */
	@Override
	public int executeUpdate() {
		return executeUpdate(this.query);
	}
	
	/**
	 * Executes the update operation supplied by the calling method.
	 * writeToFile() or end() must be called to save changes to file.
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